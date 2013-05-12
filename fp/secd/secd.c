#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <stdarg.h>
#include <limits.h>
#include <ctype.h>

#define bool    int
#define TRUE    1
#define FALSE   0

#define assert_or_continue(cond, ...) \
    if (!(cond)) { fprintf(stderr, __VA_ARGS__); fprintf(stderr, "\n"); continue; }
#define assert(cond, ...) \
    if (!(cond)) { fprintf(stderr, __VA_ARGS__); fprintf(stderr, "\n"); return NIL_CELL; }
#define asserti(cond, ...) \
    if (!(cond)) { fprintf(stderr, __VA_ARGS__); fprintf(stderr, "\n"); return 0; }
#define assertv(cond, ...) \
    if (!(cond)) { fprintf(stderr, __VA_ARGS__); fprintf(stderr, "\n"); return; }

#if (0)
# define memdebugf(...) printf(__VA_ARGS__)
# if (0)
#  define memtracef(...) printf(__VA_ARGS__)
# else
#  define memtracef(...)
# endif
#else
# define memdebugf(...)
# define memtracef(...)
#endif

#if (1)
# define ctrldebugf(...) printf(__VA_ARGS__)
#else
# define ctrldebugf(...)
#endif

#define NIL_CELL    ((cell_t *)0)

#define DONT_FREE_THIS  INTPTR_MAX

#define N_CELLS     256 * 1024
#define SECD_ALIGN  4

typedef  struct secd  secd_t;
typedef  struct cell  cell_t;

typedef  enum operation  opcode_t;

typedef  struct atom  atom_t;
typedef  struct cons  cons_t;
typedef  struct error error_t;

typedef cell_t* (*secd_opfunc_t)(secd_t *);

enum cell_type {
    CELL_UNDEF,
    CELL_ATOM,
    CELL_CONS,
    CELL_ERROR,
};

enum atom_type {
    NOT_AN_ATOM,
    ATOM_INT,
    ATOM_SYM,
    ATOM_FUNC,
};

struct atom {
    enum atom_type type;
    union {
        int as_int;
        struct {
            size_t size;
            const char *data;
        } as_sym;

        void *as_ptr;
    };
};

struct cons {
    cell_t *car;    // shares
    cell_t *cdr;    // shares
};

struct error {
    size_t len;
    const char *msg; // owns
};

struct cell {
    // this is a packed structure:
    //      bits 0 .. SECD_ALIGN-1          - enum cell_type
    //      bits SECD_ALIGN .. CHAR_BIT * (sizeof(intptr_t)-1)   - (secd_t *)
    intptr_t type;
    union {
        atom_t  as_atom;
        cons_t  as_cons;
        error_t as_err;
    };

    size_t nref;
};

// must be aligned at 1<<SECD_ALIGN
struct secd  {
    cell_t *stack;
    cell_t *env;
    cell_t *control;
    cell_t *dump;

    cell_t *free;
    cell_t *data;
};


void print_cell(const cell_t *c);
cell_t *free_cell(cell_t *c);

cell_t *lookup_env(secd_t *secd, const char *symname);

/*
 *  Cell accessors
 */

inline static enum cell_type cell_type(const cell_t *c) {
    return ((1 << SECD_ALIGN) - 1) & c->type;
}

inline static secd_t *cell_secd(const cell_t *c) {
    return (secd_t *)((INTPTR_MAX << SECD_ALIGN) & c->type);
}

inline static enum atom_type atom_type(const cell_t *c) {
    if (cell_type(c) != CELL_ATOM) return NOT_AN_ATOM;
    return (enum atom_type)(c->as_atom.type);
}

inline static off_t cell_index(const cell_t *cons) {
    if (cons == NIL_CELL) return -1;
    return cons - cell_secd(cons)->data;
}

inline static cell_t *list_next(const cell_t *cons) {
    if (cell_type(cons) != CELL_CONS) {
        fprintf(stderr, "list_next: not a cons at [%ld]\n", cell_index(cons));
        print_cell(cons);
        return NIL_CELL;
    }
    return cons->as_cons.cdr;
}

inline static cell_t *list_head(const cell_t *cons) {
    return cons->as_cons.car;
}

inline static cell_t *get_car(const cell_t *cons) {
    return cons->as_cons.car;
}
inline static cell_t *get_cdr(const cell_t *cons) {
    return cons->as_cons.cdr;
}
inline static bool is_cons(const cell_t *cell) {
    return cell_type(cell) == CELL_CONS;
}

void print_atom(const cell_t *c) {
    switch (atom_type(c)) {
      case ATOM_INT: printf("INT(%d)", c->as_atom.as_int); break;
      case ATOM_SYM: printf("SYM(%s)", c->as_atom.as_sym.data); break;
      case ATOM_FUNC: printf("BUILTIN(%p)", c->as_atom.as_ptr); break;
      case NOT_AN_ATOM: printf("ERROR(not an atom)");
    }
}

void print_cell(const cell_t *c) {
    if (!c) { printf("NIL\n"); return; }
    secd_t *secd = cell_secd(c);
    printf("[%ld]^%ld: ", cell_index(c), c->nref);
    switch (cell_type(c)) {
      case CELL_CONS:
        printf("CONS([%ld], [%ld])\n", cell_index(get_car(c)), cell_index(get_cdr(c)));
        break;
      case CELL_ATOM:
        print_atom(c); printf("\n");
        break;
      default:
        printf("unknown type: %d\n", cell_type(c));
    }
}

void print_list(cell_t *list) {
    printf("  -= ");
    while (list) {
        assertv(is_cons(list),
                "Not a cons at [%ld]\n", cell_index(list));
        printf("[%ld]:%ld\t", cell_index(list), cell_index(get_car(list)));
        print_cell(get_car(list));
        printf("  -> ");
        list = list_next(list);
    }
    printf("NIL\n");
}

/*
 * Reference-counting
 */

inline static cell_t *share_cell(cell_t *c) {
    if (c) {
        ++c->nref;
        memtracef("share[%ld] %ld\n", cell_index(c), c->nref);
    } else memdebugf("share[NIL]\n");
    return c;
}

inline static cell_t *drop_cell(cell_t *c) {
    if (!c) {
        memdebugf("drop [NIL]\n");
        return NULL;
    }
    assert(c->nref > 0, "drop_cell[%ld]: negative", cell_index(c));

    -- c->nref;
    memtracef("drop [%ld] %ld\n", cell_index(c), c->nref);
    if (c->nref) return c;
    return free_cell(c);
}

/*
 *  Cell memory management
 */

cell_t *pop_free(secd_t *secd) {
    cell_t *cell = secd->free;
    assert(cell != NIL_CELL, "pop_free: no free memory");

    secd->free = list_next(cell);
    memdebugf("NEW [%ld]\n", cell_index(cell));

    cell->type = (intptr_t)secd;
    return cell;
}

void push_free(cell_t *c) {
    assertv(c, "push_free: NIL_CELL");
    assertv(c->nref == 0, "push_free: [%ld]->nref is %ld\n", cell_index(c), c->nref);
    secd_t *secd = cell_secd(c);
    c->type = (intptr_t)secd | CELL_CONS;
    c->as_cons.cdr = secd->free;
    secd->free = c;
    memdebugf("FREE[%ld]\n", cell_index(c));
}

cell_t *new_cons(secd_t *secd, cell_t *car, cell_t *cdr) {
    cell_t *cell = pop_free(secd);
    cell->type |= CELL_CONS;
    cell->as_cons.car = share_cell(car);
    cell->as_cons.cdr = share_cell(cdr);
    return cell;
}

cell_t *new_number(secd_t *secd, int num) {
    cell_t *cell = pop_free(secd);
    cell->type |= CELL_ATOM;
    cell->as_atom.type = ATOM_INT;
    cell->as_atom.as_int = num;
    return cell;
}

cell_t *new_symbol(secd_t *secd, const char *sym) {
    cell_t *cell = pop_free(secd);
    cell->type |= CELL_ATOM;
    cell->as_atom.type = ATOM_SYM;
    cell->as_atom.as_sym.size = strlen(sym);
    cell->as_atom.as_sym.data = strdup(sym);
    return cell;
}

cell_t *new_clone(secd_t *secd, const cell_t *from) {
    if (!from) return NIL_CELL;
    cell_t *clone = pop_free(secd);
    memcpy(clone, from, sizeof(cell_t));
    clone->type = (intptr_t)secd | cell_type(from);
    clone->nref = 0;
    return clone;
}

cell_t *new_error(secd_t *secd, const char *fmt, ...) {
    va_list va;
    va_start(va, fmt);
#define MAX_ERROR_SIZE  512
    char buf[MAX_ERROR_SIZE];
    vsnprintf(buf, MAX_ERROR_SIZE, fmt, va);
    va_end(va);

    cell_t *err = pop_free(secd);
    err->type |= CELL_ERROR;
    err->as_err.len = strlen(buf);
    err->as_err.msg = strdup(buf);
    return err;
}

void free_atom(cell_t *cell) {
    switch (cell->as_atom.type) {
      case ATOM_SYM:
        if (cell->as_atom.as_sym.size != DONT_FREE_THIS)
            free((char *)cell->as_atom.as_sym.data); break;
      default: return;
    }
}

cell_t *free_cell(cell_t *c) {
    enum cell_type t = cell_type(c);
    switch (t) {
      case CELL_ATOM:
        free_atom(c);
        break;
      case CELL_CONS:
        drop_cell(get_car(c));
        drop_cell(get_cdr(c));
        break;
      case CELL_ERROR:
        return c;
      default:
        return new_error(cell_secd(c), "free_cell: unknown cell_type 0x%x", t);
    }
    push_free(c);
    return NIL_CELL;
}

cell_t *push_stack(secd_t *secd, cell_t *newc) {
    cell_t *top = new_cons(secd, newc, secd->stack);
    drop_cell(secd->stack);
    secd->stack = share_cell(top);
    memdebugf("PUSH S[%ld (%ld, %ld)]\n", cell_index(top),
                        cell_index(get_car(top)), cell_index(get_cdr(top)));
    return top;
}

cell_t *pop_stack(secd_t *secd) {
    assert(secd->stack, "pop_stack: empty");
    cell_t *cell = secd->stack;
    assert(is_cons(cell), "pop_stack: not a cons");
    secd->stack = list_next(secd->stack);
    memdebugf("POP S[%ld]\n", cell_index(cell));
    return cell; // don't forget to drop_call(result)
}

cell_t *push_control(secd_t *secd, cell_t *opcons) {
    assert(is_cons(opcons),
           "push_control: failed, not a cons at [%ld]\n", cell_index(opcons));
    return (secd->control = share_cell(opcons));
}

cell_t *pop_control(secd_t *secd) {
    cell_t *cons = secd->control;
    assert(cons, "pop_control: NIL");
    assert(is_cons(cons), "pop_control: not a cons at [%ld]\n", cell_index(cons));

    cell_t *op = share_cell(get_car(cons));
    secd->control = share_cell(list_next(cons));
    drop_cell(cons);
    return op;
}

/*
 *  SECD built-ins
 */

cell_t *secd_cons(secd_t *secd) {
    ctrldebugf("CONS\n");
    cell_t *a = pop_stack(secd);
    assert(a, "secd_cons: pop_stack(a) failed");
    assert(is_cons(a), "secd_cons: S[top] not a cons");
    cell_t *b = pop_stack(secd);
    assert(b, "secd_cons: pop_stack(b) failed");
    assert(is_cons(a), "secd_cons: S[top-1] is not a cons");

    cell_t *cons = new_cons(secd, get_car(a), get_car(b));
    drop_cell(a);

    push_stack(secd, cons);
    return cons;
}

cell_t *secd_car(secd_t *secd) {
    ctrldebugf("CAR\n");
    cell_t *top = pop_stack(secd);
    assert(top, "secd_car: pop_stack() failed");
    assert(is_cons(top), "secd_car: cons expected");

    cell_t *car = push_stack(secd, get_car(get_car(top)));
    drop_cell(top);
    return car;
}

cell_t *secd_cdr(secd_t *secd) {
    ctrldebugf("CDR\n");
    cell_t *top = pop_stack(secd);
    assert(top, "secd_cdr: pop_stack() failed");
    assert(is_cons(top), "secd_cdr: cons expected");

    cell_t *cdr = push_stack(secd, get_cdr(get_car(top)));
    drop_cell(top);
    return cdr;
}

cell_t *secd_ldc(secd_t *secd) {
    ctrldebugf("LDC\n");
    cell_t *arg = pop_control(secd);
    assert(cell_type(arg) == CELL_ATOM,
           "secd_ldc: arg at [%ld] is not an atom", cell_index(arg));
    return push_stack(secd, arg);
}

cell_t *secd_ld(secd_t *secd) {
    ctrldebugf("LD\n");
    cell_t *arg = pop_control(secd);
    assert(atom_type(arg) == ATOM_SYM,
           "secd_ld: not a symbol [%ld]", cell_index(arg));

    cell_t *val = lookup_env(secd, arg->as_atom.as_sym.data);
    drop_cell(arg);
    return push_stack(secd, val);
}

static inline cell_t *to_bool(secd_t *secd, bool cond) {
    return ((cond)? lookup_env(secd, "T") : NIL_CELL);
}

bool atom_eq(const cell_t *a1, const cell_t *a2) {
    enum atom_type atype1 = atom_type(a1);
    if (a1 == a2)
        return TRUE;
    if (atype1 != atom_type(a2))
        return FALSE;
    switch (atype1) {
      case ATOM_INT: return (a1->as_atom.as_int == a2->as_atom.as_int);
      case ATOM_SYM: return (!strcmp(a1->as_atom.as_sym.data, a2->as_atom.as_sym.data));
      case ATOM_FUNC: return (a1->as_atom.as_ptr == a2->as_atom.as_ptr);
      default: fprintf(stderr, "atom_eq([%ld], [%ld]): don't know how to handle type %d\n",
                       cell_index(a1), cell_index(a2), atype1);
    }
    return FALSE;
}

bool list_eq(const cell_t *xs, const cell_t *ys) {
    asserti(is_cons(xs), "list_eq: [%ld] is not a cons", cell_index(xs));
    if (xs == ys)
        return TRUE;
    if (!is_cons(ys))
        return FALSE;
    while (xs) {
        if (!ys) return FALSE;
        const cell_t *x = get_car(xs);
        const cell_t *y = get_car(ys);
        if (is_cons(x)) {
            if (!list_eq(x, y))
                return FALSE;
        } else if (!atom_eq(x, y))
            return FALSE;

        xs = list_next(xs);
        ys = list_next(ys);
    }
    return !ys;
}

cell_t *secd_atom(secd_t *secd) {
    ctrldebugf("ATOM\n");
    cell_t *top = pop_stack(secd);
    assert(top, "secd_atom: pop_stack() failed");
    cell_t *val = get_car(top);
    cell_t *result = to_bool(secd, (val ? !is_cons(val) : TRUE));
    cell_t *newtop = push_stack(secd, result);
    drop_cell(top);
    return newtop;
}

cell_t *secd_eq(secd_t *secd) {
    ctrldebugf("EQ\n");
    cell_t *sa = pop_stack(secd);
    cell_t *a = get_car(sa);

    cell_t *sb = pop_stack(secd);
    assert(sb, "secd_eq: pop_stack(b) failed");
    cell_t *b = get_car(sb);

    cell_t *val = is_cons(a) ?
                  to_bool(secd, list_eq(a, b)) :
                  to_bool(secd, atom_eq(a, b)) ;
    drop_cell(sa);
    return push_stack(secd, val);
}

cell_t *secd_add(secd_t *secd) {
    ctrldebugf("ADD\n");
    cell_t *sa = pop_stack(secd);
    cell_t *a = get_car(sa);
    assert(atom_type(a) == ATOM_INT, "secd_add: a is not int");

    cell_t *sb = pop_stack(secd);
    assert(sb, "secd_add: pop_stack(b) failed");
    cell_t *b = get_car(sb);
    assert(atom_type(b) == ATOM_INT, "secd_add: b is not int");

    int num = a->as_atom.as_int + b->as_atom.as_int;
    drop_cell(sa);

    return push_stack(secd, new_number(secd, num));
}


#define INIT_SYM(name) {    \
    .type = CELL_ATOM,      \
    .as_atom = {            \
        .type = ATOM_SYM,   \
        .as_sym = { .size = DONT_FREE_THIS, \
                    .data = (name) } }, \
    .nref = INTPTR_MAX }

#define INIT_NUM(num) {     \
    .type = CELL_ATOM,      \
    .as_atom = { .type = ATOM_INT,  \
                 .as_int = (num) }, \
    .nref = INTPTR_MAX }

#define INIT_FUNC(func) {   \
    .type = CELL_ATOM,      \
    .as_atom = { .type = ATOM_FUNC,  \
                 .as_ptr = (void *)(func) }, \
    .nref = INTPTR_MAX }

const cell_t cons_func  = INIT_FUNC(secd_cons);
const cell_t car_func   = INIT_FUNC(secd_car);
const cell_t cdr_func   = INIT_FUNC(secd_cdr);
const cell_t add_func   = INIT_FUNC(secd_add);
const cell_t ldc_func   = INIT_FUNC(secd_ldc);
const cell_t ld_func    = INIT_FUNC(secd_ld);
const cell_t atom_func  = INIT_FUNC(secd_atom);

const cell_t cons_sym   = INIT_SYM("CONS");
const cell_t car_sym    = INIT_SYM("CAR");
const cell_t cdr_sym    = INIT_SYM("CDR");
const cell_t add_sym    = INIT_SYM("ADD");
const cell_t ldc_sym    = INIT_SYM("LDC");
const cell_t ld_sym     = INIT_SYM("LD");
const cell_t atom_sym   = INIT_SYM("ATOM");

const cell_t two_num    = INIT_NUM(2);
const cell_t t_sym      = INIT_SYM("T");
const cell_t nil_sym    = INIT_SYM("NIL");

const struct {
    const cell_t *sym;
    const cell_t *val;
} global_binding[] = {
    { &cons_sym,    &cons_func },
    { &car_sym,     &car_func },
    { &cdr_sym,     &cdr_func },
    { &add_sym,     &add_func },
    { &ldc_sym,     &ldc_func },
    { &ld_sym,      &ld_func  },
    { &atom_sym,    &atom_func },
    { &t_sym,       &t_sym    },
    { &nil_sym,     NIL_CELL  },
    { NULL,         NIL_CELL  } // must be last
};

void fill_global_env(secd_t *secd) {
    int i;
    cell_t *symlist = NIL_CELL;
    cell_t *vallist = NIL_CELL;

    for (i = 0; global_binding[i].sym; ++i) {
        cell_t *sym = new_clone(secd, global_binding[i].sym);
        cell_t *val = new_clone(secd, global_binding[i].val);
        symlist = new_cons(secd, sym, symlist);
        vallist = new_cons(secd, val, vallist);
    }

    cell_t *frame = new_cons(secd, symlist, vallist);
    cell_t *env = new_cons(secd, frame, NIL_CELL);

    secd->env = share_cell(env);
}

cell_t *lookup_env(secd_t *secd, const char *symname) {
    cell_t *env = secd->env;
    assert(cell_type(env) == CELL_CONS, "lookup_env: environment is not a list\n");

    while (env) {       // walk through frames
        cell_t *frame = get_car(env);
        cell_t *symlist = get_car(frame);
        cell_t *vallist = get_cdr(frame);

        while (symlist) {   // walk through symbols
            cell_t *symbol = get_car(symlist);
            if (atom_type(symbol) != ATOM_SYM) {
                fprintf(stderr, "lookup_env: variable at [%ld] is not a symbol\n",
                        cell_index(symbol));
                symlist = list_next(symlist); vallist = list_next(vallist);
                continue;
            }

            if (!strcmp(symname, symbol->as_atom.as_sym.data)) {
                return get_car(vallist);
            }
            symlist = list_next(symlist);
            vallist = list_next(vallist);
        }

        env = list_next(env);
    }
    printf("lookup_env: %s not found\n", symname);
}

cell_t *lookup_symbol(secd_t *secd, const char *symname) {
    cell_t *env = secd->env;
    assert(cell_type(env) == CELL_CONS, "lookup_symbol: environment is not a list\n");

    while (env) {       // walk through frames
        cell_t *frame = get_car(env);
        cell_t *symlist = get_car(frame);

        while (symlist) {   // walk through symbols
            cell_t *symbol = get_car(symlist);
            assert(atom_type(symbol) != ATOM_SYM,
                    "lookup_symbol: variable at [%ld] is not a symbol\n", cell_index(symbol));

            if (!strcmp(symname, symbol->as_atom.as_sym.data)) {
                return symbol;
            }
            symlist = list_next(symlist);
        }

        env = list_next(env);
    }
}

void print_env(secd_t *secd) {
    cell_t *env = secd->env;
    int i = 0;
    printf("Environment:\n");
    while (env) {
        printf(" Frame #%d:\n", i);
        cell_t *frame = get_car(env);
        cell_t *symlist = get_car(frame);
        cell_t *vallist = get_cdr(frame);

        while (symlist) {
            cell_t *sym = get_car(symlist);
            cell_t *val = get_car(vallist);
            if (atom_type(sym) != ATOM_SYM)
                fprintf(stderr, "print_env: not a symbol at *%p in vallist\n", sym);
            printf("  %s\t=>\t", sym->as_atom.as_sym.data, cell_index(val));
            print_cell(val);

            symlist = list_next(symlist);
            vallist = list_next(vallist);
        }

        env = list_next(env);
    }
}

void fill_control_path(secd_t *secd, cell_t *ops[]) {
    int i = 0;
    while (ops[++i]);  // go to the end

    cell_t *control = NIL_CELL;
    while (i-- > 0) {
        cell_t *op = new_clone(secd, ops[i]);
        control = new_cons(secd, op, control);
    }

    secd->control = share_cell(control);
}

/*
 *  SECD parser
 *  A parser of a simple Lisp subset
 */
#define MAX_LEXEME_SIZE     256

typedef  int  token_t;
typedef  struct secd_parser secd_parser_t;

enum {
    TOK_EOF = -1,
    TOK_STR = -2,
    TOK_NUM = -3,
    TOK_ERR = -4,
};

struct secd_parser {
    token_t token;
    FILE *f;

    /* lexer guts */
    int lc;
    int numtok;
    char strtok[MAX_LEXEME_SIZE];
    char issymbc[UCHAR_MAX + 1];
};

secd_parser_t *init_parser(secd_parser_t *p, FILE *f) {
    p->lc = ' ';
    p->f = (f ? f : stdin);

    memset(p->issymbc, FALSE, 0x20);
    memset(p->issymbc + 0x20, TRUE, UCHAR_MAX - 0x20);
    char *s = " ();\n";
    while (*s)
        p->issymbc[(unsigned char)*s++] = FALSE;
    return p;
}

secd_parser_t *new_parser(FILE *f) {
    secd_parser_t *p = (secd_parser_t *)calloc(1, sizeof(secd_parser_t));
    return init_parser(p, f);
}

inline static int nextchar(secd_parser_t *p) {
    return p->lc = fgetc(p->f);
}

token_t lexnext(secd_parser_t *p) {
    /* skip spaces */
    while (isspace(p->lc))
        nextchar(p);

    switch (p->lc) {
      case EOF:
        return (p->token = TOK_EOF);
      case ';':
        /* comment */
        do nextchar(p); while (p->lc != '\n');
        return lexnext(p);

      case '(': case ')': case '\'':
        /* one-char tokens */
        p->token = p->lc;
        nextchar(p);
        return p->token;
    }

    if (isdigit(p->lc)) {
        char *s = p->strtok;
        do {
            *s++ = p->lc; nextchar(p);
        } while (isdigit(p->lc));
        *s = '\0';

        p->numtok = atoi(p->strtok);
        return (p->token = TOK_NUM);
    }

    if (p->issymbc[(unsigned char)p->lc]) {
        char *s = p->strtok;
        do {
            *s++ = p->lc;
            nextchar(p);
        } while (p->issymbc[(unsigned char)p->lc]);
        *s = '\0';

        return (p->token = TOK_STR);
    }
    return TOK_ERR;
}

void test_lexer(FILE *f) {
    secd_parser_t p;
    init_parser(&p, NULL);
    int tok;
    while (TOK_EOF != (tok = lexnext(&p))) {
        switch (tok) {
          case TOK_STR: printf("lexsym(%s)\n", p.strtok); break;
          case TOK_NUM: printf("lexnum(%d)\n", p.numtok); break;
          case TOK_ERR: printf("lexerr.\n"); return;
          case '(': printf("lexLB\n"); break;
          case ')': printf("lexRB\n"); break;
          default: printf("lextok('%c')\n", tok);
        }
    }
    printf("lexbye.\n");
}

cell_t *read_secd(secd_t *secd, FILE *f) {
    secd_parser_t p;
    init_parser(&p, f);
    cell_t *head = NIL_CELL, *tail = NIL_CELL;
    cell_t *newtail, *val;

    while (TRUE) {
        int tok = lexnext(&p);
        switch (tok) {
          case TOK_STR:
              val = new_symbol(secd, p.strtok); break;
          case TOK_NUM:
              val = new_number(secd, p.numtok); break;
          case TOK_EOF: case ')':
              return head;
          case '(':
              val = read_secd(secd, f); break;
          case TOK_ERR:
              free_cell(head); return NIL_CELL;
        }

        newtail = new_cons(secd, val, NIL_CELL);
        if (head) {
            tail->as_cons.cdr = share_cell(newtail);
            tail = newtail;
        } else {
            head = tail = newtail;
        }
    }
}

/*
 * SECD machine
 */

secd_t * init_secd(secd_t *secd) {
    /* allocate memory chunk */
    secd->data = (cell_t *)calloc(N_CELLS, sizeof(cell_t));

    /* mark up a list of free cells */
    int i;
    for (i = 0; i < N_CELLS - 1; ++i) {
        cell_t *c = secd->data + i;
        c->type = (intptr_t)secd | CELL_CONS;
        c->as_cons.cdr = secd->data + i + 1;
    }
    cell_t * c = secd->data + N_CELLS - 1;
    c->type = (intptr_t)secd | CELL_CONS;
    c->as_cons.cdr = NIL_CELL;

    secd->free = secd->data;
    return secd;
}

void run_secd(secd_t *secd) {
    cell_t *op;
    while (NIL_CELL != (op = pop_control(secd))) {
        assert_or_continue(atom_type(op) == ATOM_SYM,
                "run: not a symbol at [%ld]\n", cell_index(op));

        const char *symname = op->as_atom.as_sym.data;
        cell_t *val = lookup_env(secd, symname);
        assert_or_continue(val, "run: lookup_env() failed for %s\n", symname);
        assert_or_continue(atom_type(val) == ATOM_FUNC, "run: not a ATOM_FUNC\n");
        drop_cell(op);

        secd_opfunc_t callee = (secd_opfunc_t) val->as_atom.as_ptr;
        callee(secd);

        printf("Stack:\n"); print_list(secd->stack);
    }
}

const cell_t *test2plus2[] = {
    &ldc_sym, &two_num,
    &ldc_sym, &two_num,
    &add_sym,
    NIL_CELL
};

const cell_t *test_car[] = {
    &ldc_sym, &two_num,
    &ldc_sym, &two_num,
    &ld_sym, &nil_sym,
    &cons_sym,
    &cons_sym,
    NIL_CELL
};

secd_t __attribute__((aligned(1 << SECD_ALIGN))) secd;

int main(int argc, char *argv[]) {
    init_secd(&secd);

    fill_global_env(&secd);
    print_env(&secd);

    cell_t *inp = read_secd(&secd, NULL);

    push_control(&secd, inp);
    //fill_control_path(&secd, );
    printf("Control path:\n");
    print_list(secd.control);

    printf("-----------------------------------\n");
    run_secd(&secd);
}

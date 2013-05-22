#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <stdarg.h>
#include <limits.h>
#include <ctype.h>

#define errorf(...) fprintf(stderr, __VA_ARGS__)
#define assert_or_continue(cond, ...) \
    if (!(cond)) { errorf(__VA_ARGS__); fprintf(stderr, "\n"); continue; }
#define assert(cond, ...) \
    if (!(cond)) { errorf(__VA_ARGS__); fprintf(stderr, "\n"); return NULL; }
#define asserti(cond, ...) \
    if (!(cond)) { errorf(__VA_ARGS__); fprintf(stderr, "\n"); return 0; }
#define assertv(cond, ...) \
    if (!(cond)) { errorf(__VA_ARGS__); fprintf(stderr, "\n"); return; }

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

#define DONT_FREE_THIS  INTPTR_MAX

#define N_CELLS     256 * 1024
#define SECD_ALIGN  4

typedef enum { false, true } bool;

typedef  struct secd    secd_t;
typedef  struct cell    cell_t;
typedef  unsigned long  index_t;

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
    cell_t *stack;      // list
    cell_t *env;        // list
    cell_t *control;    // list
    cell_t *dump;       // list

    cell_t *free;       // list
    cell_t *data;       // array
    cell_t *nil;        // pointer
};


void print_cell(const cell_t *c);
cell_t *free_cell(cell_t *c);

void print_env(secd_t *secd);
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

inline static bool is_nil(const cell_t *cell) {
    secd_t *secd = cell_secd(cell);
    return cell == secd->nil;
}
inline static bool not_nil(const cell_t *cell) {
    return !is_nil(cell);
}

inline static off_t cell_index(const cell_t *cons) {
    if (is_nil(cons)) return -1;
    return cons - cell_secd(cons)->data;
}

inline static cell_t *list_next(const cell_t *cons) {
    if (cell_type(cons) != CELL_CONS) {
        errorf("list_next: not a cons at [%ld]\n", cell_index(cons));
        print_cell(cons);
        return NULL;
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
    assertv(c, "print_cell(NULL)\n");
    if (is_nil(c)) {
         printf("NIL\n");
         return;
    }
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
    while (not_nil(list)) {
        assertv(is_cons(list),
                "Not a cons at [%ld]\n", cell_index(list));
        printf("[%ld]:%ld\t", cell_index(list), cell_index(get_car(list)));
        print_cell(get_car(list));
        printf("  -> ");
        list = list_next(list);
    }
    printf("NIL\n");
}

void printc(cell_t *c) {
    assertv(c, "printc(NULL)");
    if (is_cons(c))
        print_list(c);
    else
        print_cell(c);
}

/*
 * Reference-counting
 */

inline static cell_t *share_cell(cell_t *c) {
    if (not_nil(c)) {
        ++c->nref;
        memtracef("share[%ld] %ld\n", cell_index(c), c->nref);
    } else {
        memdebugf("share[NIL]\n");
    }
    return c;
}

inline static cell_t *drop_cell(cell_t *c) {
    if (is_nil(c)) {
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
    assert(not_nil(cell), "pop_free: no free memory");

    secd->free = list_next(cell);
    memdebugf("NEW [%ld]\n", cell_index(cell));

    cell->type = (intptr_t)secd;
    return cell;
}

void push_free(cell_t *c) {
    assertv(c, "push_free(NULL)");
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
    if (!from) return NULL;
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
    return NULL;
}

inline static cell_t *push(secd_t *secd, cell_t **to, cell_t *what) {
    cell_t *newtop = new_cons(secd, what, *to);
    drop_cell(*to);
    return (*to = share_cell(newtop));
}

inline static cell_t *pop(cell_t **from) {
    cell_t *top = *from;
    assert(not_nil(top), "pop: stack is empty");
    assert(is_cons(top), "pop: not a cons");

    cell_t *val = share_cell(get_car(top));
    *from = share_cell(get_cdr(top));
    drop_cell(top);
    return val;
}

cell_t *push_stack(secd_t *secd, cell_t *newc) {
    cell_t *top = push(secd, &secd->stack, newc);
    memdebugf("PUSH S[%ld (%ld, %ld)]\n", cell_index(top),
                        cell_index(get_car(top)), cell_index(get_cdr(top)));
    return top;
}

cell_t *pop_stack(secd_t *secd) {
    cell_t *cell = pop(&secd->stack);
    memdebugf("POP S[%ld]\n", cell_index(cell));
    return cell; // don't forget to drop_call(result)
}

cell_t *set_control(secd_t *secd, cell_t *opcons) {
    assert(is_cons(opcons),
           "set_control: failed, not a cons at [%ld]\n", cell_index(opcons));
    return (secd->control = share_cell(opcons));
}

cell_t *pop_control(secd_t *secd) {
    return pop(&secd->control);
}

cell_t *push_dump(secd_t *secd, cell_t *cell) {
    cell_t *top = push(secd, &secd->dump, cell);
    memdebugf("PUSH D[%ld] (%ld, %ld)\n", cell_index(top),
            cell_index(get_car(top), get_cdr(top)));
    return top;
}

cell_t *pop_dump(secd_t *secd) {
    cell_t *cell = pop(&secd->dump);
    memdebugf("POP D[%ld]\n", cell_index(cell));
    return cell;
}

/*
 *  SECD built-ins
 */

cell_t *secd_cons(secd_t *secd) {
    ctrldebugf("CONS\n");
    cell_t *a = pop_stack(secd);

    cell_t *b = pop_stack(secd);

    cell_t *cons = new_cons(secd, a, b);
    drop_cell(a); drop_cell(b);

    return push_stack(secd, cons);
}

cell_t *secd_car(secd_t *secd) {
    ctrldebugf("CAR\n");
    cell_t *cons = pop_stack(secd);
    assert(cons, "secd_car: pop_stack() failed");
    assert(not_nil(cons), "secd_car: stack is empty");
    assert(is_cons(cons), "secd_car: cons expected");

    cell_t *car = push_stack(secd, get_car(cons));
    drop_cell(cons);
    return car;
}

cell_t *secd_cdr(secd_t *secd) {
    ctrldebugf("CDR\n");
    cell_t *cons = pop_stack(secd);
    assert(cons, "secd_cdr: pop_stack() failed");
    assert(not_nil(cons), "secd_cdr: stack is empty");
    assert(is_cons(cons), "secd_cdr: cons expected");

    cell_t *cdr = push_stack(secd, get_cdr(cons));
    drop_cell(cons);
    return cdr;
}

cell_t *secd_ldc(secd_t *secd) {
    ctrldebugf("LDC\n");
    cell_t *arg = pop_control(secd);
    assert(arg, "secd_ldc: pop_control failed");
    push_stack(secd, arg);
    drop_cell(arg);
    return arg;
}

cell_t *secd_ld(secd_t *secd) {
    ctrldebugf("LD\n");
    cell_t *arg = pop_control(secd);
    assert(arg, "secd_ld: stack empty");
    assert(atom_type(arg) == ATOM_SYM,
           "secd_ld: not a symbol [%ld]", cell_index(arg));

    cell_t *val = lookup_env(secd, arg->as_atom.as_sym.data);
    drop_cell(arg);
    return push_stack(secd, val);
}

static inline cell_t *to_bool(secd_t *secd, bool cond) {
    return ((cond)? lookup_env(secd, "T") : secd->nil);
}

bool atom_eq(const cell_t *a1, const cell_t *a2) {
    enum atom_type atype1 = atom_type(a1);
    if (a1 == a2)
        return true;
    if (atype1 != atom_type(a2))
        return false;
    switch (atype1) {
      case ATOM_INT: return (a1->as_atom.as_int == a2->as_atom.as_int);
      case ATOM_SYM: return (!strcmp(a1->as_atom.as_sym.data, a2->as_atom.as_sym.data));
      case ATOM_FUNC: return (a1->as_atom.as_ptr == a2->as_atom.as_ptr);
      default: fprintf(stderr, "atom_eq([%ld], [%ld]): don't know how to handle type %d\n",
                       cell_index(a1), cell_index(a2), atype1);
    }
    return false;
}

bool list_eq(const cell_t *xs, const cell_t *ys) {
    asserti(is_cons(xs), "list_eq: [%ld] is not a cons", cell_index(xs));
    if (xs == ys)   return true;
    while (not_nil(xs)) {
        if (!is_cons(xs)) return atom_eq(xs, ys);
        if (is_nil(ys)) return false;
        if (!is_cons(ys)) return false;
        const cell_t *x = get_car(xs);
        const cell_t *y = get_car(ys);
        if (not_nil(x)) {
            if (is_nil(y)) return false;
            if (is_cons(x)) {
                if (!list_eq(x, y)) return false;
            } else {
                if (!atom_eq(x, y)) return false;
            }
        } else {
            if (not_nil(y)) return false;
        }

        xs = list_next(xs);
        ys = list_next(ys);
    }
    return not_nil(ys);
}

cell_t *secd_atom(secd_t *secd) {
    ctrldebugf("ATOM\n");
    cell_t *val = pop_stack(secd);
    assert(val, "secd_atom: pop_stack() failed");
    assert(not_nil(val), "secd_atom: empty stack");

    cell_t *result = to_bool(secd, (val ? !is_cons(val) : true));
    drop_cell(val);
    return push_stack(secd, result);
}

cell_t *secd_eq(secd_t *secd) {
    ctrldebugf("EQ\n");
    cell_t *a = pop_stack(secd);
    assert(a, "secd_eq: pop_stack(a) failed");

    cell_t *b = pop_stack(secd);
    assert(b, "secd_eq: pop_stack(b) failed");

    bool eq = (is_cons(a) ? list_eq(a, b) : atom_eq(a, b));
                
    cell_t *val = to_bool(secd, eq);
    drop_cell(a); drop_cell(b);
    return push_stack(secd, val);
}

static cell_t *arithm_op(secd_t *secd, int op(int, int)) {
    cell_t *a = pop_stack(secd);
    assert(a, "secd_add: pop_stack(a) failed")
    assert(atom_type(a) == ATOM_INT, "secd_add: a is not int");

    cell_t *b = pop_stack(secd);
    assert(b, "secd_add: pop_stack(b) failed");
    assert(atom_type(b) == ATOM_INT, "secd_add: b is not int");

    int res = op(a->as_atom.as_int, b->as_atom.as_int);
    drop_cell(a); drop_cell(b);
    return push_stack(secd, new_number(secd, res));
}

inline static int iplus(int x, int y) {
    return x + y;
}
inline static int iminus(int x, int y) {
    return x - y;
}
inline static int imult(int x, int y) {
    return x * y;
}
inline static int idiv(int x, int y) {
    return x / y;
}
inline static int irem(int x, int y) {
    return x % y;
}

cell_t *secd_add(secd_t *secd) {
    ctrldebugf("ADD\n");
    return arithm_op(secd, iplus);
}
cell_t *secd_sub(secd_t *secd) {
    ctrldebugf("SUB\n");
    return arithm_op(secd, iminus);
}
cell_t *secd_mul(secd_t *secd) {
    ctrldebugf("MUL\n");
    return arithm_op(secd, imult);
}
cell_t *secd_div(secd_t *secd) {
    ctrldebugf("SUB\n");
    return arithm_op(secd, idiv);
}
cell_t *secd_rem(secd_t *secd) {
    ctrldebugf("SUB\n");
    return arithm_op(secd, irem);
}


cell_t *secd_sel(secd_t *secd) {
    ctrldebugf("SEL\n");

    cell_t *condcell = pop_stack(secd);
    print_cell(condcell);

    bool cond = not_nil(condcell) ? true : false;
    drop_cell(condcell);

    cell_t *thenb = pop_control(secd);
    cell_t *elseb = pop_control(secd);
    assert(is_cons(thenb) && is_cons(elseb), "secd_sel: both branches must be conses");

    cell_t *joinb = secd->control;
    secd->control = share_cell(cond ? thenb : elseb);
    push_dump(secd, joinb);

    drop_cell(thenb); drop_cell(elseb); drop_cell(joinb);
    return secd->control;
}

cell_t *secd_join(secd_t *secd) {
    ctrldebugf("JOIN\n");

    cell_t *joinb = pop_dump(secd);
    assert(joinb, "secd_join: pop_dump() failed");

    return (secd->control = share_cell(joinb));
}

cell_t *secd_ldf(secd_t *secd) {
    ctrldebugf("LDF\n");

    cell_t *func = pop_control(secd);
    assert(func, "secd_ldf: failed to get the control path");

    cell_t *closure = new_cons(secd, func, secd->env);
    drop_cell(func);
    return push_stack(secd, closure);
}

cell_t *secd_ap(secd_t *secd) {
    ctrldebugf("AP\n");

    cell_t *closure = pop_stack(secd);
    cell_t *argvals = pop_stack(secd);

    cell_t *newenv = get_cdr(closure);
    cell_t *func = get_car(closure);
    cell_t *argnames = get_car(func);
    cell_t *control = get_car(list_next(func));

    push_dump(secd, secd->control);
    push_dump(secd, secd->env);
    push_dump(secd, secd->stack);

    cell_t *frame = new_cons(secd, argnames, argvals);
    printf("new frame: \n"); print_cell(frame);
    printf(" argnames: \n"); printc(argnames);
    printf(" argvals : \n"); printc(argvals);
    secd->stack = secd->nil;
    secd->env = share_cell(new_cons(secd, frame, newenv));
    print_env(secd);
    secd->control = share_cell(control);

    drop_cell(closure); drop_cell(argvals);

    return control;
}

cell_t *secd_rtn(secd_t *secd) {
    ctrldebugf("RTN\n");

    drop_cell(secd->env);

    assert(not_nil(secd->stack), "secd_rtn: stack is empty");
    cell_t *top = pop_stack(secd);
    assert(is_nil(secd->stack), "secd_rtn: stack holds more than 1 value");

    cell_t *prevstack = pop_dump(secd);
    cell_t *prevenv = pop_dump(secd);
    cell_t *prevcontrol = pop_dump(secd);

    secd->stack = share_cell(new_cons(secd, top, prevstack));
    secd->env = share_cell(prevenv);
    secd->control = share_cell(prevcontrol);

    drop_cell(top); drop_cell(prevstack); 
    drop_cell(prevenv); drop_cell(prevcontrol);

    return top;
}


cell_t *secd_dum(secd_t *secd) {
    ctrldebugf("DUM\n");

    cell_t *oldenv = secd->env;
    cell_t *newenv = new_cons(secd, secd->nil, oldenv);

    secd->env = share_cell(newenv);
    drop_cell(oldenv);

    return newenv;
}

cell_t *secd_rap(secd_t *secd) {
    ctrldebugf("RAP\n");

    cell_t *closure = pop_stack(secd);
    cell_t *argvals = pop_stack(secd);

    cell_t *newenv = get_cdr(closure);
    cell_t *func = get_car(closure);
    cell_t *argnames = get_car(func);
    cell_t *control = get_car(list_next(func));

    push_dump(secd, secd->control);
    push_dump(secd, get_cdr(secd->env));
    push_dump(secd, secd->stack);

    cell_t *frame = new_cons(secd, argnames, argvals);
    printf("new frame: \n"); print_cell(frame);
    printf(" argnames: \n"); printc(argnames);
    printf(" argvals : \n"); printc(argvals);
    newenv->as_cons.car = share_cell(frame);

    secd->stack = secd->nil;
    secd->env = share_cell(newenv);
    print_env(secd);

    secd->control = share_cell(control);

    drop_cell(closure); drop_cell(argvals);

    return control;
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
const cell_t sub_func   = INIT_FUNC(secd_sub);
const cell_t mul_func   = INIT_FUNC(secd_mul);
const cell_t div_func   = INIT_FUNC(secd_div);
const cell_t rem_func   = INIT_FUNC(secd_rem);
const cell_t ldc_func   = INIT_FUNC(secd_ldc);
const cell_t ld_func    = INIT_FUNC(secd_ld);
const cell_t eq_func    = INIT_FUNC(secd_eq);
const cell_t atom_func  = INIT_FUNC(secd_atom);
const cell_t sel_func   = INIT_FUNC(secd_sel);
const cell_t join_func  = INIT_FUNC(secd_join);
const cell_t ldf_func   = INIT_FUNC(secd_ldf);
const cell_t ap_func    = INIT_FUNC(secd_ap);
const cell_t rtn_func   = INIT_FUNC(secd_rtn);
const cell_t dum_func   = INIT_FUNC(secd_dum);
const cell_t rap_func   = INIT_FUNC(secd_rap);

const cell_t cons_sym   = INIT_SYM("CONS");
const cell_t car_sym    = INIT_SYM("CAR");
const cell_t cdr_sym    = INIT_SYM("CDR");
const cell_t add_sym    = INIT_SYM("ADD");
const cell_t sub_sym    = INIT_SYM("SUB");
const cell_t mul_sym    = INIT_SYM("MUL");
const cell_t div_sym    = INIT_SYM("DIV");
const cell_t rem_sym    = INIT_SYM("REM");
const cell_t ldc_sym    = INIT_SYM("LDC");
const cell_t ld_sym     = INIT_SYM("LD");
const cell_t eq_sym     = INIT_SYM("EQ");
const cell_t atom_sym   = INIT_SYM("ATOM");
const cell_t sel_sym    = INIT_SYM("SEL");
const cell_t join_sym   = INIT_SYM("JOIN");
const cell_t ldf_sym    = INIT_SYM("LDF");
const cell_t ap_sym     = INIT_SYM("AP");
const cell_t rtn_sym    = INIT_SYM("RTN");
const cell_t dum_sym    = INIT_SYM("DUM");
const cell_t rap_sym    = INIT_SYM("RAP");

const cell_t two_num    = INIT_NUM(2);
const cell_t t_sym      = INIT_SYM("T");
const cell_t nil_sym    = INIT_SYM("NIL");

const struct {
    const cell_t *sym;
    const cell_t *val;
} global_binding[] = {
    { &atom_sym,    &atom_func },
    { &cons_sym,    &cons_func },
    { &car_sym,     &car_func },
    { &cdr_sym,     &cdr_func },

    { &add_sym,     &add_func },
    { &sub_sym,     &sub_func },
    { &mul_sym,     &mul_func },
    { &div_sym,     &div_func },
    { &rem_sym,     &rem_func },

    { &eq_sym,      &eq_func  },
    { &ldc_sym,     &ldc_func },
    { &ld_sym,      &ld_func  },

    { &sel_sym,     &sel_func },
    { &join_sym,    &join_func },
    { &ldf_sym,     &ldf_func },
    { &ap_sym,      &ap_func  },
    { &rtn_sym,     &rtn_func },
    { &dum_sym,     &dum_func },
    { &rap_sym,     &rap_func },

    { &t_sym,       &t_sym    },
    { NULL,         NULL  } // must be last
};

void fill_global_env(secd_t *secd) {
    int i;
    cell_t *symlist = secd->nil;
    cell_t *vallist = secd->nil;

    for (i = 0; global_binding[i].sym; ++i) {
        cell_t *sym = new_clone(secd, global_binding[i].sym);
        cell_t *val = new_clone(secd, global_binding[i].val);
        symlist = new_cons(secd, sym, symlist);
        vallist = new_cons(secd, val, vallist);
    }

    cell_t *sym = new_clone(secd, &nil_sym);
    cell_t *val = secd->nil;
    symlist = new_cons(secd, sym, symlist);
    vallist = new_cons(secd, val, vallist);

    cell_t *frame = new_cons(secd, symlist, vallist);
    cell_t *env = new_cons(secd, frame, secd->nil);

    secd->env = share_cell(env);
}

cell_t *lookup_env(secd_t *secd, const char *symname) {
    cell_t *env = secd->env;
    assert(cell_type(env) == CELL_CONS, "lookup_env: environment is not a list\n");

    while (not_nil(env)) {       // walk through frames
        cell_t *frame = get_car(env);
        if (is_nil(frame)) {
            printf("lookup_env: warning: skipping OMEGA-frame...\n");
            env = list_next(env);
            continue;
        }
        cell_t *symlist = get_car(frame);
        cell_t *vallist = get_cdr(frame);

        while (not_nil(symlist)) {   // walk through symbols
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
    return NULL;
}

cell_t *lookup_symbol(secd_t *secd, const char *symname) {
    cell_t *env = secd->env;
    assert(cell_type(env) == CELL_CONS, "lookup_symbol: environment is not a list\n");

    while (not_nil(env)) {       // walk through frames
        cell_t *frame = get_car(env);
        cell_t *symlist = get_car(frame);

        while (not_nil(symlist)) {   // walk through symbols
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
    return NULL;
}

void print_env(secd_t *secd) {
    cell_t *env = secd->env;
    int i = 0;
    printf("Environment:\n");
    while (not_nil(env)) {
        printf(" Frame #%d:\n", i++);
        cell_t *frame = get_car(env);
        cell_t *symlist = get_car(frame);
        cell_t *vallist = get_cdr(frame);

        while (not_nil(symlist)) {
            cell_t *sym = get_car(symlist);
            cell_t *val = get_car(vallist);
            if (atom_type(sym) != ATOM_SYM)
                fprintf(stderr, "print_env: not a symbol at *%p in vallist\n", sym);
            printf("  %s\t=>\t", sym->as_atom.as_sym.data);
            print_cell(val);

            symlist = list_next(symlist);
            vallist = list_next(vallist);
        }

        env = list_next(env);
    }
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

    memset(p->issymbc, false, 0x20);
    memset(p->issymbc + 0x20, true, UCHAR_MAX - 0x20);
    char *s = " ();\n";
    while (*s)
        p->issymbc[(unsigned char)*s++] = false;
    return p;
}

secd_parser_t *new_parser(FILE *f) {
    secd_parser_t *p = (secd_parser_t *)calloc(1, sizeof(secd_parser_t));
    return init_parser(p, f);
}

inline static int nextchar(secd_parser_t *p) {
    //printf("nextchar\n");
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

void test_lexer(void) {
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

cell_t *read_list(secd_t *secd, secd_parser_t *p) {
    cell_t *head = secd->nil, *tail = secd->nil;
    cell_t *newtail, *val;
    while (true) {
        int tok = lexnext(p);
        switch (tok) {
          case TOK_STR:
              val = new_symbol(secd, p->strtok); break;
          case TOK_NUM:
              val = new_number(secd, p->numtok); break;
          case TOK_EOF: case ')':
              return head;
          case '(':
              val = read_list(secd, p);
              if (p->token == TOK_ERR) {
                  free_cell(head);
                  errorf("read_list: TOK_ERR\n");
                  return NULL;
              }
              if (p->token == TOK_EOF) {
                  free_cell(head);
                  errorf("read_list: TOK_EOF, ')' expected");
              }
              assert(p->token == ')', "read_list: not a closing bracket");
              break;
          case TOK_ERR:
              free_cell(head); return NULL;
        }

        newtail = new_cons(secd, val, secd->nil);
        if (not_nil(head)) {
            tail->as_cons.cdr = share_cell(newtail);
            tail = newtail;
        } else {
            head = tail = newtail;
        }
    }
}

cell_t *read_secd(secd_t *secd, FILE *f) {
    secd_parser_t p;
    init_parser(&p, f);

    cell_t *result = read_list(secd, &p);
    if (p.token != TOK_EOF) {
        errorf("read_secd: failed\n");
        if (result) drop_cell(result);
        return NULL;
    }
    return result;
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
    secd->nil = c;
    c->type = (intptr_t)secd | CELL_CONS;
    c->as_cons.cdr = NULL;

    secd->free = secd->data;
    secd->stack = secd->dump =  secd->nil;
    secd->control = secd->env =  secd->nil;
    return secd;
}

void run_secd(secd_t *secd) {
    cell_t *op;
    while (true)  {
        op = pop_control(secd);
        assertv(op, "run_secd: no command");

        print_cell(op);
        assert_or_continue(atom_type(op) == ATOM_SYM,
                "run: not a symbol at [%ld]\n", cell_index(op));

        const char *symname = op->as_atom.as_sym.data;
        if (!strcmp("STOP", symname)) return;

        cell_t *val = lookup_env(secd, symname);
        assert_or_continue(val, "run: lookup_env() failed for %s\n", symname);
        assert_or_continue(atom_type(val) == ATOM_FUNC, "run: not a ATOM_FUNC\n");
        drop_cell(op);

        secd_opfunc_t callee = (secd_opfunc_t) val->as_atom.as_ptr;
        callee(secd);

        printf("Stack:\n"); print_list(secd->stack);
    }
}

secd_t __attribute__((aligned(1 << SECD_ALIGN))) secd;

int main() {
    init_secd(&secd);

    fill_global_env(&secd);
    print_env(&secd);

    printf(">>>>>\n");
    cell_t *inp = read_secd(&secd, NULL);
    asserti(inp, "read_secd failed");
    if (is_nil(inp)) {
        printf("no commands.");
        return 0;
    }

    set_control(&secd, inp);
    printf("Control path:\n");
    print_list(secd.control);

    printf("<<<<<\n");
    run_secd(&secd);

    printf("-----\n");
    if (not_nil(secd.stack)) {
        printf("Stack head:\n");
        printc(get_car(secd.stack));
    } else
        printf("Stack is empty\n");
    return 0;
}

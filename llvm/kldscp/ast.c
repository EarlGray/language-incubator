#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <ctype.h>

#include "ast.h"

#define DEBUG  (0)

#define assertf(cond, ...) do { \
    if (!(cond)) { fprintf(stderr, __VA_ARGS__); return NULL; } \
    } while (0)

#define assert_parsed(parser, tkn, ...) \
    if ((parser)->token != (tkn)) return parse_error((parser), __VA_ARGS__);

/*
 * Small in-place list implementation
 */
struct list_node {
    void *data;
    struct list_node *next;
};

struct list {
    list_node_t *head, *last;
    size_t length;
};

list_t *new_list(void) {
    list_t *l = (list_t *)malloc(sizeof(list_t));
    l->head = NULL;
    l->last = NULL;
    l->length = 0;
    return l;
}

list_t *list_append(list_t *l, void *data) {
    assertf(l, "list_append(NULL)\n");

    list_node_t *n = (list_node_t*)malloc(sizeof(list_node_t));
    n->data = data;
    n->next = NULL;

    if (!l->head) 
        l->head = n;

    if (l->last) 
        l->last->next = n;
    l->last = n;
    ++ l->length;

    return l;
}

list_node_t *list_head(list_t *l) {
    assertf(l, "list_head(NULL)\n");
    return l->head;
}

list_node_t *list_next(list_node_t *n) {
    assertf(n, "list_next(NULL)\n");
    return n->next;
}

void * list_data(list_node_t *n) {
    assertf(n, "list_data(NULL)\n");
    return n->data;
}

size_t list_length(list_t *l) {
    if (! l) return 0;
    return l->length;
}

void free_list(list_t *l, void (*free_node)(void *)) {
    list_node_t *n = l->head;
    while (n) {
        if (free_node) 
            free_node(n->data);
        else
            free(n->data);
        n = n->next;
    }
    free(l);
}

/*
 * Binary operations
 */

#define PAREN_PRECEDENCE   100

int binop_preceds[128] = {
    ['+'] = 10,
    ['-'] = 20,
    ['*'] = 40,
    ['/'] = 50,
    ['<'] = 5,
};

int unary_op[128] = { 0 };

int is_unary(int op) {
    if (0 > op || op > 127) return 0;
    return unary_op[op];
}

int binop_precedence(int op) {
    if (!(0 < op && op < 128)) return 0;
    return binop_preceds[op];
}

/*
 *  Lexer
 */

struct lexstate {
    /* interface part */
    int token;

    double numtoken;
    char strtoken[STRTOKEN_MAX_SIZE];
    char op;

    /* private part */
    int lastchar;
    nextchar_func_t nextchar;
};

void lexinit(lexer_t *, nextchar_func_t next);
int lextoken(lexer_t *);


void lexinit(lexer_t *lex, nextchar_func_t next) {
    lex->lastchar = ' ';
    memset(lex->strtoken, 0, STRTOKEN_MAX_SIZE);
    lex->token = 0;
    lex->nextchar = next;
}

lexer_t *new_lexer(nextchar_func_t nextchar) {
    lexer_t *lex = (lexer_t*)malloc(sizeof(lexer_t));
    lexinit(lex, nextchar);
    return lex;
}

inline static int lexcmp(lexer_t *lex, const char *s) {
    return !strncmp(s, lex->strtoken, STRTOKEN_MAX_SIZE);
}

const struct {
    const char *word;
    int token;
} reserved[] = {
    { "if",     TOK_IF      },
    { "then",   TOK_THEN    },
    { "else",   TOK_ELSE    },
    { "def",    TOK_DEF     },
    { "extern", TOK_EXT     },
    { "import", TOK_IMPORT  },
    { "binary", TOK_BINARY  },
    { "unary",  TOK_UNARY   },
    { NULL,     TOK_EOF     },
};

int lextoken(lexer_t *lex) {
    while (isspace(lex->lastchar))
        lex->lastchar = lex->nextchar(lex);

    if (isalpha(lex->lastchar)) {
        char *idstr = lex->strtoken;
        *idstr++ = lex->lastchar;

        while (isalnum(lex->lastchar = lex->nextchar(lex)))
            *idstr++ = lex->lastchar;

        *idstr = 0;

        int i;
        for (i = 0; reserved[i].word; ++i)
            if (lexcmp(lex, reserved[i].word))
                return (lex->token = reserved[i].token);

        return (lex->token = TOK_ID);
    }

    if (isdigit(lex->lastchar) || (lex->lastchar == '.')) {
        char *numstr = lex->strtoken;

        do {
            *numstr++ = lex->lastchar;
            lex->lastchar = lex->nextchar(lex);
        } while (isdigit(lex->lastchar) || lex->lastchar == '.');
        *numstr = 0;

        if (!strcmp(lex->strtoken, ".")) {
            return (lex->token = '.');
        };

        char *end = 0;
        lex->numtoken = strtod(lex->strtoken, &end);
        //printf("strtod: read %ld\n", (end - lex->strtoken));
        return (lex->token = TOK_NUM);
    }

    if (lex->lastchar == '#') {
        do lex->lastchar = lex->nextchar(lex);
        while (lex->lastchar != EOF && lex->lastchar != '\n');
        
        if (lex->lastchar != EOF)
            return lextoken(lex);
    }
    
    if (lex->lastchar == EOF)
        return (lex->token = TOK_EOF);

    if (binop_precedence(lex->lastchar)) {
        lex->op = lex->lastchar;
        lex->lastchar = lex->nextchar(lex);
        return (lex->token = TOK_BINOP);
    }
    if (is_unary(lex->lastchar)) {
        lex->op = lex->lastchar;
        lex->lastchar = lex->nextchar(lex);
        return (lex->token = TOK_UNARY);
    }

    int thischar = lex->lastchar;
    lex->lastchar = lex->nextchar(lex);
    return (lex->token = thischar);
}


/*
 *  Parser
 */

void next_token(parser_t *p) {
    p->prev_token = p->token;
    p->token = lextoken(p->lex);
#if DEBUG
    printf("next_token: ");
    int i;
    for (i = 0; reserved[i].word; ++i) 
        if (p->token == reserved[i].token) {
            printf("%s\n", reserved[i].word);
            return ;
        }
    switch (p->token) {
      case TOK_NUM: printf("NUM(%f)\n", p->lex->numtoken); break;
      case TOK_ID: printf("ID(%s)\n", p->lex->strtoken); break;
      case TOK_BINOP: printf("BINOP(%c)\n", p->lex->op); break;
      case TOK_EOF: printf("EOF\n"); break;
      default: printf("'%1$c' (%1$d)\n", p->token);
    }
#endif
}

parser_t *new_parser(lexer_t *lex) {
    parser_t *p = (parser_t*)malloc(sizeof(parser_t));
    parseinit(p, lex);
    return p;
}

void parseinit(parser_t *p, lexer_t *lex) {
    p->lex = lex;
    p->token = TOK_START;
}

int parse_eof(parser_t *p) {
    return p->token == TOK_EOF;
}

int parsed_semicolon(parser_t *p) {
    return p->prev_token == ';';
}

static inline ast_t *astmalloc(int type) {
    ast_t *ast = (ast_t *)malloc(sizeof(ast_t));
    ast->type = type;
    return ast;
}

ast_t *new_num(double val) { 
    ast_t *num = astmalloc(AST_NUM);
    num->as_num = val;
    return num;
}

ast_t *new_fundef(const char *name, list_t *args, ast_t *body) {
    ast_t *fundef = astmalloc(AST_FUNDEF);
    fundef->as_fundef.name = name;
    fundef->as_fundef.args = args;
    fundef->as_fundef.body = body;
    return fundef;
}

ast_t *new_funcall(const char *name, list_t *args) {
    ast_t *funcall = astmalloc(AST_CALL);
    funcall->as_funcall.name = name;
    funcall->as_funcall.args = args;
    return funcall;
}

ast_t *new_binop(char op, ast_t *lhs, ast_t *rhs, int prec) {
    ast_t *bop = astmalloc(AST_BINOP);
    bop->as_binop.op = op;
    bop->as_binop.lhs = lhs;
    bop->as_binop.rhs = rhs;
    bop->as_binop.preced = prec;
    return bop;
}

ast_t *new_var(const char *varname) {
    ast_t *var = astmalloc(AST_VAR);
    var->as_var = varname;
    return var;
}

ast_t *new_if(ast_t *cond, ast_t *thenb, ast_t *elseb) {
    ast_t *ifexpr = astmalloc(AST_IF);
    ifexpr->as_if.cond = cond;
    ifexpr->as_if.thenb = thenb;
    ifexpr->as_if.elseb = elseb;
    return ifexpr;
}

ast_t *new_import(const char *name) {
    ast_t *import = astmalloc(AST_IMPORT);
    import->as_import = name;
    return import;
}



ast_t *parse_error(parser_t *p, const char *msg, ...) {
    va_list va;
    va_start(va, msg);
    vfprintf(stderr, msg, va);
    va_end(va);
    next_token(p);
    return NULL;
}


ast_t *parse_expr(parser_t *);

ast_t *parse_num(parser_t *p) {
    assert_parsed(p, TOK_NUM, "number expected\n");
    double val = p->lex->numtoken;
    next_token(p);
    return new_num(val);
}

ast_t *parse_paren_expr(parser_t *p) {
    assert_parsed(p, '(', "parse_paren_expr: expected '('\n");
    next_token(p);

    ast_t *expr = parse_expr(p);
    assertf(expr, "parse_paren: parse_expr() failed\n");
    if (expr->type == AST_BINOP)
        expr->as_binop.preced = PAREN_PRECEDENCE;

    assert_parsed(p, ')', "parse_paren_expr(): expected ')'\n");
    next_token(p);
    return expr;
}

ast_t *parse_idexpr(parser_t *p) {
    assert_parsed(p, TOK_ID, "parse_id: TOK_ID expected\n");

    const char *identifier = strdup(p->lex->strtoken);
    next_token(p);

    // is it a variable
    if (p->token != '(') {
        return new_var(identifier);
    }

    // is it a function call
    list_t *args = NULL;
    next_token(p);
    if (p->token == ')') 
        next_token(p);
    else {
        args = new_list();
        while (1) {
            ast_t *expr = parse_expr(p);
            assertf(expr, "funcall arguments: failed to read expression\n");

            list_append(args, expr);

            if (p->token == ')') {
                next_token(p);
                break;
            }

            assert_parsed(p, ',', "funcall argmuents: a comma inspected\n");

            next_token(p);
        }
    }

    return new_funcall(identifier, args);
}

ast_t *parse_if(parser_t *p) {
    assert_parsed(p, TOK_IF, "parse_if: 'if'  expected\n");

    next_token(p);
    ast_t *cond = parse_expr(p);
    assertf(cond, "parse_if: parse_expr(condition) failed\n");

    assert_parsed(p, TOK_THEN, "parse_if: 'then' expected\n");

    next_token(p);
    ast_t *thenb = parse_expr(p);
    assertf(thenb, "parse_if: parse_expr(thenb) failed\n");

    assert_parsed(p, TOK_ELSE, "parse_if: 'else' expected\n");

    next_token(p);
    ast_t *elseb = parse_expr(p);
    assertf(elseb, "parse_if: parse_expr(elseb) failed\n");

    return new_if(cond, thenb, elseb);
}

ast_t *parse_binop(parser_t *p, ast_t *lhs) {
    int op1 = p->lex->op;
    int p1 = binop_precedence(op1);
    if (p1 == 0)
        return parse_error(p, "Operation '%c' is not defined\n", op1);

    next_token(p);
    ast_t *expr = parse_expr(p);
    assertf(expr, "parse_binop: parse_expr() failed\n");

    if (expr->type != AST_BINOP)
        return new_binop(op1, lhs, expr, p1);

    int op2 = expr->as_binop.op;
    int p2 = expr->as_binop.preced;

    //printf("parse_binop(): '%c'[%d], '%c'[%d]\n", op1, p1, op2, p2);
    if (p1 < p2)
        return new_binop(op1, lhs, expr, p1);

    // rebalance
    return new_binop(op2,
            new_binop(op1, lhs, expr->as_binop.lhs, p1),
            expr->as_binop.rhs, p2);
}

ast_t *parse_unary(parser_t *p) {
    assert_parsed(p, TOK_UNARY, "parse_unary: TOK_UNARY expected\n");

    char op = p->lex->op;
    char funname[10] = "unary";
    funname[strlen("unary")] = op;

    next_token(p);
    ast_t *e = parse_expr(p);
    assertf(e, "parse_unary: parse_expr failed\n");

    return new_funcall(strdup(funname), list_append(new_list(), e));
}

ast_t *parse_primary(parser_t *p) {
    switch (p->token) {
      case TOK_ID:    return parse_idexpr(p);
      case TOK_NUM:   return parse_num(p);
      case TOK_IF:    return parse_if(p);
      case TOK_BINOP: p->token = TOK_UNARY; // fall through
      case TOK_UNARY: case TOK_BINARY: 
                      return parse_unary(p);
      case '(' :      return parse_paren_expr(p);
    }
    return parse_error(p, "parse_primary: unknown token '%1$c' (%1$d)\n", p->token);
}

ast_t *parse_expr(parser_t *p) {
    ast_t *expr = parse_primary(p);
    assertf(expr, "parse_expr: parse_primary() failed\n");

    if (p->token == TOK_BINOP)
        return parse_binop(p, expr);

    return expr;
}

static ast_t * parse_proto(parser_t *p) {
    assert_parsed(p, TOK_ID, "parse_proto: expected function name\n");
    const char *fname = strdup(p->lex->strtoken);

    next_token(p);
    assert_parsed(p, '(', "parse_proto: expected '('\n");

    next_token(p);
    list_t *args = NULL;
    if (p->token != ')') {
        args = new_list();
        while (1) {
            assert_parsed(p, TOK_ID, "parse_proto: an identifier expected\n");

            list_append(args, strdup(p->lex->strtoken));

            next_token(p);
            if (p->token == ')')
                break;

            assert_parsed(p, ',', "parse_ext: a comma expected\n");
            next_token(p);
        }
    }

    next_token(p);
    return new_fundef(fname, args, NULL);
}

ast_t *parse_opdef(parser_t *p) {
    switch (p->token) {
      case TOK_UNARY: case TOK_BINARY: break;
      default: return parse_error(p, "parse_opdef: 'unary' or 'binary' expected\n");
    }

    int isbinary = (p->token == TOK_BINARY);
    next_token(p);

    int c = p->token;
    int prec = DEFAULT_PRECEDENCE;

    // a quick hack to allow user-defined unary operations like '-'
    if (p->token == TOK_BINOP && !isbinary) {
        c = p->lex->op;
    }

    if (isbinary && binop_precedence(c))
        return parse_error(p, "op '%c' already defined\n", c);

    if (!ispunct(c))
        return parse_error(p, "parse_opdef: token '%1$c' (%1$d) can't be binary/unary op\n", c);

    char funname[10];
    snprintf(funname, 10, "%s%c", (isbinary ? "binary" : "unary"), c);

    next_token(p);
    if (p->token == TOK_NUM) {
        prec = (int) p->lex->numtoken;
        next_token(p);
    }
    assert_parsed(p, '(', "parse_opdef: '(' expected\n");

    next_token(p);
    assert_parsed(p, TOK_ID, "parse_opdef: an identifier expected for LHS\n");

    list_t *args = new_list();
    list_append(args, (void *)strdup(p->lex->strtoken));

    next_token(p);

    if (isbinary) {
        assert_parsed(p, ',', "parse_opdef: binary op must have a second argument\n");
        next_token(p);

        assert_parsed(p, TOK_ID, "parse_opdef: an identifier expected for RHS\n");
        list_append(args, (void *)strdup(p->lex->strtoken));

        next_token(p);
    }

    assert_parsed(p, ')', "parse_opdef: %s op must have %d arguments\n", funname, (isbinary ? 2 : 1));
    next_token(p);

    ast_t *body = parse_expr(p);
    assertf(body, "parse_opdef: body expected\n");

    // commit a new operation
    if (isbinary) {
        binop_preceds[c] = prec;
    } else {
        unary_op[(int)c] = 1;
    }
    return new_fundef(strdup(funname), args, body);
}

ast_t *parse_def(parser_t *p) {
    assert_parsed(p, TOK_DEF, "parse_def: 'def' expected\n");
    next_token(p);

    if (p->token == TOK_UNARY || p->token == TOK_BINARY)
        return parse_opdef(p);
            
    ast_t *fundef = parse_proto(p);
    assertf(fundef, "parse_def: parse_proto() failed\n");

    ast_t *body = parse_expr(p);
    assertf(body, "parse_def: body expected\n");

    fundef->as_fundef.body = body;
    return fundef;
}

ast_t *parse_extern(parser_t *p) {
    assert_parsed(p, TOK_EXT, "parse_ext: TOK_EXT expected\n");

    next_token(p);
    return parse_proto(p);
}

ast_t *parse_import(parser_t *p) {
    assert_parsed(p, TOK_IMPORT, "parse_import: TOK_IMPORT expected\n");
    next_token(p);

    assert_parsed(p, TOK_ID, "parse_import: failed");

    char *modname = strdup(p->lex->strtoken);
    next_token(p);
    return new_import(modname);
}

ast_t *parse_toplevel(parser_t *p) {
    switch (p->token) {
      case TOK_START: 
        next_token(p); 
        return parse_toplevel(p);
      case TOK_EOF:
        return NULL;
      case TOK_EXT: 
        return parse_extern(p);
      case TOK_DEF:
        return parse_def(p);
      case TOK_IMPORT:
        return parse_import(p);
      case ';':
        next_token(p);
        return NULL;
      default: 
        return parse_expr(p);
    }
}


/*
 *  Dumping AST
 */

inline static void print_indent(int indent) {
    int i;
    for (i = 0; i < indent; ++i) printf("  ");
}

void print_ast(int indent, ast_t *ast) {
    switch (ast->type) {
      case AST_NUM:
        print_indent(indent);
        printf("NUM(%f)\n", ast->as_num);
        break;
      case AST_VAR:
        print_indent(indent);
        printf("VAR(%s)\n", ast->as_var);
        break;
      case AST_IMPORT:
        print_indent(indent);
        printf("IMPORT(%s)\n", ast->as_import);
        break;
      case AST_BINOP:
        print_indent(indent); printf("BINOP(%c)\n", ast->as_binop.op);
        print_ast(indent + 1, ast->as_binop.lhs);
        print_ast(indent + 1, ast->as_binop.rhs);
        break;
      case AST_IF:
        print_indent(indent); printf("IF:\n");
        print_ast(indent + 1, ast->as_if.cond);
        print_indent(indent); printf("THEN:\n");
        print_ast(indent + 1, ast->as_if.thenb);
        print_indent(indent); printf("ELSE:\n");
        print_ast(indent + 1, ast->as_if.elseb);
        break;
      case AST_CALL:
        print_indent(indent); printf("CALL(%s)\n", ast->as_funcall.name);
        if (ast->as_funcall.args) {
            list_node_t *arg = list_head(ast->as_funcall.args);
            while (arg) {
                print_indent(indent); printf("ARG:\n");
                print_ast(indent + 1, (ast_t *)list_data(arg));
                arg = list_next(arg);
            }
        }
        break;
      case AST_FUNDEF: {
        print_indent(indent);
        printf("DEF %s(", ast->as_fundef.name);
        // arguments
        if (ast->as_fundef.args) {
            list_node_t *arg = list_head(ast->as_fundef.args);
            while (arg) {
                printf("%s", (char *)list_data(arg));
                arg = list_next(arg);
                if (!arg) break;
                printf(", ");
            }
        }
        printf(")\n");

        // body
        ast_t* body = ast->as_fundef.body;
        if (body)
            print_ast(indent + 1, body);
        else {
            print_indent(indent + 1);
            printf("<extern>\n");
        }

        } break;
      default:
        printf("PRINT ERROR: unknown ast->type %d\n", ast->type);
    }
}

/*
 * AST operations
 */

void free_ast(ast_t *ast) {
    switch (ast->type) {
      case AST_NUM: break;
      case AST_VAR: 
        free((void *)ast->as_var); 
        break;
      case AST_IMPORT: 
        free((void *)ast->as_import); 
        break;
      case AST_BINOP:
        free_ast(ast->as_binop.lhs);
        free_ast(ast->as_binop.rhs);
        break;
      case AST_IF:
        free_ast(ast->as_if.cond);
        free_ast(ast->as_if.thenb);
        free_ast(ast->as_if.elseb);
        break;
      case AST_FUNDEF: {
        list_t *args = ast->as_fundef.args;
        if (args)
            free_list(args, NULL);

        ast_t *body = ast->as_fundef.body;
        if (body) free(body);

        free((void *)ast->as_fundef.name);
        } break;
      case AST_CALL:
        if (ast->as_funcall.args)
            free_list(ast->as_funcall.args, (void (*)(void *))free_ast);
        free((void *)ast->as_funcall.name);
        break;
    }
    free(ast);
}

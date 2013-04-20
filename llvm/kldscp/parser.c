#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

#include "lexer.h"

#define assertf(cond, ...) do { \
    if (cond) { fprintf(__VA_ARGS__); return NULL; } \
    } while (0)

/*
 * Small in-place list implementation
 */
typedef struct list_node {
    void *data;
    struct list_node *next;
} list_node_t;

typedef struct list {
    list_node_t *head, *last;
    size_t length;
} list_t;

list_t *new_list(void) {
    list_t *l = malloc(sizeof(list_t));
    l->head = NULL;
    l->length = 0;
    return l;
}

list_t *list_append(list_t *l, void *data) {
    assertf(l, "list_append(NULL)\n");
    list_node_t *n = malloc(sizeof(list_node_t));
    n->data = data;
    n->next = NULL;
    l->last->next = n;
    l->last = n;
    ++ l->length;
    return l;
}

void free_list(list_t *l) {
    list_node_t *n = l->head;
    while (n) {
        free(n->data);
        n = n->next;
    }
    free(l);
}

/*
 *  Lexer
 */

enum token_e {
    TOK_EOF = -1,
    TOK_DEF = -2,
    TOK_EXT = -3,
    TOK_ID = -4,
    TOK_NUM = -5
};

typedef  struct lexstate  lexer_t;

typedef int (*nextchar_func_t)(lexer_t *);

#define STRTOKEN_MAX_SIZE   256

struct lexstate {
    int token;
    int lastchar;

    double numtoken;
    char strtoken[STRTOKEN_MAX_SIZE];

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

lexer_t *new_lexer(nextchar_func_t *nextchar) {
    lexer_t *lex = malloc(sizeof(lexer_t));
    lexinit(lex, nextchar);
    return lex;
}

int lextoken(lexer_t *lex) {
    while (isspace(lex->lastchar))
        lex->lastchar = lex->nextchar(lex);

    if (isalpha(lex->lastchar)) {
        char *idstr = lex->strtoken;
        *idstr++ = lex->lastchar;

        while (isalnum(lex->lastchar = lex->nextchar(lex)))
            *idstr++ = lex->lastchar;

        *idstr = 0;
        if (!strncmp(lex->strtoken, "def", STRTOKEN_MAX_SIZE)) 
            return TOK_DEF;
        if (!strncmp(lex->strtoken, "extern", STRTOKEN_MAX_SIZE)) 
            return TOK_EXT;
        return TOK_ID;
    }

    if (isdigit(lex->lastchar) || (lex->lastchar == '.')) {
        char *numstr = lex->strtoken;
        do {
            *numstr++ = lex->lastchar;
            lex->lastchar = lex->nextchar(lex);
        } while (isdigit(lex->lastchar) || lex->lastchar == '.');

        *numstr = 0;

        char *end = 0;
        lex->numtoken = strtod(lex->strtoken, &end);
        //printf("strtod: read %ld\n", (end - lex->strtoken));
        return TOK_NUM;
    }

    if (lex->lastchar == '#') {
        do lex->lastchar = lex->nextchar(lex);
        while (lex->lastchar != EOF && lex->lastchar != '\n');
        
        if (lex->lastchar != EOF)
            return lextoken(lex);
    }
    
    if (lex->lastchar == EOF)
        return TOK_EOF;

    int thischar = lex->lastchar;
    lex->lastchar = lex->nextchar(lex);
    return thischar;
}


void test_lexer(void) {
    lexer_t lex = { 0 };
    int c;
    lexinit(&lex, (nextchar_func_t)getchar);

    for (;;) { 
        switch (c = lextoken(&lex)) {
          case TOK_EOF: printf("TOK_EOF\n"); return;
          case TOK_EXT: printf("TOK_EXT\n"); break;
          case TOK_NUM: printf("TOK_NUM(%f)\n", lex.numtoken); break;
          case TOK_DEF: printf("TOK_DEF\n"); break;
          case TOK_ID: printf("TOK_ID(%s)\n", lex.strtoken); break;
          default: printf("'%c'\n", c);
        }
    }
}

/*
 *
 */

typedef  struct parser  parser_t;
typedef void (*nexttoken_func_t)(struct parser*);

struct parser {
    int curr_token;
    int prev_token;

    lexer_t *lex;
};

enum node_type {
    AST_NUM, AST_VAR, AST_BINOP,
    AST_CALL, AST_PROTO, AST_FUNDEF,
};

typedef struct ast_node {
    enum node_type type;
    union {
        // AST_NUM:
        double as_num;

        // AST_VAR:
        const char *as_var;

        // AST_BINOP:
        struct {
            char op;
            struct ast_node *rhs, *lhs;
        } as_binop;

        // AST_CALL:
        struct {
            const char *funname;
            list_t *args;  // of ast_node
        } as_funcall;

        // AST_PROTO:
        struct {
            const char *funname;
            list_t *args; // const char *
        } as_proto;

        // AST_FUNDEF:
        struct {
            struct ast_node *proto;
            struct ast_node *body;
        } as_fundef;
    };
} ast_t;

ast_t *empty_proto(void) {
    ast_t *p = malloc(sizeof(ast_t));
    p->type = AST_PROTO;
    p->as_proto.args = new_list();
    p->as_proto.funname = NULL;
    return p;
}

ast_t *new_fundef(ast_t *proto, ast_t *body) {
    ast_t *fundef = malloc(sizeof(ast_t));
    fundef->as_fundef.proto = proto;
    fundef->as_fundef.body = body;
    return fundef;
}

ast_t *new_var(const char *varname) {
    return strdup(varname);
}

void next_token(parser_t *p) {
    p->prev_token = p->curr_token;
    p->curr_token = lextoken(p->lex);
}

ast_t *parse_error(const char *msg, ...) {
    va_list va;
    va_start(va, msg);
    vfprintf(stderr, msg, va);
    va_end(msg);
    return NULL;
}

ast_t *parse_number(parser_t *p) {
    ast_t *num = malloc(sizeof(ast_t));
    num->type = AST_NUM;
    num->as_num = p->lex->numtoken;
    return num;
}

ast_t *parse_identifier_expr(parser_t *p) {
    next_token(p);
    // is it a variable
    if (p->curr_token != '(')
        return new_var(p->lex->strtoken);

    // is it a function call
    next_token(p);  // '('
    if (p->curr_token != ')') {
    }
    next_token(p);

    return new_funcall(
}

ast_t *parse_paren_expr(parser_t *p) {
    next_token(p);
    assertf(p->curr_token == '(', "parse_paren_expr: expected '('\n");
    ast_t *expr = parse_expr(p);
    if (!expr) return NULL;

    assertf(p->curr_token == ')', "parse_paren_expr(): expected ')'\n");
    next_token(p);
    return expr;
}

ast_t *parse_primary(parser_t *p) {
    switch (p->curr_token) {
      case TOK_ID: return parse_identifier_expr(p);
      case TOK_NUM: return parse_number(p);
      case '(' : return parse_paren_expr(p);
      default: return parse_error("Unknown token, expecting an expression\n");
    }
}

ast_t *parse_expr(parser_t *p) {
    ast_t *lhs = parse_primary(p);
    if (!lhs) return 0;
    return parse_binop(NULL, lhs);
}

ast_t *parse_toplevel(parser_t *p) {
    ast_t *e;
    if (e = parse_expr(p)) {
        return new_fundef(NULL, e);
    }
    return NULL;
}

ast_t *parse_def(parser_t *p) {
    next_token(p);
    assertf(p->curr_token == EXT_DEF, "parse_def: not extern\n");

    ast_t *proto = parse_proto(p);
    assertf(proto, "parse_proto() failed");
    
    ast_t *e = parse_expr(p);
    if (e) return new_fundef(proto, e);
    return NULL;
}

ast_t *parse_extern(parser_t *p) {
    next_token(p);
    return parse_proto(p);
}


void handle_def(parser_t *) {
    if (parse_def(p)) {
        fprintf(stderr, "Parsed a function definition\n");
    } else {
        next_token(p);
    }
}

void handle_extern(parser_t *) {
    if (parse_extern(p)) {
        fprintf(stderr, "Parsed an extern definiton\n");
    } else {
        next_token(p);
    }
}

void handle_toplevelexpr(parser_t *p) {
    if (parse_toplevel(p)) {
        fprintf(stderr, "Parsed a toplevel expression\n");
    } else {
        next_token(p);
    }
}

static void parser_loop(parser_t *p) {
    while (1) {
        fprintf(stderr, "ready> ");
        switch (p->curr_token) {
          case TOK_EOF: printf("Parser: done\n"); return;
          case ';':     next_token(p); break;
          case TOK_DEF: handle_def(p); break;
          case TOK_EXT: handle_extern(p); break;
          default:      handle_toplevelexpr(p); break;
        }
    }
}

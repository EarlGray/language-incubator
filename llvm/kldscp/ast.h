#ifndef __KLDSCP_PARSER__H_
#define __KLDSCP_PARSER__H_

#define STRTOKEN_MAX_SIZE   256

#ifdef __cplusplus
extern "C" {
#include <glob.h>
#endif

/*
 * List 
 */

typedef  struct list_node  list_node_t;
typedef  struct list       list_t;

size_t        list_length(list_t *);
list_node_t * list_head(list_t *l);
list_node_t * list_next(list_node_t *);
void *        list_data(list_node_t *);


/*
 *  Lexer
 */
typedef  struct lexstate  lexer_t;

typedef int (*nextchar_func_t)(lexer_t *);

lexer_t *new_lexer(nextchar_func_t next);


/*
 * Parser
 */

typedef  struct parser  parser_t;
typedef void (*nexttoken_func_t)(struct parser*);

struct parser {
    int token;
    int prev_token;

    lexer_t *lex;
};

enum node_type {
    AST_NUM, 
    AST_VAR, 
    AST_BINOP,
    AST_CALL, 
    AST_FUNDEF,
    AST_IF,
    AST_IMPORT,
};

typedef  struct ast_node  ast_t;

struct ast_node {
    enum node_type type;
    union {
        // AST_FUNDEF:
        struct {
            const char *name;
            // may be NULL
            list_t/* char * */ *args;
            // may be NULL (it is an extern function)
            struct ast_node *body;
        } as_fundef;

        // AST_CALL:
        struct {
            const char *name;
            list_t/*ast_t*/ *args;
        } as_funcall;

        // AST_NUM:
        double as_num;

        // AST_VAR:
        const char *as_var;

        // AST_BINOP:
        struct {
            char op;
            ast_t *rhs, *lhs;
            int preced;
        } as_binop;

        // AST_IMPORT
        const char *as_import;

        // AST_IF
        struct {
            ast_t *cond;
            ast_t *thenb, *elseb;
        } as_if;
    };
};

#define DEFAULT_PRECEDENCE  100
int binop_precedence(int op);

typedef enum {
    TOK_EOF     = -1,
    TOK_DEF     = -2,
    TOK_EXT     = -3,
    TOK_ID      = -4,
    TOK_BINOP   = -5,
    TOK_NUM     = -6,
    TOK_IF      = -7,
    TOK_THEN    = -8,
    TOK_ELSE    = -9,
    TOK_BINARY  = -10,
    TOK_UNARY   = -11,
    TOK_IMPORT  = -12,
    TOK_START   = 0xffff,
} token_e ;

typedef enum {
    BINOP_PLUS = 0,
    BINOP_MINUS,
    BINOP_MUL,
    BINOP_DIV,
    BINOP_LESS,
} binop_e;

parser_t *new_parser(lexer_t *lex);
void parseinit(parser_t *p, lexer_t *lex);

ast_t *parse_toplevel(parser_t *p);

int parsed_semicolon(parser_t *p);
int parse_eof(parser_t *p);

void print_ast(int indent, ast_t *ast);

void free_ast(ast_t *ast);

#ifdef __cplusplus
}
#endif

#endif //__KLDSCP_PARSER__H_

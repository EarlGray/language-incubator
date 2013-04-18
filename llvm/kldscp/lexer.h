#ifndef __LKD_LEXER_H__
#define __LKD_LEXER_H__

enum Token {
    TOK_EOF = -1,
    TOK_DEF = -2,
    TOK_EXT = -3,
    TOK_ID = -4,
    TOK_NUM = -5
};

struct lexstate;

typedef int (*nextchar_func_t)(struct lexstate *);

#define STRTOKEN_MAX_SIZE   256

struct lexstate {
    int token;
    int lastchar;

    double numtoken;
    char strtoken[STRTOKEN_MAX_SIZE];
    nextchar_func_t nextchar;
};

void lexinit(struct lexstate *, nextchar_func_t next);

int lextoken(struct lexstate *);

#endif // __LKD_LEXER_H__

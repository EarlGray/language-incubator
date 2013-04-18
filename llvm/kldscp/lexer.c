#include <string.h>
#include <ctype.h>
#include <stdio.h>

#include "lexer.h"

void lexinit(struct lexstate *lex, nextchar_func_t next) {
    lex->lastchar = ' ';
    memset(lex->strtoken, 0, STRTOKEN_MAX_SIZE);
    lex->token = 0;
    lex->nextchar = next;
}

int lextoken(struct lexstate *lex) {
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

    if (isdigit(lex->lastchar) || lex->lastchar == '.') {
        char *numstr = lex->strtoken;
        do {
            *numstr++ = lex->lastchar;
            lex->lastchar = lex->nextchar(lex);
        } while (isdigit(lex->lastchar) || lex->lastchar == '.');

        *numstr = 0;

        lex->numtoken = strtod(lex->strtoken, 0);
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

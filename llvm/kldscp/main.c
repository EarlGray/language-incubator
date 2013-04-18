#include "lexer.h"
#include <stdio.h>

void test_lexer(void) {
    struct lexstate lex = { 0 };
    lexinit(&lex, (nextchar_func_t)getchar);

    for (;;) { 
        switch (lextoken(&lex)) {
          case TOK_EOF: printf("TOK_EOF\n"); return;
          case TOK_EXT: printf("TOK_EXT\n"); break;
          case TOK_NUM: printf("TOK_NUM(%f)\n", lex.numtoken); break;
          case TOK_DEF: printf("TOK_DEF\n"); break;
          case TOK_ID: printf("TOK_ID(%s)\n", lex.strtoken); break;
          default: printf("'%c' ", lex.lastchar);
        }
    }
}

int main(int argc, char *argv[]) {
    test_lexer();
}

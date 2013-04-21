#include <stdio.h>

#include "ast.h"

void test_parser(void) {
    parser_t p;

    p.token = TOK_START;
    p.lex = new_lexer(getchar);

    while (1) {
        printf("### ");
        ast_t *ast = parse_toplevel(&p);

        if (NULL == ast) {
            if (p.token == TOK_EOF) break;
            if (p.prev_token == ';') continue;

            fprintf(stderr, "======================\n PARSER ERROR\n\n");
            continue;
        }

        print_ast(0, ast);

        free_ast(ast);
    }
    printf("Bye.\n");
}

int main(int argc, char *argv[]) {
    test_parser();
}

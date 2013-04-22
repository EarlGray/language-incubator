#include <stdio.h>

#include "ast.h"
#include "codegen.h"

void test_parser(void) {
    parser_t p;

    parseinit(&p, new_lexer(getchar));

    while (1) {
        printf("#AST# ");
        ast_t *ast = parse_toplevel(&p);

        if (NULL == ast) {
            if (parse_eof(&p)) break;
            if (parsed_semicolon(&p)) continue;

            fprintf(stderr, "======================\n PARSER ERROR\n\n");
            continue;
        }

        print_ast(0, ast);

        free_ast(ast);
    }
    printf("Bye.\n");
}

int main(int argc, char *argv[]) {
    //test_lexer();
    test_parser();
}

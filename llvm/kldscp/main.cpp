#include <stdio.h>
#include <string>

#include "ast.h"
#include "codegen.h"

static int lexnext(lexer_t *) {
    return getchar();
}

void test_parser(void) {
    parser_t p;

    parseinit(&p, new_lexer(lexnext));

    printf("#AST# ");
    while (1) {
        ast_t *ast = parse_toplevel(&p);

        if (NULL == ast) {
            if (parse_eof(&p)) break;
            if (parsed_semicolon(&p)) continue;

            fprintf(stderr, "======================\n PARSER ERROR\n\n");
            continue;
        }

        print_ast(0, ast);

        free_ast(ast);
        printf("#AST# ");
    }
    printf("Bye.\n");
}

void usage(const char *arg0) {
    printf("USAGE: \n");
    printf("    %s                     -- run interpreter\n", arg0);
    printf("    %s [--codegen|-c] [-i] -- test codegenerator (interactively)\n", arg0);
    printf("    %s [--parser|-p]       -- test parser\n", arg0);
}

int main(int argc, char *argv[]) {
    if (argc < 2) {
        test_interp();
        return 0;
    }

    std::string arg1(argv[1]);

    if (arg1 == "--parser" || arg1 == "-p") {
        test_parser();
        return 0;
    }

    if (arg1 == "--codegen" || arg1 == "-c") {
        bool interactive = false;
        if (argc == 3 && std::string(argv[2]) == "-i")
            interactive = true;

        test_codegen(interactive);
        return 0;
    }

    usage(argv[0]);
    return 1;
}

#include <stdio.h>
#include <stdlib.h>

#define YYSTYPE double
#define L_EOS   0

#define BUF_SIZE    256 
char expr_buf[BUF_SIZE];

#include "y.tab.c"

#ifndef YYBISON
#define L_NUM   256

YYSTYPE yylval;
#endif


char is_delim(char c)
{
    switch (c) {
    case ' ': case '\t': 
        return 1;
    default:    
        return 0;
    }
}

char is_digit(const char c)
{
    if (('0' <= c) && (c <= '9'))
        return 1;
    if (c == '.')
        return 1;
    return 0;
}

void lex_error(const char *err_msg)
{
    fprintf(stderr, "Lex error: %s\n", err_msg);
}

int yylex(void)
{
    static int sp = 0;      /*  stream position in expr_buf  */
    while (1) {
        /*  word delimiters    */
        if (is_delim(expr_buf[sp])) {
        } else
        /*  number  */
        if (is_digit(expr_buf[sp])) {
            char *num_end;
            double val = strtod(expr_buf + sp, &num_end);
            if (expr_buf + sp == num_end) {
                lex_error("Can't read double from input");
                goto end_of_stream;
            }
            yylval = val;
            sp = num_end - expr_buf;
            return L_NUM;
        } else 
        /*  the end of stream   */
        if (0 == expr_buf[sp]) {
            goto end_of_stream;
        /*   any other digit   */
        } else {
            return expr_buf[sp++];
        }       
        ++sp;
    }
end_of_stream:
    sp = 0;
    return L_EOS;
}

void yyerror(const char *err_msg)
{
    fprintf(stderr, "Error: %s\n", err_msg);
}

const char prompt[] = "rpn> ";

int main(int argc, char **argv)
{
    while (1) {
        printf("%s", prompt);
        scanf("%s", expr_buf);
    
        if (! strcmp(expr_buf, "quit")) break;
    
#ifdef YYBISON
        if (yyparse()) {
            fprintf(stderr, "yyprse() not 0\n");
        }
        printf("= %f\n", yylval);
#else   // tokenizer test
        int lexem;
        while ((lexem = yylex()) != L_EOS)
        switch (lexem) {
        case L_NUM:
            printf("L_NUM: %f\n", yylval);
            break;
        default:
            printf("token '%c'\n", (char)lexem);
        } 
#endif        
    }   
    return 0;
}

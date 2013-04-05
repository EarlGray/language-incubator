#include <stdio.h>
#include "y.tab.c"

int yylex(void)
{
    int c;  
    while ((c = getchar()) == ' ' || c == '\t');
    if (c == '.' || (isdigit(c))) {
        ungetc(c, stdin);
        scanf("%lf", &yylval);
        return L_NUM;
    }
    if (c == EOF)
        return 0;
    return c;
}

void yyerror(const char *err_msg)
{
    fprintf(stderr, "Syntax error: %s\n", err_msg);
}

int main()
{
    return yyparse();
}

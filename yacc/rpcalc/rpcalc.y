/*  Reverse polish notation calculator  */

%{

#include <math.h>

#define YYSTYPE double

int yylex(void);
void yyerror(const char *);

%}

%token L_NUM

%%
input:  /* empty */
     |  input line
;

line:   '\n'
    |   exp '\n'        { printf("\t%.10g\n", $1);  }
;

exp:    L_NUM           {   $$ = $1;    } 
    |   exp exp '+'     {   $$ = $1 + $2;   }
    |   exp exp '-'     {   $$ = $1 - $2;   }
    |   exp exp '*'     {   $$ = $1 * $2;   }
    |   exp exp '/'     {   $$ = $1 / $2;   }
    |   exp exp '^'     {   $$ = pow($1, $2);   }
    |   exp 'n'         {   $$ = -$1;       }
;

%%



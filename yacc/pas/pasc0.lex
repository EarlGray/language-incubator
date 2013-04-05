
%{
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#define NONE    0
#define BOLD    1
#define UND     4
#define RED     31
#define GREEN   32
#define YELLOW  33
#define BLUE    34
#define MAG     35
#define CYAN    36

#define RED_B   41

void color_print(const char *s, int color) ;

%}

%option noyywrap

DIGIT       [0-9]
ID          [a-z][a-z0-9]*
STRING      '.*'

%%
{DIGIT}+                {   color_print(yytext, MAG);    }
{DIGIT}+"."{DIGIT}*     {   color_print(yytext, MAG);    }
if|then|else|begin|end|function|program|uses|var|type|procedure|while|do|repeat|until|case             {   color_print(yytext, BOLD);    }
integer|string|word|byte|shortint   {   color_print(yytext, GREEN); }
{ID}                    {   color_print(yytext, NONE);    }
{STRING}                {   color_print(yytext, MAG);   }
"+"|"-"|"*"|"/"         {   color_print(yytext, CYAN);   }
"{"[^{}\n]*"}"          {   color_print(yytext, BLUE);  }
[ \t\n]+                {   printf("%s", yytext);   }
"="                     {   color_print(yytext, CYAN);  }
[();,:\.]               {   color_print(yytext, YELLOW);  }
:=                      {   color_print(yytext, YELLOW); }
.                       {   color_print(yytext, RED_B);    }

%%

void color_print(const char *s, int color) {
    printf("[%dm%s[0m", color, s);
}

int main(int argc, char **argv) {
    if (argc > 1) 
        yyin = fopen(argv[1], "r");
    else 
        yyin = stdin;
    return yylex();
}

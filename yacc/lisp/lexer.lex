%{

#include "lexer.h"

%}
%option noyywrap

DIGIT       [0..9]
ID          [^()| \t\n;`,"']
QUOTER      ['`,]

%%

"("                { return L_LP;  }
")"                { return L_RP;  }
error|defun|defmacro|defclass|defgeneric|defmethod|defvar|defparameter|defconst|cond|if|do|dolist|dotimes|lambda   { return L_SPF; }
:{ID}+          { return L_STAT_ID; }
{ID}+               { return L_ID;  }
\"[^\"]*\"       { return L_STRING; }
{QUOTER}            { return L_QUOTER;  }
{DIGIT}*             { return L_NUM; }
;.*\n              { return L_COMMENT;  }
[ \t\n]         { printf("%s", yytext);  }

%%

void lexscan_init(FILE *stream) {
    if (stream == NULL) 
        yyin = stdin;
    else 
        yyin = stream;
}



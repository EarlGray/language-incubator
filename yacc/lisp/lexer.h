#ifndef __LISP_LEX_H__
#define __LISP_LEX_H__

#define L_LP        1
#define L_RP        2
#define L_STRING    3
#define L_NUM       4
#define L_COMMENT   5
#define L_QUOTER    6
#define L_ID        7
#define L_STAT_ID   8
#define L_SPF       9

#include <stdio.h>

int yylex(void);

void lexscan_init(FILE *stream);

extern char *yytext;

#endif // __LISP_LEX_H__

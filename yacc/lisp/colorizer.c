#include "lexer.h"

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

const int token_color[] = {
    0,  YELLOW,  YELLOW,
    CYAN,  MAG, RED, BOLD, 
    NONE, GREEN, BOLD
};

inline void color_print(const char *msg, int color) {
    printf("[%dm%s[0m", color, msg);
}

int main() {
    lexscan_init(NULL);
    int token = 0;
    while (token = yylex()) {
         color_print(yytext, token_color[token]);
    }
}


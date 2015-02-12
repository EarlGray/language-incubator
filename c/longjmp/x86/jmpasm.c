#include <stdio.h>
#if 0
#include <setjmp.h>
#else
#include "my_setjmp.h"
#endif

jmp_buf env;

void second() {
    static const int val = 42;
    printf("second(): I got the number!\n");
    longjmp(env, val);
}

void first() {
    second();
    printf("first(): hit the floor, ouch\n");
}

int main() {
    int val;
    if (val = setjmp(env)) {
        printf("main(): I'm back with val=%d\n", val);
    } else {
        printf("main(): drum roll, preparing to jump through mortal hoops\n");
        first();
    }
}

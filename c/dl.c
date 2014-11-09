#include <dlfcn.h>
#include <stdio.h>
#include <stdlib.h>

typedef int (*printf_t)(const char *, ...);

int main(int argc, char *argv[]) {
    void *dl = dlopen(argv[0], RTLD_LAZY | RTLD_GLOBAL);
    if (!dl)
        exit(-1);

    printf_t ptf = dlsym(dl, "printf");
    if (!ptf)
        exit(-2);

    ptf("Hello world!\n");
    exit(0);
}

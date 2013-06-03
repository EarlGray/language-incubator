#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <sys/mman.h>

typedef int (*func_t)(int,int);

unsigned char asm_add[] = "\x8b\x44\x24\x08\x8b\x4c\x24\x04\x01\xc8\xc3";

int main() {
    void *execmem = mmap(NULL, 4096, 
                         PROT_READ | PROT_WRITE | PROT_EXEC, 
                         MAP_PRIVATE | MAP_ANON, -1, 0);
    if (!execmem) {
        fprintf(stderr, "mmap failed\n\n");
        return 1;
    }
    func_t func = (func_t)memcpy(execmem, asm_add, sizeof(asm_add));
    
    int res = func(4,5);
    printf("%d\n", res);
}

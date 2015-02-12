#define NOT_C_CODE
#include "my_setjmp.h"

#define SIZEOFINT 4

.global my_setjmp
.global my_longjmp

/*
 * int my_setjmp(my_jmp_buf env)
 */
my_setjmp:
    movl 4(%esp), %edx         # &jmp_buf
    movl (%esp),  %ecx         # ret addr

    # save registers protected by cdecl
    movl %ecx, (SIZEOFINT * JMPBUF_EIP_OFFSET)(%edx)
    movl %esp, (SIZEOFINT * JMPBUF_ESP_OFFSET)(%edx)
    movl %ebx, (SIZEOFINT * JMPBUF_EBX_OFFSET)(%edx)
    movl %esi, (SIZEOFINT * JMPBUF_ESI_OFFSET)(%edx)
    movl %edi, (SIZEOFINT * JMPBUF_EDI_OFFSET)(%edx)
    movl %ebp, (SIZEOFINT * JMPBUF_EBP_OFFSET)(%edx)

    xor %eax, %eax
    ret

/*
 * int my_longjmp(my_jmp_buf env, val)
 */
my_longjmp:
    movl 4(%esp), %edx          # &jmp_buf
    movl 8(%esp), %eax          # val

    # restore saved regisers
    movl (SIZEOFINT * JMPBUF_EBP_OFFSET)(%edx), %ebp
    movl (SIZEOFINT * JMPBUF_EDI_OFFSET)(%edx), %edi
    movl (SIZEOFINT * JMPBUF_ESI_OFFSET)(%edx), %esi
    movl (SIZEOFINT * JMPBUF_EBX_OFFSET)(%edx), %ebx
    movl (SIZEOFINT * JMPBUF_ESP_OFFSET)(%edx), %esp
    movl (SIZEOFINT * JMPBUF_EIP_OFFSET)(%edx), %ecx  # return address

    movl %edx, 4(%esp)
    movl %ecx, (%esp)

    movl $1, %ecx
    test %eax, %eax
    cmovzl %ecx, %eax  # set return value to 1 if received val==0

    ret

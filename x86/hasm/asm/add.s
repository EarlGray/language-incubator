.text
.globl asm_add
asm_add:
    movl 8(%esp), %eax
    movl 4(%esp), %ecx
    addl %ecx, %eax 
    ret

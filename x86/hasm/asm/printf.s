.text

.extern printf
.global main
main:
    push %esi
    push %edi
    
    movl $1, %esi
    movl $31, %edi

L1:
    push %esi
    push $format
    call printf
    addl $8, %esp

    addl %esi, %esi
    decl %edi
    
    jnz L1
        
    pop %edi
    pop %esi
    ret

.data

format:
.asciz "%d\n"


.file "printf.s"

 # don't align these lines!:
.global main
  .type main,STT_FUNC   #
.global lol
    
  .type lol,STT_OBJECT
  .extern printf

#.extern printf
.type printf,STT_FUNC

.type howdy,STT_OBJECT   #comment

   

.text
main:
  _main:
    push %esi
    push %edi

    push $howdy
    call printf
    addl $4, %esp
    
    movl $1, %esi
    movl $31, %edi

.L1:
    push %esi
    push $format
    call printf
    addl $8, %esp

    addl %esi, %esi
    addl $1, %edi
    
    jnz .L1

    push $lol
    call printf
    addl $4, %esp
        
    pop %edi 
    pop %esi
    ret


.section .rodata
format:
#.asciz "%d\n"

.data
lol:
#.asciz "lol, bye\n"
#.byte 'c
howdy:
#.asciz "howdy\n"

.text

.global _start
.global start

_start:
start:
    movl $4, %eax       # syscall 4: write()
    movl $1, %ebx       # file handle 1, stdout
    movl $message, %ecx # message string   
    movl $13, %edx      # number of bytes
    int $0x80

    movl $1, %eax       # syscall 1: exit()
    xor %ebx, %ebx      # success
    int $0x80
.data

message:
.asciz "Hello, world\n"

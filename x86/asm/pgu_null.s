# 
# On a 64-bit machine:
# $ gcc -m32 pgu_null.s -o pgu_null.o
# $ ld -melf_i386 pgu_null.o -o pgu_null
#
.section .data
a:
.long 0xdeadbeef
b:
.long 0xfaceb001

.section .text
.globl _start
_start:
  movl $1, %eax
  movl $0, %ebx
  int $0x80

  movl b(%eax), %ecx

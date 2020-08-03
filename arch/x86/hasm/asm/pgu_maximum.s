#   Find maximum number in data_items[]
#   and return it as status code
.data
data_items:
.long 3,67,34,222,45,75,54,34,44,33,22,11,66,0 # terminates with 0

.text
.globl _start
_start:
  movl $0, %edi                     # index in data_items
  movl data_items(,%edi,4), %eax    # get the first value
  movl %eax, %ebx                   # initilize the max variable 

start_loop:
  cmpl $0, %eax         # is this the end?
  je loop_exit
  incl %edi             # i++
  movl data_items(,%edi,4), %eax
  cmpl %ebx, %eax
  jle start_loop
    
  movl %eax, %ebx
  jmp start_loop

loop_exit:
  # %ebx is the status code to return
  movl $1, %eax
  int $0x80

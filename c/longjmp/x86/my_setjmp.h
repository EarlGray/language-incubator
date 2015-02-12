#ifndef __MY_SETJMP_H__
#define __MY_SETJMP_H__


#define MY_JMPBUF_SIZE 6

#define JMPBUF_EIP_OFFSET 0
#define JMPBUF_ESP_OFFSET 1
#define JMPBUF_EBX_OFFSET 2
#define JMPBUF_ESI_OFFSET 3
#define JMPBUF_EDI_OFFSET 4
#define JMPBUF_EBP_OFFSET 5

#ifndef NOT_C_CODE

typedef int my_jmp_buf[MY_JMPBUF_SIZE];

int my_setjmp(my_jmp_buf env);

void my_longjmp(my_jmp_buf env, int val);

#endif 

#define setjmp my_setjmp
#define longjmp my_longjmp
#define jmp_buf my_jmp_buf

#endif // __MY_SETJMP_H__

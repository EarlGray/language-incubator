.org 0x1000
.text
hang:                           # 1000:
    jmp hang                    # 1000:  e9 fb ff ff ff

    jmp *0xdeadbeef             # 1005:  ff 25 ef be ad de

proc1:                          # 100b:  
    mov 4(%esp), %eax           # 100b:  8b 44 24 04
    cmp %eax, %eax              # 100f:  39 c0
    jne .l1                     # 1011:  0f 84 06 00 00 00
    movl $0, %eax               # 1017:  b8 00 00 00 00 00
    ret                         # 101c:  c3
.l1:                            # 101d:
    movl arr1(,%eax,4), %eax    # 101d:  8b 04 85 25 10 00 00
    ret                         # 1024:  c3

.data

arr1:                           # 1025:
.long 0x30
.long 0x31
.long 0x32
.long 0x33

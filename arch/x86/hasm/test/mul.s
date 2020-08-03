.text
imul %dh            # f6 ee
imul %edx           # f7 ea
imull (%edi)         # f7 2f

imulb 0xdeadbeef    # f6 2d ef be ad de
imull 0xdeadbeef    # f7 2d ef be ad de

imul 4(%esp), %edi  # 0f af 7c 24 04

imull $32, %esi, %ecx                    #  6b ce 20
imulw $2, 0xb8000(%esi, %ecx, 2), %bx    #  66 6b 9c 4e 00 80 0b 00 02

imull $0x100, %esi, %ecx                         #  69 ce 00 01 00 00
imull $0x1234, 0xaa55aa55(%ebp, %esi, 1), %eax   #  69 84 35 55 aa 55 aa 43 12 00 00

imul %eax, %esi     # 0f af f0

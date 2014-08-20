.text
facti:
  movl 4(%esp), %ecx        # 0000:  8b 4c 24 04
  cmpl $1, %ecx             # 0004:  83 f9 01
  je .lbl1                  # 0007:  0f 84 19 00 00 00
  movl $1, %eax             # 000d:  b8 01 00 00 00
  movl $1, %edx             # 0012:  ba 01 00 00 00
.lbl2:                      # 0017:
  imull %edx, %eax          # 0017:  0f af c2
  addl $1, %edx             # 001a:  83 c2 01
  cmpl %ecx, %edx           # 001d:  39 ca
  jne .lbl2                 # 001f:  0f 85 f2 ff ff ff
  ret                       # 0025:  c3
.lbl1:                      # 0026:
  movl $1, %eax             # 0026:  b8 01 00 00 00
  ret                       # 002b:  c3

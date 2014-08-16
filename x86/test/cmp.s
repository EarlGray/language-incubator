.text
  cmpb $0x42, %al        # 3C 42
  cmpw $0x0fff, %ax      # 66 3D ff 0f
  cmpl $0x10000, %eax    # 3D 00 00 01 00

  cmpb $0x42, (%edi)                    # 80 3f 42
  cmpb $0x42, (%esp)                    # 80 3c 24 42
  cmpb $0x42, (%ebp)                    # 80 7d 00 42
  cmpb $0x42, 4(%esp)                   # 80 7c 24 04 42
  #cmpb (%esp), $0x42                   # invalid for GAS
  cmpw $0x0fff, (%esp)                  # 66 81 3c 24 ff 0f
  cmpl $0x0fffffff, 4(%esp)             # 81 7c 24 04 ff ff ff 0f
  cmpw $0x7fff, 0x80(%edi, %ebx, 4)     # 66 81 bc 9f 80 00 00 00 ff 7f
  cmpw $0x7fff, 0x33(%edi, %ebx, 4)     # 66 81 7c 9f 33 ff 7f
  cmpl $0x80a0c0e0, 0x80(%edi, %ebx, 4) # 81 bc 9f 80 00 00 00 e0 c0 a0 80

cmp_mem:
  cmpb 0xb8000, %al             #  3a 05 00 80 0b 00
  cmpb %al, 0xb8000             #  38 05 00 80 0b 00
  cmpw 0xb8000, %ax             #  66 3b 05 00 80 0b 00
  cmpl %eax, 0xb8000            #  39 05 00 80 0b 00
  cmpl 0xb8000, %eax            #  3b 05 00 80 0b 00
  #cmpl (%esp), 0x804800         #

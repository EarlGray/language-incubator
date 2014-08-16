.text
  cmpb $0x42, %al        # 3C 42
  cmpw $0x0fff, %ax      # 66 3D ff 0f
  cmpl $0x10000, %eax    # 3D 00 00 01 00

  cmpb $0x42, (%esp)     # 
  #cmpb (%esp), $0x42     # invalid for GAS

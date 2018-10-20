movl %eax, %eax                 #  89 c0                    -- 211 300
movl (%eax), %eax               #  8b 00                    -- 213 000
movl (%esp), %eax               #  8b 04 24                 -- 213 004 044
movl (%ebp), %eax               #  8b 45 00                 -- 213 105 000   ; movl 0x0(%ebp), %eax
movl 0xdeadbeef, %eax           #  a1 ef be ad de           -- 
movl 0x7f, %eax                 #  a1 7f 00 00 00
movl 4(%esi), %eax              #  8b 46 04                 -- 213 106 004
movl 4(%ebp), %eax              #  8b 45 04                 -- 213 105 004
movl 4(%esp), %eax              #  8b 44 24 04              -- 213 104 044 004
movl (,%eax), %eax              #  8b 04 05 00 00 00 00     -- 213 
movl (%ecx, %esi), %eax         #  8b 04 31
movl (%ebp, %esi), %eax         #  8b 44 35 00              -- 213 104 065 000 ; movl 0x0(%ebp, %esi,1), %eax
movl (%ecx, %esi, 8), %eax      #  8b 04 f1
movl (%ebp, %esi, 8), %eax      #  8b 44 f5 00
#movl (%ebp, %esp), %eax        -- invalid
movl 4(%ecx, %esi), %eax        #  8b 44 31 04              -- 213 104 061 4
movl 4(%ebp, %edi), %eax        #  8b 44 3d 04              -- 213 104 075 4
movl 4(,%esi), %eax             #  8b 04 35 04 00 00 00     -- 213 004 065 4
movl 4(,%esi,4), %eax           #  8b 04 b5 04 00 00 00     -- 213 004 265 4
movl 4(,%ebp,4), %eax           #  8b 04 ad 04 00 00 00     -- 213 004 255 4
movl 0xdeadbeef(%edi), %eax     #  8b 87 ef be ad de        -- 213 207 deadbeef
movl 0xdeadbeef(%esp), %eax     #  8b 84 24 ef be ad de     -- 213 204 044 deadbeef
movl 0xdeadbeef(,%edi,4), %eax  #  8b 04 bd ef be ad de     -- 213 004 275 deadbeef
movl 0xdeadbeef(%ebp,%edi,2), %eax    #  8b 84 7d ef be ad de -- 213 204 175 deadbeef

.byte 0x8b, 0x04, 0x25
.long 0xdeadbeef                #  movl 0xdeadbeef(,%eiz,1), %eax   -- gas

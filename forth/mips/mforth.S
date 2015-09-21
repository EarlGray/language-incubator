#include <syscall.h>

.set INITIAL_USERMEM_SIZE, (1024 * 512)
.set RETURN_STACK_SIZE, (1024 * 16)
.set IOBUFSZ,1024
.set WORDSZ,32

/*********************************************************/
.text

# $fp is the return stack pointer
.macro PUSH_RSP reg
	addiu $fp, -4
	sw    \reg, ($fp)
.endm
.macro POP_RSP reg
	lw    \reg, ($fp)
	addiu $fp, 4
.endm

# $s0 as "instruction pointer"
next_instruction:
	lw    $t0, ($s0)
	addiu $s0, 4
	lw    $t1, ($t0)         # read where is points, $t0 for DOCOL
	#move  $t0, $t0
	jr    $t1                # jump there

.macro NEXT
	move  $t0, $t0
	j next_instruction
.endm

# the interpreter function, DO COLON
# argument $t0 points to codeword
.p2align 2
DOCOL:
	PUSH_RSP $s0
	addiu $t0, 4
	move  $s0, $t0
	NEXT

/*********************************************************/
.section .rodata

.set F_IMMED,0x80
.set F_HIDDEN,0x20
.set F_LENMASK,0x1f		# length mask

.set link,0

#### DEFWORD
.macro DEFWORD name, namelen, flags=0, label
	.text
	.p2align 2
name_\label :
	.globl name_\label
	.int link
	.set link,name_\label
	.byte \flags+\namelen
	.ascii "\name"
	.p2align 2	# pad
\label :
	.globl \label
	.int DOCOL
.endm

#### DEFCODE
.macro DEFCODE name, namelen, flags=0, label
	.text
	.p2align 2
name_\label :
	.globl name_\label
	.int link
	.set link,name_\label
	.byte \flags+\namelen
	.ascii "\name"
	.p2align 2	# pad
\label :
	.globl \label
	.globl code_\label
	.int code_\label        # codeword
code_\label :
.endm

#### DEFVAR
.macro DEFVAR name, namelen, flags=0, label, initial=0
	DEFCODE \name,\namelen,\flags,\label
	la    $t0, var_\name
	addiu $sp, -4
	sw    $t0, ($sp)
	NEXT

	.data
	.p2align 2
var_\name :
	.int \initial
.endm

#### DEFCONST
.macro DEFCONST name, namelen, flags=0, label, value
	DEFCODE \name,\namelen,\flags,\label
	li    $t0, \value
	addiu $sp, -4
	sw    $t0, ($sp)
	NEXT
.endm

.macro DEFSYM name, namelen, flags=0, label, sym
	DEFCODE \name,\namelen,\flags,\label
	la    $t0, \sym
	addiu $sp, -4
	sw    $t0, ($sp)
	NEXT
.endm

##
##      Forth variables and constants
##

# is the interpreter executing code (0)
#    or compiling a word (~0)
DEFVAR "STATE",5,,STATE

# the next free byte of memory
DEFVAR "HERE",4,,HERE

# the most recently defined word
DEFVAR "LATEST",6,,LATEST,0

# top of the parameter stack
DEFVAR "S0",2,,SZ

# the current base for printing and reading numbers
DEFVAR "BASE",4,,BASE,10


DEFSYM "R0",2,,RZ,return_stack_top
DEFSYM "DOCOL",5,,__DOCOL,DOCOL

DEFCONST "VERSION",7,,VERSION,1
DEFCONST "F_IMMED",7,,__F_IMMED,F_IMMED
DEFCONST "F_HIDDEN",8,,__F_HIDDEN,F_HIDDEN
DEFCONST "F_LENMASK",9,,__F_LENMASK,F_LENMASK

DEFCONST "SYS_EXIT",8,,SYS_EXIT,__NR_exit
# DEFCONST "SYS_OPEN",9,,SYS_OPEN,__NR_open
DEFCONST "SYS_CLOSE",9,,SYS_CLOSE,__NR_close
DEFCONST "SYS_READ",8,,SYS_READ,__NR_read
DEFCONST "SYS_WRITE",9,,SYS_WRITE,__NR_write

##
##      Essential words
##

DEFCODE "EXIT",4,,EXIT
	POP_RSP $s0
	NEXT

DEFCODE "LIT",3,,LIT
	lw    $t0, ($s0)
	addiu $s0, 4
	addiu $sp, -4
	sw    $t0, ($sp)
	NEXT

DEFCODE "BYE",3,,BYE
	li    $a0, 0
	li    $v0, __NR_exit
	syscall
1:	j 1b

##
##		Stack manipulation
##
DEFCODE "DROP",4,,DROP
	lw    $t0, ($sp)
	addiu $sp, 4
	NEXT

DEFCODE "SWAP",4,,SWAP
	lw    $t0, ($sp)
	lw    $t1, 4($sp)
	sw    $t1, ($sp)
	sw    $t0, 4($sp)
	NEXT

DEFCODE "DUP",3,,DUP
	lw    $t0, ($sp)
	addiu $sp, -4
	sw    $t0, ($sp)
	NEXT

DEFCODE "OVER",4,,OVER
	addiu $sp, $sp, -4
	lw    $t0, 8($sp)      # get the second stack element,
	sw    $t0, ($sp)       # push on top
	NEXT

DEFCODE "ROT",3,,ROT
	lw    $t0, ($sp)       # before: a b c ...
	lw    $t1, 4($sp)
	lw    $t2, 8($sp)
	sw    $t0, 8($sp)      # after:  b c a ...
	sw    $t2, 4($sp)
	sw    $t1, ($sp)
	NEXT

DEFCODE "-ROT",4,,NROT
	lw    $t0,  ($sp)      # before: a b c ...
	lw    $t1, 4($sp)
	lw    $t2, 8($sp)
	sw    $t1, 8($sp)      # after:  c a b ...
	sw    $t0, 4($sp)
	sw    $t2,  ($sp)
	NEXT

DEFCODE "?DUP",4,,QDUP
	# duplicate if non-zero
	lw    $t0, ($sp)
	beqz  $t0, 1f
	addiu $sp, -4
	sw    $t0, ($sp)
1:
	NEXT

##
##		Return stack words
##
DEFCODE ">R",2,,TOR
	lw    $t0, ($sp)
	addiu $sp, 4           # pop from the param stack
	PUSH_RSP $t0           # push to the return stack
	NEXT

DEFCODE "R>",2,,FROMR
	POP_RSP $t0            # pop from the return stack
	addiu $sp, -4
	sw    $t0, ($sp)       # push to the param stack
	NEXT

DEFCODE "RSP@",4,,RSPFETCH
	addiu $sp, -4
	sw    $fp, ($sp)       # push RSP to the param stack
	NEXT

DEFCODE "RSP!",4,,RSPSTORE
	lw    $fp, ($sp)
	addiu $sp, 4           # pop RSP from the param stack
	NEXT

DEFCODE "RDROP",5,,RDROP
	addiu $fp, 4
	NEXT

##
##		Parameter stack manipulation
##
DEFCODE "DSP@",4,,DSPFETCH
	move  $t0, $sp
	addiu $sp, -4
	sw    $t0, ($sp)
	NEXT

DEFCODE "DSP!",4,,DSPSTORE
	lw    $sp, ($sp)
	NEXT

##
##		Memory access
##
DEFCODE "!",1,,STORE
	lw    $t1, ($sp)       # address to store at
	lw    $t0, 4($sp)      # data to store
	addiu $sp, 8
	sw    $t0, ($t1)
	NEXT

DEFCODE "@",1,,FETCH
	lw    $t1, ($sp)       # address to fetch
	lw    $t0, ($t1)
	sw    $t0, ($sp)
	NEXT

DEFCODE "+!",2,,ADDSTORE
	lw    $t1,  ($sp)       # address
	lw    $t0, 4($sp)       # addend
	addiu $sp, 8
	lw    $t2, ($t1)
	add   $t2, $t2, $t0
	sw    $t2, ($t1)
	NEXT

DEFCODE "C!",2,,STOREBYTE
	lw    $t1, ($sp)       # address to store at
	lw    $t0, 4($sp)      # data to store
	addiu $sp, 8
	sb    $t0, ($t1)
	NEXT

DEFCODE "C@",2,,FETCHBYTE
	lw    $t1, ($sp)       # address to fetch
	lb    $t1, ($t1)
	sw    $t1, ($sp)
	NEXT

DEFCODE "C@C!",4,,COPYBYTE
	# dst src ... -> dst+1 src+1 ... (writing a byte)
	lw    $t0,  ($sp)      # destination address
	lw    $t1, 4($sp)      # source address
	lb    $t2,  ($t1)      # get the byte to copy
	sb    $t2,  ($t0)      # put it
	addi  $t0, $t0, 1
	addi  $t1, $t1, 1
	sw    $t0,  ($sp)
	sw    $t1, 4($sp)
	NEXT

/*
DEFCODE "CMOVE",5,,CMOVE
	# len dst src ... -> ...  (copying a block)
	lw    $t0,  ($sp)
	lw    $t1, 4($sp)
	lw    $t2, 8($sp)
	addiu $sp, 12
	move  $t3, $0
	???
	NEXT
*/

##
##		Arithmetics
##
DEFCODE "1+",2,,INCR
	lw    $t0, ($sp)
	addi  $t0, $t0, 1
	sw    $t0, ($sp)
	NEXT

DEFCODE "1-",2,,DECR
	lw    $t0, ($sp)
	addi  $t0, $t0, -1
	sw    $t0, ($sp)
	NEXT

DEFCODE "4+",2,,INCR4
	lw    $t0, ($sp)
	addi  $t0, $t0, 4
	sw    $t0, ($sp)
	NEXT

DEFCODE "4-",2,,DECR4
	lw    $t0, ($sp)
	addi  $t0, $t0, -4
	sw    $t0, ($sp)
	NEXT

DEFCODE "+",1,,ADD
	lw    $t1, ($sp)
	addiu $sp, 4
	lw    $t0, ($sp)
	add   $t0, $t0, $t1
	sw    $t0, ($sp)
	NEXT

DEFCODE "-",1,,SUB
	lw    $t1, ($sp)
	addiu $sp, 4
	lw    $t0, ($sp)
	sub   $t0, $t0, $t1
	sw    $t0, ($sp)
	NEXT

DEFCODE "*",1,,MUL
	lw    $t1, ($sp)
	addiu $sp, 4
	lw    $t0, ($sp)
	mult  $t0, $t1
	mflo  $t0            # ignore overflow
	sw    $t0, ($sp)
	NEXT

DEFCODE "/MOD",4,,DIVMOD
	lw    $t1, ($sp)
	lw    $t0, 4($sp)
	div   $t0, $t1
	mfhi  $t1            # remainder
	mflo  $t0            # quotient
	sw    $t1, 4($sp)
	sw    $t0, ($sp)
	NEXT

DEFCODE "=",1,,EQU
	lw    $t1, ($sp)
	addiu $sp, 4
	lw    $t0, ($sp)
	seq   $t0, $t0, $t1
	sw    $t0, ($sp)
	NEXT

DEFCODE "<>",2,,NEQU
	lw    $t1, ($sp)
	addiu $sp, 4
	lw    $t0, ($sp)
	sne   $t0, $t0, $t1
	sw    $t0, ($sp)
	NEXT

DEFCODE "<",1,,LT
	lw    $t1, ($sp)
	addiu $sp, 4
	lw    $t0, ($sp)
	slt   $t0, $t0, $t1
	sw    $t0, ($sp)
	NEXT

DEFCODE ">",1,,GT
	lw    $t1, ($sp)
	addiu $sp, 4
	lw    $t0, ($sp)
	sgt   $t0, $t0, $t1
	sw    $t0, ($sp)
	NEXT

DEFCODE "<=",2,,LE
	lw    $t1, ($sp)
	addiu $sp, 4
	lw    $t0, ($sp)
	sle   $t0, $t0, $t1
	sw    $t0, ($sp)
	NEXT

DEFCODE "=>",2,,GE
	lw    $t1, ($sp)
	addiu $sp, 4
	lw    $t0, ($sp)
	sge   $t0, $t0, $t1
	sw    $t0, ($sp)
	NEXT

DEFCODE "0=",2,,ZEQU
	lw    $t0, ($sp)
	seq		$t0, $t0, $0
	sw    $t0, ($sp)
	NEXT

DEFCODE "0<>",3,,ZNEQU
	lw    $t0, ($sp)
	sne		$t0, $t0, $0
	sw    $t0, ($sp)
	NEXT

DEFCODE "0<",2,,ZLT
	lw    $t0, ($sp)
	slt		$t0, $t0, $0
	sw    $t0, ($sp)
	NEXT

DEFCODE "0>",2,,ZGT
	lw    $t0, ($sp)
	sgt		$t0, $t0, $0
	sw    $t0, ($sp)
	NEXT

DEFCODE "AND",3,,AND
	lw    $t1, ($sp)
	addiu $sp, 4
	lw    $t0, ($sp)
	and   $t0, $t0, $t1
	sw    $t0, ($sp)
	NEXT

DEFCODE "OR",2,,OR
	lw    $t1, ($sp)
	addiu $sp, 4
	lw    $t0, ($sp)
	or    $t0, $t0, $t1
	sw    $t0, ($sp)
	NEXT

DEFCODE "XOR",3,,XOR
	lw    $t1, ($sp)
	addiu $sp, 4
	lw    $t0, ($sp)
	xor   $t0, $t0, $t1
	sw    $t0, ($sp)
	NEXT

DEFCODE "INVERT",6,,INVERT
	lw    $t0, ($sp)
	not   $t0, $t0
	sw    $t0, ($sp)
	NEXT


##
##		Input/output
##

#### DATA
.data

#### RODATA
.section .rodata

err_parse_msg:
	.asciiz "PARSE ERROR: "
.equ ERR_PARSE_LEN, (. - err_parse_msg)

err_msg:
	.asciiz "FATAL ERROR!"
.equ ERR_MSG_LEN, (. - err_msg)

nl_msg: .ascii "\n"

.text

# no arguments, returns a read byte in $v0
next_key:
	# is there any data to read?
	lw    $t0, (iobufcur)   # iobufcur
	lw    $t1, (iobufend)   # iobufend
	bge   $t0, $t1, 1f      # if no data, jump to 1:

	# fetch a byte from iobuf
	lb    $v0, ($t0)        # a byte of input; the return value
	addi  $t0, $t0, 1
	sw    $t0, (iobufcur)   # increment `iobufcur`
	jr    $ra

1: # no data in the iobuf, read() more
	la    $s1, iobuf
	sw    $s1, (iobufcur)   # iobufcur = iobuf

	move  $a0, $0           # arg[0] = FILENO_STDIN;
	move  $a1, $s1          # arg[1] = iobuf;
	li    $a2, IOBUFSZ      # arg[2] = IOBUFSZ;
	li    $v0, __NR_read
	syscall
	bnez  $a3, 2f           # syscall failed

	addu  $s1, $s1, $v0     # iobuf + nread
	sw    $s1, (iobufend)
	j next_key              # tail-rec. self-call

2: # error or end of input; bail out
	move  $a0, $0
	li    $v0, __NR_exit
	syscall


# no arguments, returns word_addr ($v0), word_len ($v1)
next_word:
	addiu $sp, -4
	sw    $ra, ($sp)        # $ra will be used by `next_key`

1:  # search for a non-blank
	jal next_key
	beq   $v0, '\\', 4f
	beq   $v0, ' ' , 1b
	beq   $v0, '\n', 1b
	beq   $v0, '\t', 1b

	move  $s2, $0           # how many bytes read
2:  # add to `wordbuf`
	sb    $v0, wordbuf($s2)
	addiu $s2, 1
	jal   next_key
	beq   $v0, ' ' , 3f
	beq   $v0, '\t', 3f
	beq   $v0, '\n', 3f
	j 2b

3:  # end of word
	la    $v0, wordbuf
	move  $v1, $s2
	lw    $ra, ($sp)
	addiu $sp, 4
	jr    $ra

4:  # skip the comment
	jal   next_key
	beq   $v0, '\n', 1b
	j 4b


# takes a character ($a3), base ($a2)
# returns ($t7): -1 if is not valid digit, number otherwise
# clobbers: $t9, $t8
digit_in_base:
	blt   $a3, '0', $Lerr    # number can't be below '0'
	bgt   $a2, 10, 2f        # base is more than 10?
	addiu $t9, $a2, '0'      # t9 <- upper limit
	bge   $a3, $t9, $Lerr    # number can't be above '0'+base

$L1:
	addi  $t7, $a3, -'0'     # $t7 : the return value
	jr    $ra

2:  # base is more than 10
	bgt   $a2, 36, $Lerr     # base must be <= 36
	addiu $t8, $a2, -10      # base -= 10
	ble   $a3, '9', $L1      # is inside decimal range
	addi  $t7, $a3, -'A'     # v2 = c - 'A'
	bltz  $t7, $Lerr         # less than 'A'?
	bge   $t7, $t8, 4f       # not in 'A'..'A'+base-10 range
	addiu $t7, 10
	jr    $ra

4:  blt   $a3, 'a', $Lerr    # cannot be less than 'a' here
	addi  $t7, $a3, -'a'     # t1 = c - 'a'
	bge   $t7, $t8, $Lerr    # above 'a'+base-10 range
	addiu $t7, 10
	jr    $ra

$Lerr:  # failed
	li    $t7, -1
	jr    $ra


# takes a str_addr ($a0), str_len ($a1)
# returns a number ($v0), number of unconverted syms ($v1)
parse_number:
	addiu $sp, -4
	sw    $ra, ($sp)
	move  $v1, $a1          # n_unconv = slen

	lw    $a2, (var_BASE)   # a2 = base
	move  $v0, $0           # res = 0
	move  $t6, $0           # negative = false

	lb    $a3, ($a0)
	beq   $a3, '-', 1f
	beq   $a3, '+', 2f
	j     3f
1:	li    $t6, 1            # negative = true
2:  addi  $a0, $a0, 1
	addi  $v1, $v1, -1
3:
	beqz  $v1, 5f           # a zero-length string returns 0

	lb    $a3, ($a0)
	jal   digit_in_base
	bltz  $t7, 5f

	mult  $a2, $v0
	mflo  $v0
	add   $v0, $v0, $t7
	addi  $v1, $v1, -1
	addi  $a0, $a0, 1
	j 3b

5:
	beqz  $t6, 6f
	neg   $v0
6:
	lw    $ra, ($sp)
	addiu $sp, 4
	jr    $ra

# $a0 contains address of a one-byte output buffer
emit_byte:
	li    $a2, 1            # arg[2] = 1 (byte)
	move  $a1, $a0          # arg[1] = &outbuf
	li    $a0, 1            # arg[0] = FILENO_STROUT
	li    $v0, __NR_write
	syscall
	jr $ra

find_word:
# takes str_addr ($a0), str_len ($a1)
# return dict entry in $v0 (maybe NULL)
	lw    $t0, var_LATEST   # curr_item = LATEST
$Llookup:
	beqz  $t0, $Ldone       # NULL, fail
	# compare length
	lb    $t1, 4($t0)       # load len/flags
	andi  $t1, $t1, (F_HIDDEN | F_LENMASK)
	bne   $t1, $a1, $Lnext  # will fail if HIDDEN
	# compare the string
	move  $t9, $a1          # len
	move  $t8, $a0          # str
	addiu $t7, $t0, 5       # link+5
$Lsymcmp:
	beqz  $t9, $Ldone       # if (!len) goto $Ldone
	lb    $t5, ($t7)
	lb    $t6, ($t8)
	bne   $t5, $t6, $Lnext  # if wrd[0] != src[0] goto $Lnext
	addi  $t7, $t7, 1       # wrd++
	addi  $t8, $t8, 1       # str++
	addi  $t9, $t9, -1      # --len
	j $Lsymcmp
$Lnext: # move to the next word
	lw    $t0, ($t0)
	j $Llookup
$Ldone:
	move  $v0, $t0
	jr    $ra

codeword_addr:
# takes a dict entry pointer ($a0)
# returns its codeword pointer ($v0)
	addi  $a0, $a0, 4
	lb    $t0, ($a0)
	andi  $t0, $t0, F_LENMASK
	addi  $t0, $t0, 1
	# pad to 4
	andi  $t2, $t0, 3
	slt   $t2, $0, $t2      # t2 = 0 < t2 ? 1 : 0
	srl   $t0, $t0, 2       # t0 >>= 2
	add   $t0, $t0, $t2     # t0 += t2
	sll   $t0, $t0, 2       # t0 <<= 2
	add   $v0, $a0, $t0
	jr    $ra

DEFCODE "KEY",3,,KEY
	jal next_key
	addiu $sp, $sp, -4
	sw    $v0, ($sp)
	NEXT

DEFCODE "EMIT",4,,EMIT
	move  $a0, $sp
	jal emit_byte
	addiu $sp, $sp, 4
	NEXT

DEFCODE "WORD",4,,WORD
	# reads a word, skipping whitespaces
	# ... -> len addr ...
	jal   next_word
	addiu $sp, $sp, -8
	sw    $v0, 4($sp)       # str_addr
	sw    $v1,  ($sp)       # str_len
	NEXT

DEFCODE "NUMBER",6,,NUMBER
	# parses a number from string
	#  str_len str_addr ... -> num ...
	lw    $a1,  ($sp)       # str_len
	lw    $a0, 4($sp)       # str_addr
	jal   parse_number
	sw    $v1,  ($sp)       # unconverted characters
	sw    $v0, 4($sp)       # the number
	NEXT

DEFCODE "FIND",4,,FIND
	lw    $a1,  ($sp)       # len
	lw    $a0, 4($sp)       # addr
	jal   find_word
	addiu $sp, 4
	lw    $v0, ($sp)
	NEXT

DEFCODE ">CFA",4,,TCFA
	# "Code Field Address"?
	# link_ptr ... -> interp_ptr ...
	lw    $a0, ($sp)
	jal   codeword_addr
	sw    $v0, ($sp)
	NEXT

DEFWORD ">DFA",4,,TDFA
	# link_ptr to the first instruction pointer
	.int TCFA
	.int INCR4
	.int EXIT

DEFCODE "CREATE",6,,CREATE
	# writes the header of a new word:
	# len name ... -> ...
	lw    $t0,  ($sp)       # len
	lw    $t1, 4($sp)       # name
	addiu $sp, 8
	lw    $t2, (var_HERE)
	lw    $t3, (var_LATEST)
	sw    $t3, ($t2)        # write old LATEST at HERE
	sw    $t2, (var_LATEST)
	sb    $t0, 4($t2)       # write len at HERE
	addiu $t2, 5
	# copy the word
$Lbytecpy:
	beqz  $t0, $Lcopied     #

	addi  $t0, $t0, -1
	addi  $t1, $t1, 1
	addi  $t2, $t2, 1       # ++HERE
	j $Lbytecpy
$Lcopied: # now pad to 4
	addi  $t2, $t2, 3
	srl   $t2, $t2, 2
	sll   $t2, $t2, 2
	sw    $t2, (var_HERE)
	NEXT

.macro PUTHERE reg
	# *HERE++ = val;
	lw    $t2, (var_HERE)
	sw    \reg, ($t2)
	addiu $t2, 4
	sw    $t2, (var_HERE)
.endm

DEFCODE ",",1,,COMMA
	lw    $t0, ($sp)
	addiu $sp, 4
	PUTHERE $t0
	NEXT

DEFCODE "[",1,F_IMMED,LBRAC
	# always executed, even in compile mode!
	# STATE = 0, switch to immediate mode
	sw    $0, (var_STATE)
	NEXT

DEFCODE "]",1,F_IMMED,RBRAC
	# always executed, even in compile mode!
	# STATE = 1, switch to compile mode
	li    $t0, 1
	sw    $t0, (var_STATE)
	NEXT

DEFCODE "HIDDEN",6,,HIDDEN
	# dict_entry ... -> ... toggling HIDDEN bit
	lw    $t0, ($sp)
	addiu $sp, 4
	lb    $t1, 4($t0)
	xori  $t1, $t1, F_HIDDEN # toggle
	sb    $t1, 4($t0)
	NEXT

DEFWORD "HIDE",4,,HIDE
	.int WORD               # read a word after HIDE
	.int FIND               # get its dict entry
	.int HIDDEN
	.int EXIT

DEFCODE "IMMED",5,,IMMED
	# dict_entry ... -> ... making the LATEST word immediate
	lb    $t1, (var_LATEST + 4)
	xori  $t1, $t1, F_IMMED
	sb    $t1, (var_LATEST + 4)
	NEXT

DEFWORD ":",1,,COLON
	.int WORD               # read a word
	.int CREATE             # write its header into the dictionary
	.int LIT, DOCOL, COMMA  # append DOCOL
	.int LATEST, FETCH, HIDDEN  # make the word hidden (until compilation ends)
	.int RBRAC              # go to compile mode
	.int EXIT               # compile the definiton

DEFWORD ";",1,F_IMMED,SEMICOLON
	.int LIT, EXIT, COMMA   # append EXIT
	.int LATEST, FETCH, HIDDEN  # toggle HIDDEN back
	.int LBRAC              # go back to immediate mode
	.int EXIT

DEFCODE "'",1,,TICK
	# fetches codeword of the word on the next word
	# typical usage: ' FOO , -- append FOO codeword
	# e.g. :LIT2 IMMED  ' LIT , 2 , ;
	#   -- LIT2 inside a definition appends LIT 2
	lw    $t0, ($s0)
	addiu $s0, 4
	addiu $sp, -4
	sw    $t0, ($sp)
	NEXT

DEFCODE "BRANCH",6,,BRANCH
	# BRANCH <n> skips n words
	lw    $t0, ($s0)
	add   $s0, $s0, $t0
	NEXT

DEFCODE "0BRANCH",7,,ZBRANCH
	# 0BRANCH <n> skips n words if the stack top is 0
	lw    $t0, ($sp)
	addiu $sp, 4
	beqz  $t0, code_BRANCH
	addiu $s0, 4            # skip the offset otherwise
	NEXT

DEFCODE "LITSTRING",9,,LITSTRING
	# LITSTRING <len> <str...>
	# takes a string literal in a compiled word
	# puts <len> <addr> on the stack
	lw    $t0, ($s0)        # take the length
	addiu $s0, 4            # s0 is now the literal address
	addiu $sp, -8
	sw    $s0, 4($sp)
	sw    $t0,  ($sp)
	# skip the string
	add   $s0, $s0, $t0
	# pad to 4
	addi  $s0, $s0, 3
	srl   $s0, $s0, 2
	sll   $s0, $s0, 2      # ??? andi  $s0, $s0, ~3
	NEXT

DEFCODE "TELL",4,,TELL
	# write to stdout a string on the stack
	# len addr ... -> ...
	li    $a0, 1            # FILENO_STDOUT
	lw    $a1, 4($sp)       # str_addr
	lw    $a2,  ($sp)       # str_len
	li    $v0, __NR_write
	syscall
	addiu $sp, 8
	NEXT

/*
 *	Jonesforth is pretty cryptic here.
 *	First, we have to find_word next_word to see if it's defined.
 *	If it is not found, assume it to be a literal, try to parse as number;
 *
 *	Interpret the results:
 *	If it is a word:
 *		if STATE is IMMED or it is F_IMMED:
 *			jump to its codeword;
 *		if STATE is COMPILE:
 *			append its codeword;
 *	If is a literal:
 *		if STATE is IMMED: push it on the stack;
 *		if STATE is COMPILE: append LIT, append it;
 */

parse_token:
	move  $s7, $ra
	move  $s2, $v0
	move  $s3, $v1

	move  $a0, $s2
	move  $a1, $s3
	jal   find_word
	# now $v0 may be a dict entry

	bnez  $v0, $Lexit
	# no, it's not; assume it's a literal number
	move  $a0, $s2
	move  $a1, $s3
	jal   parse_number
	bnez  $v1, $Lparse_error
	move  $v1, $v0
	move  $v0, $0

$Lexit:
	jr    $s7
	# returns:
	# 	token (addr in $s2, len in $s3),
	#   either a word: dict entry in $v0
	# 	or  a literal: zero in $v0, literal in $v1

$Lparse_error:
	# not a known word or number
	li    $a0, 2            # arg[0] = FILENO_STDERR
	la    $a1, err_parse_msg
	li    $a2, ERR_PARSE_LEN
	li    $v0, __NR_write
	syscall

	li    $a0, 2
	move  $a1, $s2
	move  $a2, $s3
	li    $v0, __NR_write
	syscall

	li    $a0, 2
	la    $a1, nl_msg
	li    $a2, 1
	li    $v0, __NR_write
	syscall
	NEXT

DEFCODE "INTERPRET",9,,INTERPRET
	jal   next_word
	jal   parse_token
	beqz  $v0, interpret_lit

interpret_word:
	lw    $t1, (var_STATE)  # 0/1: EXEC/COMPILE
	lb    $t0, 4($v0)
	andi  $t0, $t0, F_IMMED

	beqz  $t1, $Lexec_word  # if STATE == EXEC goto $Lexec_word
	bnez  $t0, $Lexec_word  # if IMMED goto $Lexec_word
$Lcompile_word:
	PUTHERE $v0
	NEXT
$Lexec_word:
	move  $a0, $v0
	jal   codeword_addr
	lw    $v0, ($v0)
	jr    $v0

interpret_lit:
	lw    $t1, (var_STATE)
	bnez  $t1, $Lcompile_lit
$Lexec_lit:
	# execute a literal: push it on the stack
	addiu $sp, -4
	sw    $v1, ($sp)
	NEXT
$Lcompile_lit:
	# compile a literal:
	lw    $t2, (var_HERE)
	la    $t0, LIT
	sw    $t0,  ($t2)
	sw    $v1, 4($t2)
	addiu $t2, 8
	sw    $t2, (var_HERE)
	NEXT


DEFWORD "QUIT",4,,QUIT
	.int RZ, RSPSTORE       # R0 RSP!, clear the return stack
	.int INTERPRET
	.int BRANCH, -8         # loop back indefinitely


##
##	The entry point
##
/*********************************************************/
.section .rodata
cold_start:
	.int QUIT

/*********************************************************/
.data

.align 4
iobuf:    .space (IOBUFSZ + 1)

.align 4
wordbuf:  .space WORDSZ

.align 4
iobufend: .int iobuf
iobufcur: .int iobuf

/*********************************************************/
.bss
return_stack:
.space RETURN_STACK_SIZE
return_stack_top:

user_memory:
.space INITIAL_USERMEM_SIZE

/*********************************************************/
.text
.globl _start, __start

__start:
_start:
	la    $t0, link
	sw    $t0, var_LATEST

	sw    $sp, var_S0
	la    $fp, return_stack_top

	la    $t0, user_memory
	sw    $t0, var_HERE

	la    $s0, cold_start
	NEXT

syscall_failed:
	li    $a0, 128
	li    $v0, __NR_exit
	syscall

	li    $a0, 1
	la    $a1, err_msg
	li    $a2, ERR_MSG_LEN
	li    $v0, __NR_write
	syscall
1:
	j 1b

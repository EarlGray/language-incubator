#include <asm-generic/unistd.h>

/*********************************************************/
.text

# $fp is the return stack pointer
.macro PUSH_RSP reg
	addiu $fp, $fp, -4
	sw    \reg, ($fp)
.endm
.macro POP_RSP reg
	lw    \reg, ($fp)
	addiu $fp, $fp, 4
.endm

# $s0 as "instruction pointer"
next_intruction:
	lw    $t0, ($s0)
	addiu $s0, 4
	lw    $t0, ($t0)	 # read CodeWord
	lw    $t0, ($t0)         # read where is points
	jr    $t0                # jump there

.macro NEXT
	j next_intruction
.endm

# the interpreter function, DO COLON
# argument $t0 points to codeword
.align 4
DOCOL:
	PUSH_RSP $s0
	addiu $t0, $t0, 4
	move  $s0, $t0
	NEXT

_start:
	sw    $sp, var_S0
	la    $fp, return_stack_top
	la    $s0, cold_start
	NEXT

/*********************************************************/
.data

return_stack_top:
	.space (1024 * 1024 * 4)

/*********************************************************/
.section .rodata

.set F_IMMED,0x80
.set F_HIDDEN,0x20
.set F_LENMASK,0x1f		# length mask

.set link,0

#### DEFWORD
.macro DEFWORD name, namelen, flags=0, label
	.section .rodata
	.align 4
name_\label :
	.globl name_\label
	.int link
	.set link,name_\label
	.byte \flags+\namelen
	.ascii "\name"
	.align 4	# pad to 4
	.globl \label
\label :
	.int DOCOL
.endm

#### DEFCODE
.macro DEFCODE name, namelen, flags=0, label
	.section .rodata
	.align 4
name_\label :
	.globl name_\label
	.int link
	.set link,name_\label
	.byte \flags+\namelen
	.ascii "\name"
	.align 4 		# pad
\label :
	.globl \label
	.globl code_\label
	.int code_\label        # codeword

	.text
	.align 4
code_\label :
.endm

#### DEFVAR
.macro DEFVAR name, namelen, flags=0, label, initial=0
	DEFCODE \name,\namelen,\flags,\label
	la    $t0, var_\name
	addiu $sp, $sp, -4
	sw    $t0, ($sp)
	NEXT

	.data
	.align 4
var_\name :
	.int \initial
.endm

#### DEFCONST
.macro DEFCONST name, namelen, flags=0, label, value
	DEFCODE \name,\namelen,\flags,\label
	li    $t0, \value
	addiu $sp, $sp, -4
	sw    $t0, ($sp)
	NEXT
.endm

.macro DEFSYM name, namelen, flags=0, label, sym
	DEFCODE \name,\namelen,\flags,\label
	la    $t0, \sym
	addiu $sp, $sp, -4
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
DEFVAR "LATEST",6,,LATEST,name_SYSCALL0

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
	addiu $s0, $s0, 4
	addiu $sp, $sp, -4
	sw    $t0, ($sp)
	NEXT
	
##
##		Stack manipulation
## 
DEFCODE "DROP",4,,DROP
	lw    $t0, ($sp)
	addiu $sp, $sp, 4
	NEXT

DEFCODE "SWAP",4,,SWAP
	lw    $t0, ($sp)
	lw    $t1, 4($sp)
	sw    $t1, ($sp)
	sw    $t0, 4($sp)
	NEXT

DEFCODE "DUP",3,,DUP
	lw    $t0, ($sp)
	addiu $sp, $sp, -4
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
	addiu $sp, $sp, -4
	sw    $t0, ($sp)
1:
	NEXT

##
##		Return stack words
## 
DEFCODE ">R",2,,TOR
	lw    $t0, ($sp)
	addiu $sp, $sp, 4      # pop from the param stack
	PUSH_RSP $t0           # push to the return stack
	NEXT

DEFCODE "R>",2,,FROMR
	POP_RSP $t0            # pop from the return stack
	addiu $sp, $sp, -4
	sw    $t0, ($sp)       # push to the param stack
	NEXT

DEFCODE "RSP@",4,,RSPFETCH
	addiu $sp, $sp, -4
	sw    $fp, ($sp)       # push RSP to the param stack
	NEXT

DEFCODE "RSP!",4,,RSPSTORE
	lw    $fp, ($sp)
	addiu $sp, $sp, 4      # pop RSP from the param stack
	NEXT

DEFCODE "RDROP",5,,RDROP
	addiu $fp, $fp, 4
	NEXT

##
##		Parameter stack manipulation
##
DEFCODE "DSP@",4,,DSPFETCH
	move  $t0, $sp
	addiu $sp, $sp, -4
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
	addiu $sp, $sp, 8
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
	addiu $sp, $sp, 8
	lw    $t2, ($t1)
	add   $t2, $t2, $t0
	sw    $t2, ($t1)
	NEXT

DEFCODE "C!",2,,STOREBYTE
	lw    $t1, ($sp)       # address to store at
	lw    $t0, 4($sp)      # data to store
	addiu $sp, $sp, 8
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
	addiu $sp, $sp, 12
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
	addiu $sp, $sp, 4
	lw    $t0, ($sp)
	add   $t0, $t0, $t1
	sw    $t0, ($sp)
	NEXT

DEFCODE "-",1,,SUB
	lw    $t1, ($sp)
	addiu $sp, $sp, 4
	lw    $t0, ($sp)
	sub   $t0, $t0, $t1
	sw    $t0, ($sp)
	NEXT

DEFCODE "*",1,,MUL
	lw    $t1, ($sp)
	addiu $sp, $sp, 4
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
	addiu $sp, $sp, 4
	lw    $t0, ($sp)
	seq   $t0, $t0, $t1
	sw    $t0, ($sp)
	NEXT

DEFCODE "<>",2,,NEQU
	lw    $t1, ($sp)
	addiu $sp, $sp, 4
	lw    $t0, ($sp)
	sne   $t0, $t0, $t1
	sw    $t0, ($sp)
	NEXT

DEFCODE "<",1,,LT
	lw    $t1, ($sp)
	addiu $sp, $sp, 4
	lw    $t0, ($sp)
	slt   $t0, $t0, $t1
	sw    $t0, ($sp)
	NEXT

DEFCODE ">",1,,GT
	lw    $t1, ($sp)
	addiu $sp, $sp, 4
	lw    $t0, ($sp)
	sgt   $t0, $t0, $t1
	sw    $t0, ($sp)
	NEXT

DEFCODE "<=",2,,LE
	lw    $t1, ($sp)
	addiu $sp, $sp, 4
	lw    $t0, ($sp)
	sle   $t0, $t0, $t1
	sw    $t0, ($sp)
	NEXT

DEFCODE "=>",2,,GE
	lw    $t1, ($sp)
	addiu $sp, $sp, 4
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
	addiu $sp, $sp, 4
	lw    $t0, ($sp)
	and   $t0, $t0, $t1
	sw    $t0, ($sp)
	NEXT

DEFCODE "OR",2,,OR
	lw    $t1, ($sp)
	addiu $sp, $sp, 4
	lw    $t0, ($sp)
	or    $t0, $t0, $t1
	sw    $t0, ($sp)
	NEXT

DEFCODE "XOR",3,,XOR
	lw    $t1, ($sp)
	addiu $sp, $sp, 4
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

.data

.align 4
.set IOBUFSZ,1024
iobuf:    .space (IOBUFSZ + 1)

.align 4
.set WORDSZ,32
wordbuf:  .space WORDSZ

.align 4
iobufend: .int iobuf
iobufcur: .int iobuf

.text

# no arguments, returns a read byte in $v0
next_key:
	# is there any data to read?
	la    $t0, iobufcur     # &iobufcur
	la    $t1, iobufend     # &iobufend
	bge   $t0, $t1, 1f      # if no data, jump to 1:

	# fetch a byte from iobuf
	lw    $t2, ($t0)        # the pointer in `iobufcur`
	lb    $v0, ($t2)        # a byte of input; the return value
	addi  $t2, $t2, 1
	sw    $t2, ($t0)        # increment `iobufcur`
	jr    $ra

1: # no data in the iobuf, read() more
	la    $s1, iobuf
	sw    $s1, ($t0)        # iobufcur = iobuf
	
	move  $a0, $0           # arg[0] = FILENO_STDIN;
	move  $a1, $s1          # arg[1] = iobuf;
	li    $a2, IOBUFSZ      # arg[2] = IOBUFSZ;
	li    $v0, __NR_read
	syscall
	bnez  $a3, 2f           # syscall failed

	addu  $s1, $s1, $v0     # iobuf + nread
	sw    $s1, iobufend($0)
	j next_key              # tail-rec. self-call

2: # error or end of input; bail out
	move  $a0, $0
	li    $v0, __NR_exit
	syscall


# no arguments, returns word_addr ($v0), word_len ($v1)
next_word:
	addiu $sp, $sp, -4
	sw    $ra, ($sp)        # $ra will be used by `next_key`

	move  $s2, $0           # how many bytes read
1:  # search for a non-blank
	jal next_key
	beq   $v0, '\\', 4f
	beq   $v0, ' ' , 1b
	beq   $v0, '\n', 1b
	beq   $v0, '\t', 1b

2:  # add to `wordbuf`
	sb    $v0, ($s2)
	addiu $s2, $s2, 1
	jal next_key
	beq   $v0, ' ' , 3f
	beq   $v0, '\t', 3f
	beq   $v0, '\n', 3f
	j 2b

3:  # end of word
	la    $v0, wordbuf
	move  $v1, $s2
	lw    $ra, ($sp)
	addiu $sp, $sp, 4
	jr    $ra

4:  # skip the comment
	jal   next_key
	beq   $v0, '\n', 1b
	j 4b


# $a0 contains address of a one-byte output buffer
emit_byte:
  li    $a2, 1            # arg[2] = 1 (byte)
	move  $a1, $a0          # arg[1] = &outbuf
	li    $a0, 1            # arg[0] = FILENO_STROUT
	li    $v0, __NR_write
	syscall
	jr $ra

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

/*
DEFCODE "NUMBER",6,,NUMBER
	# parses a number from string
	#  str_len str_addr ... -> num ...

	NEXT
*/

.section .rodata
cold_start:
.int QUIT


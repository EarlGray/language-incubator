variable head                    \ data index in bfmem[]
variable cmdptr                  \ instruction pointer
variable bfmem 30000 chars allot \ the machine memory

: =cmd     ( -- )  cmdptr @ c@ = ;

: cmdnext  ( -- )  cmdptr @ 1+ cmdptr ! ;
: cmdprev  ( -- )  cmdptr @ 1- cmdptr ! ;
: headaddr ( -- )  bfmem head @ + ;

: begin-loop ( -- )
    1 ( nested )
    begin
      cmdnext
      [CHAR] ] =cmd if 1- then
      [CHAR] [ =cmd if 1+ then
    dup 0=  [CHAR] ] =cmd  and until
    drop
;

: end-loop ( -- )
    1 ( nested )
    begin
      cmdprev
      [CHAR] ] =cmd if 1+ then
      [CHAR] [ =cmd if 1- then
    dup 0=  [CHAR] [ =cmd  and until
    drop
;

: bfinc ( -- )  headaddr dup c@ 1+ swap c! ;
: bfdec ( -- )  headaddr dup c@ 1- swap c! ;
: bfin ( -- )   key headaddr c! ;
: bfout ( -- )  headaddr c@ emit ;
: bfback ( -- ) head @ 1- head ! ;
: bffwd ( -- )  head @ 1+ head ! ;
: bfjb  ( -- )  headaddr c@ 0= if begin-loop then ;
: bfje  ( -- )  headaddr c@ if    end-loop   then ;

: bfdebug ( -- ) ( cmdptr @ c@ emit  space head ?  space headaddr c@ .  space cmdptr ?  cr ) ;

: bfrun ( prog_addr -- )
    cmdptr !
    0 head !

    begin 
      bfdebug
      [CHAR] + =cmd if bfinc  then
      [CHAR] - =cmd if bfdec  then
      [CHAR] > =cmd if bffwd  then
      [CHAR] < =cmd if bfback then
      [CHAR] , =cmd if bfin   then
      [CHAR] . =cmd if bfout  then
      [CHAR] [ =cmd if bfjb   then
      [CHAR] ] =cmd if bfje   then
    0 =cmd 0= while
      cmdnext
    repeat   
;

\ variable slen
\ variable buff 512 chars allot
\ buff 512 stdin  read-line  drop slen !

." ~~~~ Welcome to BrainF*rth ~~~~" cr

\ BF helloworld:
S" ++++++++++[>+++++++>++++++++++>+++>+<<<<-] >++.>+.+++++++..+++.>++.<<+++++++++++++++.  >.+++.------.--------.>+.>. " drop
bfrun
bye


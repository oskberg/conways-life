CREATE SEED  123475686 ,

: Rnd ( n -- rnd )   { Returns single random number less than n }
   SEED              { Minimal version of SwiftForth Rnd.f      }
   DUP >R            { Algorithm Rick VanNorman  rvn@forth.com  }
   @ 127773 /MOD 
   2836 * SWAP 16807 * 
   2DUP > IF - 
   ELSE - 2147483647 +  
   THEN  DUP R> !
   SWAP MOD ;
  
: CLEAR-STACK 0
BEGIN
    drop
    depth 0 =
UNTIL ;

: CREATE-ARRAY-100
100 ALLOCATE drop ;

: SHOW-ARRAY-100 CR
100 0 DO
    I 10 mod 0= IF
        CR
    THEN dup I + c@ 4 .R
LOOP CR ;

: ARRAY-@ { ARRAY-LOCATION X Y -- }
10 * + + c@ ;

: ARRAY-! { n1 ARRAY-LOCATION X Y -- }
10 * + + c! ;

: LINEAR-SMALL-ARRAY { ARRAY-LOCATION -- }
100 0 DO 
    dup I + I swap c!
LOOP ;

{ N -- }
: CREATE-N-BY-N 
dup * ALLOCATE drop ;

{ WIDTH HEIGH -- }
: CREATE-X-BY-Y 
* ALLOCATE drop ;

{ LOCATION HEIGHT WIDTH -- }
: SHOW-ARRAY-Y-X
dup rot * 0 DO
    dup I swap mod 0= IF
        CR
    THEN over I + c@ 3 .R
LOOP ;

: ARRAY-W-@ { ARRAY-LOCATION X Y WIDTH -- }
* + + c@ ;

: ARRAY-W-! { n1 ARRAY-LOCATION X Y WIDHT -- }
* + + c! ;

{ ARRAY-LOCATION TOTAL-SIZE }
{ USES THE RND FUCTION FROM GRAPHICS_TEST.F }
{ Should porbably add a c to the !}
: FILL-RND 
0 DO
    2 rnd over I + ! 
LOOP ;

{ returns 1 if alive and 0 if dead }
: LIFE-RULE ( status neighbours -- )
    CASE 
        0 OF drop 0 ENDOF 
        1 OF drop 0 ENDOF 
        2 OF 1 = abs ENDOF   { if current status = 0 return 0, else abs( -1 ). Could also do nothing }
        3 OF drop 1 ENDOF 
        4 OF drop 0 ENDOF 
        5 OF drop 0 ENDOF 
        6 OF drop 0 ENDOF 
        7 OF drop 0 ENDOF 
        8 OF drop 0 ENDOF 
    ENDCASE ;
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
    THEN over I + c@ .
LOOP ;

: ARRAY-W-@ { ARRAY-LOCATION X Y WIDTH -- }
* + + c@ ;

: ARRAY-W-! { n1 ARRAY-LOCATION X Y WIDHT -- }
* + + c! ;

{ ARRAY-LOCATION TOTAL-SIZE }
{ USES THE RND FUCTION FROM GRAPHICS_TEST.F }
: FILL-RND 
0 DO
    2 rnd over I + ! 
LOOP ;
CREATE SEED  123456789 ,

: Rnd ( n -- rnd )   { Returns single random number less than n }
   SEED              { Minimal version of SwiftForth Rnd.f      }
   DUP >R            { Algorithm Rick VanNorman  rvn@forth.com  }
   @ 127773 /MOD 
   2836 * SWAP 16807 * 
   2DUP > IF - 
   ELSE - 2147483647 +  
   THEN  DUP R> !
   SWAP MOD 
;

( empties the stack )
: CLEAR-STACK ( -- )
0
BEGIN
    drop
    depth 0 =
UNTIL 
;

: create-x-by-y 
    * ALLOCATE drop
;

( creates an array size 100 )
: CREATE-ARRAY-100 ( -- )
100 ALLOCATE drop 
;

( Prints the array into the console )
: SHOW-ARRAY-Y-X ( ARR-LOCATION Y-SIZE X-SIZE -- )
dup rot * 0 DO
    dup I swap mod 0= IF
        CR
    THEN over I + c@ 3 .R
LOOP ;

( Fills the array with a 1 or 0 )
: FILL-RND ( ARR-LOCATION TOTAL-SIZE -- )
0 DO
    2 rnd over I + ! 
LOOP 
;

( Randomly adds a 1 or 0 to location in the array )
: ADD-RND-CELL  ( Location-in-array -- )
    2 rnd swap !
;

( Fills the central 50x50 grid with random cells )
: FILL-50-RND ( -- )
GRID-X @ 2 / 25 - dup 50 + swap DO                  ( loops through x' - 25 --> x' + 25)
    GRID-Y @ 2 / 25 - dup 50 + swap DO              ( loops through y' - 25 --> x' + 25)
        GRID-Y @ I * J + ARR-CELLS @ +
        ADD-RND-CELL                                ( randomly adds 1/0s )
    LOOP
LOOP
;

( Returns 1/0 for if it should be alive or dead )
: LIFE-RULE ( status neighbours -- new-status )
    CASE 
        0 OF drop 0 ENDOF 
        1 OF drop 0 ENDOF 
        2 OF 1 = abs ENDOF                          ( if current status = 0 return 0, else abs( -1 )
        3 OF drop 1 ENDOF 
        4 OF drop 0 ENDOF 
        5 OF drop 0 ENDOF 
        6 OF drop 0 ENDOF 
        7 OF drop 0 ENDOF 
        8 OF drop 0 ENDOF 
        ." More than 8 neighbours were counted. "
    ENDCASE 
;

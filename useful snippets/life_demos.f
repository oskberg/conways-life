\ include ..\conways_life_helper_code_examples_v4b\graphics_test.f
include words-list

: print-rnd cr
    100 0 DO 
        8 rnd . cr
    LOOP cr ;

{ location size }
: FILL-RND-0-8
0 DO
    9 rnd over I + C! 
LOOP ;



: counts CR ( ARRAY-LOCATION LENGTH RANGE -- OCCURANCES-LOCATION)
    ( RANGE ) dup ALLOCATE drop  OCCURANCES ! 
    OCCURANCES @ swap 0 FILL
    ( LENGTH ) 0 DO 
        ( ARR-LOC ) dup I + c@ ( VAL IN ARRAY)
        OCCURANCES @ ( START OF OCCURANCES ) 
        + ( INDEX IN OCCURANCES = VAL IN ARRAY ) dup 
        @ 1 + ( ADD ONE TO THAT VALUE )
        swap !
    LOOP 
    OCCURANCES @
;

: counts-short CR ( ARRAY-LOCATION LENGTH RANGE -- OCCURANCES-LOCATION)
    dup ALLOCATE drop  OCCURANCES ! OCCURANCES @ swap 0 FILL
    0 DO dup I + c@ OCCURANCES @ + dup @ 1 + swap ! LOOP 
    OCCURANCES @
;

: SHOW-COUNTS CR ( OCCURANCES RANGE -- ) 
    0 DO
        dup I + C@ . 
    LOOP
;

: SHOW-COUNTS-F CR ( OCCURANCES SIZE RANGE -- ) 
    0 DO
        i . over I + C@ s>f dup s>f f/ ." - " f. cr
    LOOP
;

\ 200 create-n-by-n CONSTANT arr-200
\ arr-200 200 200 * fill-rnd-0-8
\ VARIABLE OCCURANCES
\ ARR-200 200 9 COUNTS            { There are 9 possible values since 0-8 inclusive }
\ OCCURANCES @ 200 9 SHOW-COUNTS-f

{ returns 1 if alive and 0 if dead }
: life-rule ( status neighbours -- )
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
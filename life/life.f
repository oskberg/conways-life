{ IMPORTS }
INCLUDE words-list

{ GLOBAL VARIABLES }

VARIABLE    ARR-CELLS 
VARIABLE    ARR-NEIGH 

VARIABLE    GRID-X 
VARIABLE    GRID-Y 

{ LIFE-SPECIFIC FUNCTIONS }

: SETUP-LIFE ( -- )
    ( set grid sizes in globals )
    ( HAVE TO BE DIVISABLE BY 16? )
    8  GRID-X  !
    8  GRID-Y  !

    ( create arrays )
    GRID-X @ GRID-Y @ CREATE-X-BY-Y ARR-CELLS ! 
    GRID-X @ GRID-Y @ CREATE-X-BY-Y ARR-NEIGH ! 

    ( initilise with 0 )
    ARR-CELLS @ GRID-X @ GRID-Y @ * 0 FILL
    ARR-NEIGH @ GRID-X @ GRID-Y @ * 0 FILL
    
    ( SET SEED HERE )

    ( SIMPLE 3 CELL LINE IN THE MIDDLE, FLOORED OBVIOUSLY )
    1 ARR-CELLS @ GRID-X @ GRID-Y @ 2 / * GRID-X @ 2 / + + C!
    1 ARR-CELLS @ GRID-X @ GRID-Y @ 2 / * GRID-X @ 2 / + + 1 + C!
    1 ARR-CELLS @ GRID-X @ GRID-Y @ 2 / * GRID-X @ 2 / + + 1 - C!

    ( 1 0 alive )
    1 ARR-CELLS @ 1 + C!

; 

: SHOW-LIFE-ARRS ( -- )
    CR CR CR
    ARR-CELLS @ GRID-Y @ GRID-X @ SHOW-ARRAY-Y-X DROP DROP
    CR CR CR
    ARR-NEIGH @ GRID-Y @ GRID-X @ SHOW-ARRAY-Y-X DROP DROP
    CR CR CR
;

( counts neighbours of a cells in the arr-cells and puts them into arr-neigh )
( input x and y position. Number of neighbours left on stack )
( Possibly working for non-edge cases )
: COUNT-NEIGHBOURS-NOWRAP ( X Y -- N )
    0 ( start count at 0 )
    2 -1 DO 
        2 -1 DO
            I 0 = J 0 = + -2 = IF ( IF TRUE -> I = J = 0 and don't do anything )
            ELSE
                over J + ( Y + J )
                GRID-X @ *
                3 pick I + ( X + I )
                ARR-CELLS @ + + c@ ( Read status at position x y )
                + ( add to total )
            THEN
        LOOP
    LOOP
    ROT ROT DROP DROP
;

: COUNT-NEIGHBOURS-NOWRAP ( X Y -- N )
    0 ( start count at 0 )
    over 1 swap GRID-Y @ 1 - < -
    2 pick 0 > 
    DO
        over 1 swap GRID-X @ 1 - < -
        3 pick 0 > 
        DO 
            I 0 = J 0 = + -2 = IF ( IF TRUE -> I = J = 0 and don't do anything )
            ELSE
                over J + ( Y + J )
                GRID-X @ *
                3 pick I + ( X + I )
                ARR-CELLS @ + + c@ ( Read status at position x y )
                + ( add to total )
            THEN
        LOOP
    LOOP
    rot rot drop drop 
;

{ RUNNING BIT }

SETUP-LIFE
SHOW-LIFE-ARRS

3 3 COUNT-NEIGHBOURS-NOWRAP CR CR . CR CR 

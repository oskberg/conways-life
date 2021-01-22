{ GLOBAL VARIABLES }

VARIABLE    ARR-CELLS 
VARIABLE    ARR-NEIGH 
VARIABLE    GRID-X 
VARIABLE    GRID-Y 
VARIABLE    CURRENT-GEN

{ IMPORTS }
INCLUDE     words-list.f
INCLUDE     GRAPHICS_TEST.F
INCLUDE     input-output.f

{ LIFE-SPECIFIC FUNCTIONS }

: ADD-CELL ( row-index column-index -- )
    swap
    1 rot rot ARR-CELLS @ rot rot GRID-X @ * + + C!
;

: ACORN-200 cr
    ." ACORN setup "
    200 200 ADD-CELL
    201 201 ADD-CELL
    201 202 ADD-CELL
    201 203 ADD-CELL
    199 198  ADD-CELL
    201 198  ADD-CELL
    201 197  ADD-CELL
;

: SETUP-LIFE ( -- )
    ( set grid sizes in globals )
    ( HAVE TO BE DIVISABLE BY 16? )
    400  GRID-X         !
    400  GRID-Y         !
    0    CURRENT-GEN    !

    ( create arrays )
    GRID-X @ GRID-Y @ CREATE-X-BY-Y ARR-CELLS ! 
    GRID-X @ GRID-Y @ CREATE-X-BY-Y ARR-NEIGH ! 

    ( initilise with 0 )
    ARR-CELLS @ GRID-X @ GRID-Y @ * 0 FILL
    ARR-NEIGH @ GRID-X @ GRID-Y @ * 0 FILL
    
    ( SET SEED HERE )

    ( SIMPLE 3 CELL LINE IN THE MIDDLE, FLOORED OBVIOUSLY )
    \ 1 ARR-CELLS @ GRID-X @ GRID-Y @ 2 / * GRID-X @ 2 / + + C!
    \ 1 ARR-CELLS @ GRID-X @ GRID-Y @ 2 / * GRID-X @ 2 / + + 1 + C!
    \ 1 ARR-CELLS @ GRID-X @ GRID-Y @ 2 / * GRID-X @ 2 / + + 1 - C!

    ( 1 0 alive )
    \ 1 ARR-CELLS @ 1 + C!
    \ 1 ARR-CELLS @ 7 + 3 GRID-X @ * + C!

    \ 1 ARR-CELLS @ 1 GRID-X @ * + 3 + C!
    \ 1 ARR-CELLS @ 2 GRID-X @ * + 2 + C!
    \ 1 ARR-CELLS @ 2 GRID-X @ * + 4 + C!
    \ 1 ARR-CELLS @ 3 GRID-X @ * + 3 + C!
    ( methuselah 1 )
    \ 499 500 ADD-CELL
    \ 499 501 ADD-CELL
    \ 499 502 ADD-CELL
    \ 500 499 ADD-CELL
    \ 501 499 ADD-CELL
    ( methuselah 2 )
    \ 51 47 ADD-CELL
    \ 50 47 ADD-CELL
    \ 49 47 ADD-CELL
    \ 49 48 ADD-CELL
    \ 49 49 ADD-CELL
    \ 50 49 ADD-CELL
    \ 51 49 ADD-CELL

    \ ( methuselah 5 )
    \ 499 500 ADD-CELL
    \ 499 501 ADD-CELL
    \ 499 502 ADD-CELL
    \ 500 500 ADD-CELL
    \ 500 499 ADD-CELL
    \ 501 500 ADD-CELL
    \ 501 501 ADD-CELL

    ACORN-200

    GRID-X @ bmp-x-size !    { Create a blank 16x16 .bmp in memory    }
    GRID-Y @ bmp-y-size !
    Setup-Test-Memory

    New-bmp-Window-stretch
    bmp-window-handle !
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
: COUNT-NEIGHBOURS-NOWRAP ( X Y -- N )
    0 ( start count at 0 )
    over 1 swap GRID-Y @ 1 - < -
    2 pick 0 > 
    DO
        2 pick 1 swap GRID-X @ 1 - < -
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

( counts neighbours of a cells in the arr-cells and puts them into arr-neigh )
( input x and y position. Number of neighbours left on stack )
( Possibly working for non-edge cases )
: COUNT-NEIGHBOURS-WRAP ( X Y -- N )
    0
    ( x y 0 )
    ( Loop twice to go through -1, 0, 1 for x and y )
    2 -1 DO 
        2 -1 DO
            I 0 = J 0 = + -2 = IF                       ( if x = y = 0 skip )
            ELSE
                2 pick GRID-X @ J + + GRID-X @ mod      ( find the x coord )
                2 pick GRID-Y @ I + + GRID-Y @ mod      ( find the y coord )
                GRID-X @ * + ARR-CELLS @ + c@           ( find the location in array )
                +                                       ( add to total )
            THEN
            LOOP
        LOOP
    
    rot rot drop drop
;

( count neighbours for all cells and put them into neighbour array )
: COUNT-ALL-NEIGHBOURS ( -- )
    GRID-Y @ 0 DO
        GRID-X @ 0 DO
            J I COUNT-NEIGHBOURS-WRAP     ( number of neighbours )
            I GRID-X @ * J + ARR-NEIGH @ +    ( location in arr-neigh )
            c!                               ( write to that location )
        LOOP
    LOOP
;

( updates the life array with dead/allive cells )
: UPDATE-LIFE-ARRS ( -- )
    GRID-Y @ 0 DO
        GRID-X @ 0 DO
            I GRID-x @ * J + ARR-CELLS @ + c@   ( finds status of cell )
            I GRID-x @ * J + ARR-NEIGH @ + c@   ( finds # of neighbours )
            LIFE-RULE                           ( does rules to leave 1/0 on stack )
            I GRID-x @ * J + ARR-CELLS @ + c!   ( writes value to arr-cells )
        LOOP
    LOOP
;

: DRAW-LIFE
    ARR-CELLS @ ARRAY-TO-BMP-INV
    bmp-address @ bmp-to-screen-stretch
    \ bmp-window-handle @ DestroyWindow drop
    drop
;

: RUN-LIFE
    SETUP-LIFE
    depth . cr
    MAKE-TEST-FILE
    depth . cr
    WRITE-FILE-HEADER
    depth . cr
    BEGIN
        CURRENT-GEN @ 1 + CURRENT-GEN !
        DRAW-LIFE
        SAVE-CELL-STATS
        COUNT-ALL-NEIGHBOURS
        UPDATE-LIFE-ARRS
        1 ms
        KEY?
    UNTIL
    bmp-window-handle @ DestroyWindow drop
    CLOSE-TEST-FILE
;

{ RUNNING BIT }

\ 2 1 COUNT-NEIGHBOURS-NOWRAP CR CR . CR CR 
RUN-LIFE

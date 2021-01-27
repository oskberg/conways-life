{ GLOBAL VARIABLES }

VARIABLE    ARR-CELLS 
VARIABLE    ARR-NEIGH 
VARIABLE    GRID-X 
VARIABLE    GRID-Y 
VARIABLE    CURRENT-GEN
VARIABLE    BORN
VARIABLE    KILLED
VARIABLE    AVG-X 
VARIABLE    AVG-Y 

VARIABLE    STABLE-GENS

{ IMPORTS }
INCLUDE     words-list.f
INCLUDE     GRAPHICS_TEST.F
INCLUDE     input-output.f

{ LIFE-SPECIFIC FUNCTIONS }

: ADD-CELL ( row-index column-index -- )
    swap
    1 rot rot ARR-CELLS @ rot rot GRID-X @ * + + C!
;

: N-LINE ( N -- )
    ( TODO: ERROR CHECK LENGTH OF N? )
    dup
    0 DO 
        GRID-Y @ 2 / GRID-X @ 2 / 2 pick 2 / - I + ADD-CELL
    LOOP
    drop
;

: ACORN-400 cr
    ." ACORN setup "
    200 200  ADD-CELL
    201 201  ADD-CELL
    201 202  ADD-CELL
    201 203  ADD-CELL
    199 198  ADD-CELL
    201 198  ADD-CELL
    201 197  ADD-CELL
;

: WRAPPED-EDGES-TEST cr
    ." Wrapped edges blinker test setup "
    0 0 ADD-CELL
    1 0 ADD-CELL
    GRID-Y @ 1 - 0 ADD-CELL
;

: GLIDER-SETUP cr 
    ." GLIDER setup "
    1 0 ADD-CELL 
    2 1 ADD-CELL
    0 2 ADD-CELL 
    1 2 ADD-CELL 
    2 2 ADD-CELL
;

: SPACESHIP-SETUP cr 
    ." SPACESHIP setup "
    GRID-Y @ 2 / 0 ADD-CELL
    GRID-Y @ 2 / 2 + 0 ADD-CELL
    GRID-Y @ 2 / 1 - 1 ADD-CELL
    GRID-Y @ 2 / 1 - 2 ADD-CELL
    GRID-Y @ 2 / 1 - 3 ADD-CELL
    GRID-Y @ 2 / 1 - 4 ADD-CELL
    GRID-Y @ 2 / 2 + 3 ADD-CELL
    GRID-Y @ 2 / 4 ADD-CELL
    GRID-Y @ 2 / 1 + 4 ADD-CELL
;

: SETUP-LIFE ( -- )
    ( set grid sizes in globals )
    ( HAVE TO BE DIVISABLE BY 16? )
    32  GRID-X         !
    32  GRID-Y         !
    0    CURRENT-GEN    !
    0    STABLE-GENS    !
    0    AVG-X          !
    0    AVG-Y          !

    ( create arrays )
    GRID-X @ GRID-Y @ CREATE-X-BY-Y ARR-CELLS ! 
    GRID-X @ GRID-Y @ CREATE-X-BY-Y ARR-NEIGH ! 

    ( initilise with 0 )
    ARR-CELLS @ GRID-X @ GRID-Y @ * 0 FILL
    ARR-NEIGH @ GRID-X @ GRID-Y @ * 0 FILL
    
    ( SET SEED HERE )

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

    \ ACORN-400
    \ WRAPPED-EDGES-TEST
    GLIDER-SETUP
    \ SPACESHIP-SETUP
    \ 5 N-LINE
    ( RANDOM START )
    \ ARR-CELLS @ GRID-X @ GRID-Y @ * FILL-RND

    GRID-X @ bmp-x-size !
    GRID-Y @ bmp-y-size !
    Setup-Test-Memory

    New-bmp-Window-stretch
    bmp-window-handle !
; 

: SETUP-LIFE-SILENT ( N -- ; -- ) 
    ( set grid sizes in globals )
    ( HAVE TO BE DIVISABLE BY 16? )
    200  GRID-X         !
    200  GRID-Y         !
    0    CURRENT-GEN    !
    0    STABLE-GENS    !
    0    AVG-X          !
    0    AVG-Y          !

    ( create arrays )
    GRID-X @ GRID-Y @ CREATE-X-BY-Y ARR-CELLS ! 
    GRID-X @ GRID-Y @ CREATE-X-BY-Y ARR-NEIGH ! 

    ( initilise with 0 )
    ARR-CELLS @ GRID-X @ GRID-Y @ * 0 FILL
    ARR-NEIGH @ GRID-X @ GRID-Y @ * 0 FILL
    
    ( SET SEED HERE )
    \ N-LINE ( IF ACTIVE NEEDS INPUT)
    \ ACORN-400
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
            I J COUNT-NEIGHBOURS-WRAP     ( number of neighbours )
            J GRID-X @ * I + ARR-NEIGH @ +    ( location in arr-neigh )
            c!                               ( write to that location )
        LOOP
    LOOP
;

( updates the life array with dead/allive cells )
: UPDATE-LIFE-ARRS ( -- )
    0 BORN !
    0 KILLED !
    0 AVG-X !
    0 AVG-Y !
    GRID-Y @ 0 DO
        GRID-X @ 0 DO
            J GRID-x @ * I + ARR-CELLS @ + c@   ( finds status of cell )
            dup 
            J GRID-x @ * I + ARR-NEIGH @ + c@   ( finds # of neighbours )
            LIFE-RULE                           ( does rules to leaves 1/0 on stack )
            dup 
            J GRID-x @ * I + ARR-CELLS @ + c!   ( writes value to arr-cells )
            1 pick 1 pick < IF 
                BORN @ 1 + BORN !              ( if new status > old status cell has been born ) 
                ( check if at the border )
                I GRID-X @ 1 - MOD 0 =
                J GRID-Y @ 1 - MOD 0 =
                or IF CR ." HIT EDGE " THEN
            THEN swap 1 pick > 
                IF KILLED @ 1 + KILLED ! 
                THEN                                    ( TODO: Could count alive cells here instead of wherever were doing that)
                    1 = if                               ( if cell is alive add x & y values to averages )
                        AVG-X @ I + AVG-X !             ( i think that x and y are swapped )
                        AVG-Y @ J + AVG-Y !
                    then
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
    MAKE-TEST-FILE
    WRITE-FILE-HEADER
    DRAW-LIFE
    1000 ms
    BEGIN
        DRAW-LIFE
        SAVE-CELL-STATS
        COUNT-ALL-NEIGHBOURS
        UPDATE-LIFE-ARRS
        10 ms
        CURRENT-GEN @ 1 + CURRENT-GEN !
        KEY?
    UNTIL
    bmp-window-handle @ DestroyWindow drop
    CLOSE-TEST-FILE
;

: RUN-LIFE-SILENT
    SETUP-LIFE-SILENT
    MAKE-TEST-FILE
    WRITE-FILE-HEADER
    20 0 DO
        SAVE-CELL-STATS-UNIQUE
        COUNT-ALL-NEIGHBOURS
        UPDATE-LIFE-ARRS
        \ 1 ms
        CURRENT-GEN @ 1 + CURRENT-GEN !
    LOOP
    ." tHIS IS THE END "
    CLOSE-TEST-FILE
;

VARIABLE COUNTER
0 COUNTER !
: LINE-INVESTIGATION 
    SETUP-LIFE-SILENT
    MAKE-TEST-FILE
    WRITE-FILE-HEADER
    82 1 DO 
    CR I . CR
        ARR-CELLS @ GRID-X @ GRID-Y @ * 0 FILL
        ARR-NEIGH @ GRID-X @ GRID-Y @ * 0 FILL  
        I N-LINE
        I 10000 *  CURRENT-GEN    !
        0 STABLE-GENS !
        SAVE-CELL-STATS-UNIQUE drop

        0 COUNTER !
        BEGIN
            COUNT-ALL-NEIGHBOURS
            UPDATE-LIFE-ARRS
            \ 1 ms
            CURRENT-GEN @ 1 + CURRENT-GEN !
            COUNTER @ 1 + COUNTER !
            SAVE-CELL-STATS-UNIQUE dup IF ." break " then
            BORN @ KILLED @ = IF
                STABLE-GENS @ 1 + STABLE-GENS !
            ELSE 
                0 STABLE-GENS !
            THEN 
                STABLE-GENS @ 100 > OR COUNTER @ 1000 > OR
        UNTIL
    LOOP 
;

{ RUNNING BIT }

\ RUN-LIFE
LINE-INVESTIGATION


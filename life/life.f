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
VARIABLE    BUFFER 
VARIABLE    START-TIME
VARIABLE    END-TIME

VARIABLE    STABLE-GENS
VARIABLE    MAX-X
VARIABLE    MIN-X
VARIABLE    MAX-Y
VARIABLE    MIN-Y

{ IMPORTS }
INCLUDE     words-list.f
INCLUDE     GRAPHICS_TEST.F
INCLUDE     input-output.f

{ LIFE-SPECIFIC FUNCTIONS }

: ADD-CELL ( Y X -- )
    swap
    1 rot rot ARR-CELLS @ rot rot GRID-X @ * + + C!
;

: REMOVE-CELL ( Y X -- )
    swap
    0 rot rot ARR-CELLS @ rot rot GRID-X @ * + + C!
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

: GLIDER-SETUP-NOT-CORNER cr 
    ." GLIDER setup "
    2 1 ADD-CELL 
    3 2 ADD-CELL
    1 3 ADD-CELL 
    2 3 ADD-CELL 
    3 3 ADD-CELL
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
<<<<<<< HEAD
    504    GRID-X         !
    504    GRID-Y         !
    0    CURRENT-GEN    !
    0    STABLE-GENS    !
    0    AVG-X          !
    0    AVG-Y          !
=======
    16   BUFFER @ 2 * +  GRID-X     !
    16   BUFFER @ 2 * +  GRID-Y     !
    0    CURRENT-GEN            !
    0    STABLE-GENS             !
    0    AVG-X                  !
    0    AVG-Y                  !
>>>>>>> 305f6ed238ddad694ef10c0e4f1542bb53944571

    GRID-X @ MAX-X !
    GRID-Y @ MAX-Y !
    0 MIN-X !
    0 MIN-Y !


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

    \ GLIDER-SETUP

    \ GLIDER-SETUP-NOT-CORNER

    SPACESHIP-SETUP

    \ 5 N-LINE
    ( RANDOM START )
    \ ARR-CELLS @ GRID-X @ GRID-Y @ * FILL-RND

    \ FILL-50-RND


    GRID-X @ bmp-x-size !
    GRID-Y @ bmp-y-size !
    Setup-Test-Memory

    New-bmp-Window-stretch
    bmp-window-handle !
; 

: SETUP-LIFE-SILENT ( N -- ; -- ) 
    ( set grid sizes in globals )
    ( HAVE TO BE DIVISABLE BY 16? )
    504  GRID-X         !
    504  GRID-Y         !
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
    ( RANDOM START )
    \ ARR-CELLS @ GRID-X @ GRID-Y @ * FILL-RND

    ( RANDOM START 50 X 50 ) 
    FILL-50-RND
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
            I J  hOGBERG1
                 ( number of neighbours )
            J GRID-X @ * I + ARR-NEIGH @ +    ( location in arr-neigh )
            c!                               ( write to that location )
        LOOP
    LOOP
;

( THIS SHIT AINT WORKING CHEIF )
: COUNT-ALL-NEIGHBOURS-OPT ( -- )
    MAX-Y @ dup GRID-Y @ < - 1+
    MIN-Y @ DUP 0        > + DO
        MAX-X @ dup GRID-X @ < - 1+
        MIN-X @ dup 0        > + DO
            CR I . J .
            I J COUNT-NEIGHBOURS-WRAP     ( number of neighbours )
            J GRID-X @ * I + ARR-NEIGH @ +    ( location in arr-neigh )
            c!                               ( write to that location )
        LOOP
    LOOP
;

variable temp1
variable temp2
( updates the life array with dead/allive cells )
: UPDATE-LIFE-ARRS-OPT ( -- )
    CR ." OPT VERSION " CR
    0 BORN !
    0 KILLED !
    0 AVG-X !
    0 AVG-Y !
    
    ( SET CURRENT MIN TO THE MAX AND VICE VERSA )
    GRID-X @ MIN-X !
    GRID-Y @ MIN-Y !
    0 MAX-X !
    0 MAX-Y !

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
                        
                        ( GET MIN AND MAX X AND Y )
                        I MIN-X @ < IF I MIN-X ! THEN
                        J MIN-Y @ < IF J MIN-Y ! THEN
                        I MAX-X @ > IF I MAX-X ! THEN
                        J MAX-Y @ > IF J MAX-Y ! THEN
                    then
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
                or IF CR CURRENT-GEN @ . ." HIT EDGE " THEN
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

: CLEAR-BUFFER ( -- )
GRID-X @ 0 DO
    BUFFER @ 0 DO
        J I REMOVE-CELL
        J GRID-X @ I - REMOVE-CELL
    LOOP
LOOP
GRID-Y @ 0 DO
    BUFFER @ 0 DO
        I J REMOVE-CELL
        GRID-Y @ I - J REMOVE-CELL
    LOOP
LOOP
;

: RUN-LIFE
    0 BUFFER !
    SETUP-LIFE
    MAKE-TEST-FILE
    WRITE-FILE-HEADER
    DRAW-LIFE
    1000 ms
    \ DRAW-LIFE
    \ 10000 ms
    
    BEGIN
        DRAW-LIFE
        SAVE-CELL-STATS
        COUNT-ALL-NEIGHBOURS

        \ MAX-X @ temp1 !
        \ MIN-X @ temp2 !
        
        UPDATE-LIFE-ARRS
<<<<<<< HEAD
        10 ms
=======
        100 ms
>>>>>>> 305f6ed238ddad694ef10c0e4f1542bb53944571
        CURRENT-GEN @ 1 + CURRENT-GEN !
        KEY?
    UNTIL
    bmp-window-handle @ DestroyWindow drop
    CLOSE-TEST-FILE
;

: RUN-LIFE-SILENT
    0 BUFFER !
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

: RUN-LIFE-BUFFER 
    0 BUFFER !
    SETUP-LIFE
        MAKE-TEST-FILE
        WRITE-FILE-HEADER
        DRAW-LIFE
        5000 ms
        DRAW-LIFE
        1000 ms
        BEGIN
            DRAW-LIFE
            SAVE-CELL-STATS
            COUNT-ALL-NEIGHBOURS
            UPDATE-LIFE-ARRS
            CURRENT-GEN @ BUFFER @ 5 * MOD 0 = IF
                CLEAR-BUFFER
            THEN
            1 ms
            CURRENT-GEN @ 1 + CURRENT-GEN !
            KEY?
        UNTIL
        bmp-window-handle @ DestroyWindow drop
        CLOSE-TEST-FILE
;


VARIABLE COUNTER
0 COUNTER !
: LINE-INVESTIGATION 
    0 BUFFER !
    SETUP-LIFE-SILENT
    MAKE-TEST-FILE
    WRITE-FILE-HEADER
    150 1 DO 
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
            1 ms
            CURRENT-GEN @ 1 + CURRENT-GEN !
            COUNTER @ 1 + COUNTER !
            SAVE-CELL-STATS-UNIQUE dup IF ." break " then
            BORN @ KILLED @ = IF
                STABLE-GENS @ 1 + STABLE-GENS !
            ELSE 
                0 STABLE-GENS !
            THEN 
                STABLE-GENS @ 3 > OR COUNTER @ 1000 > OR
        UNTIL
    LOOP 
    CLOSE-TEST-FILE
;


: GRID-SIZE-INVESTIGATION 
    TIME&DATE DROP DROP DROP 60 * + 60 * + START-TIME !
    0 BUFFER !
    SETUP-LIFE-SILENT
    MAKE-TEST-FILE
    WRITE-FILE-HEADER
    12000 0 DO
        SAVE-CELL-STATS-UNIQUE
        COUNT-ALL-NEIGHBOURS
        UPDATE-LIFE-ARRS
        1 ms
        I 500 mod 0 = IF 
            CR
            CURRENT-GEN @ .
        THEN
        CURRENT-GEN @ 1 + CURRENT-GEN !
    LOOP

    cr
    TIME&DATE DROP DROP DROP 60 * + 60 * + END-TIME !
    CR
    END-TIME @ START-TIME @ - .
    CLOSE-TEST-FILE
;

{ RUNNING BIT }

\ RUN-LIFE
\ RUN-LIFE-SILENT
\ RUN-LIFE-BUFFER
<<<<<<< HEAD
LINE-INVESTIGATION

=======
\ LINE-INVESTIGATION
\ GRID-SIZE-INVESTIGATION
>>>>>>> 305f6ed238ddad694ef10c0e4f1542bb53944571

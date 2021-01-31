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
VARIABLE    UPDATE-COUNT
VARIABLE    COUNTER

VARIABLE    STABLE-GENS
VARIABLE    MAX-X
VARIABLE    MIN-X
VARIABLE    MAX-Y
VARIABLE    MIN-Y
VARIABLE    temp1
VARIABLE    temp2
VARIABLE    HIT-EDGE

{ IMPORTS }
INCLUDE     words-list.f
INCLUDE     GRAPHICS_TEST.F
INCLUDE     input-output.f

{ LIFE-SPECIFIC FUNCTIONS }

( Sets the cell at position x, y to alive. )
: ADD-CELL ( Y X -- )
    swap
    1 rot rot ARR-CELLS @ rot rot GRID-X @ * + + C!
;

( Sets the cell at position x, y to dead. )
: REMOVE-CELL ( Y X -- )
    swap
    0 rot rot ARR-CELLS @ rot rot GRID-X @ * + + C!
;

( Creates a line of alive cells in the middle of the grid, N cells wide. )
: N-LINE ( N -- )
    dup
    0 DO 
        GRID-Y @ 2 / GRID-X @ 2 / 2 pick 2 / - I + ADD-CELL
    LOOP
    drop
;

( Generates an Acorn starting configuration in the centre of a 400x400 grid. )
: ACORN-400 cr ( -- )
    ." ACORN setup "
    200 200  ADD-CELL
    201 201  ADD-CELL
    201 202  ADD-CELL
    201 203  ADD-CELL
    199 198  ADD-CELL
    201 198  ADD-CELL
    201 197  ADD-CELL
;

( Generates a blinker in the corner of the grid. )
: WRAPPED-EDGES-TEST cr ( -- )
    ." Wrapped edges blinker test setup "
    0 0 ADD-CELL
    1 0 ADD-CELL
    GRID-Y @ 1 - 0 ADD-CELL
;

( Generates a glider in the top left corner of the grid. )
: GLIDER-SETUP cr ( -- )
    ." GLIDER setup "
    1 0 ADD-CELL 
    2 1 ADD-CELL
    0 2 ADD-CELL 
    1 2 ADD-CELL 
    2 2 ADD-CELL
;

( Generates a glider 1 cell in diagonally from the corner of the grid. )
: GLIDER-SETUP-NOT-CORNER cr ( -- )
    ." GLIDER setup "
    2 1 ADD-CELL 
    3 2 ADD-CELL
    1 3 ADD-CELL 
    2 3 ADD-CELL 
    3 3 ADD-CELL
;

( Generates a spaceship in the centre of y axis, traveling from left to right on the grid. )
: SPACESHIP-SETUP cr ( -- )
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

( Generates the methuselah 1 from section 4.2 in the notes )
: Methuselah-1 ( -- )
    499 500 ADD-CELL
    499 501 ADD-CELL
    499 502 ADD-CELL
    500 499 ADD-CELL
    501 499 ADD-CELL
;

( Generates the methuselah 2 from section 4.2 in the notes )
: Methuselah-2 ( -- ) 
    51 47 ADD-CELL
    50 47 ADD-CELL
    49 47 ADD-CELL
    49 48 ADD-CELL
    49 49 ADD-CELL
    50 49 ADD-CELL
    51 49 ADD-CELL
;

( Generates the methuselah 5 from section 4.2 in the notes )
: Methuselah-5 ( -- )
    499 500 ADD-CELL
    499 501 ADD-CELL
    499 502 ADD-CELL
    500 500 ADD-CELL
    500 499 ADD-CELL
    501 500 ADD-CELL
    501 501 ADD-CELL
;

( Initialises the setup of the board and arrays with drawing )
: SETUP-LIFE ( -- )
    ( set grid sizes in globals )
    ( HAVE TO BE DIVISABLE BY 4 )
    ( Buffer is used for absorbing, set to 0 for wrapping )
    64  BUFFER @ 2 * +  GRID-X     !
    64  BUFFER @ 2 * +  GRID-Y     !
    0    CURRENT-GEN            !
    0    STABLE-GENS             !
    0    AVG-X                  !
    0    AVG-Y                  !

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

    \ Methuselah-1 

    \ Methuselah-2

    \ Methuselah-5

    \ ACORN-400

    \ WRAPPED-EDGES-TEST

    \ GLIDER-SETUP

    \ GLIDER-SETUP-NOT-CORNER

    \ SPACESHIP-SETUP

    \ 43 N-LINE

    ( RANDOM START )
    ARR-CELLS @ GRID-X @ GRID-Y @ * FILL-RND

    \ FILL-50-RND         ( CREATES A 50 X 50 RANDOM GRID IN MIDDLE of the array )

    ( Initialise the display window )
    GRID-X @ bmp-x-size !
    GRID-Y @ bmp-y-size !
    Setup-Test-Memory

    New-bmp-Window-stretch
    bmp-window-handle !
; 

( Initialises the setup of the board and arrays without drawing )
: SETUP-LIFE-SILENT ( -- ) 
    ( set grid sizes in globals )
    ( HAVE TO BE DIVISABLE BY 4 )
    1000  GRID-X        !
    1000  GRID-Y        !
    ( Buffer is used for absorbing, set to 0 for wrapping )
    \ 400  BUFFER @ 2 * +  GRID-X        !
    \ 400  BUFFER @ 2 * +  GRID-Y        !

    0    CURRENT-GEN    !
    0    STABLE-GENS    !
    0    AVG-X          !
    0    AVG-Y          !

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
    \ N-LINE ( IF ACTIVE NEEDS INPUT)
    \ ACORN-400
    ( RANDOM START )
    \ ARR-CELLS @ GRID-X @ GRID-Y @ * FILL-RND

    \ GLIDER-SETUP-NOT-CORNER

    ( RANDOM START 50 X 50 ) 
    \ FILL-50-RND
; 

( Prints the array using SHOW-ARRAY-Y-X in GRAPHICS_TEST to print the array in 1's and 0's )
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
    0                                       ( start count at 0 )
    over 1 swap GRID-Y @ 1 - < -            ( checks if on y boarder )
    2 pick 0 > 
    DO
        2 pick 1 swap GRID-X @ 1 - < -      ( checks if on x boarder )
        3 pick 0 > 
        DO 
            I 0 = J 0 = + -2 = IF           ( IF TRUE -> I = J = 0 and don't do anything )
            ELSE
                over J +                    ( Y + J )
                GRID-X @ *
                3 pick I +                  ( X + I )
                ARR-CELLS @ + + c@          ( Read status at position x y )
                +                           ( add to total )
            THEN
        LOOP
    LOOP
    rot rot drop drop                       ( drops X and Y and leaves N on the stack )
;

( counts neighbours of a cells in the arr-cells and puts them into arr-neigh )
( input x and y position. Number of neighbours left on stack )
: COUNT-NEIGHBOURS-WRAP ( X Y -- N )
    0                                                   ( start count at 0 )                                         
    2 -1 DO                                             ( Loop twice to go through -1, 0, 1 for x and y )
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
    
    rot rot drop drop                                   ( drops X and Y and leaves N on the stack )
;

( count neighbours for all cells and put them into neighbour array )
( can change between wrap and no-wrap or if using buffer then stay with wrap algorithm )
: COUNT-ALL-NEIGHBOURS ( -- )
    GRID-Y @ 0 DO
        GRID-X @ 0 DO
            I J COUNT-NEIGHBOURS-WRAP           ( number of neighbours )
            J GRID-X @ * I + ARR-NEIGH @ +      ( location in arr-neigh )
            c!                                  ( write # neighbours to that location )
        LOOP
    LOOP
;

( counts the # of neighbours for a percentage, S, of all the cells and leaves the rest unchanged )
: COUNT-S-NEIGHBOURS ( S -- )
    0 UPDATE-COUNT !
    GRID-Y @ 0 DO
        GRID-X @ 0 DO
            dup 100 RND > IF                        ( UPDATE ) 
                I J COUNT-NEIGHBOURS-WRAP           ( number of neighbours )
                J GRID-X @ * I + ARR-NEIGH @ +      ( location in arr-neigh )
                c!                                  ( write to that location )
                UPDATE-COUNT @ 1+ UPDATE-COUNT !    ( incrament # cells counted )
            THEN
        LOOP
    LOOP
    drop
    \ CR UPDATE-COUNT @ .
;

( Optimisation algorithm in progress which just checks the "active" area of the grid  )
: COUNT-ALL-NEIGHBOURS-OPT ( -- )
    CR MAX-Y @ . MIN-Y @ . MAX-X @ . MIN-X @ . CR
    MAX-Y @
    MIN-Y @ DO
        MAX-X @
        MIN-X @ DO
            I J COUNT-NEIGHBOURS-WRAP               ( number of neighbours )
            J GRID-X @ * I + ARR-NEIGH @ +          ( location in arr-neigh )
            c!                                      ( write to that location )
        LOOP
    LOOP
;


( Optimisation algorithm in progress which updates the life array with dead/allive cells )
: UPDATE-LIFE-ARRS-OPT ( -- )
    \ CR ." OPT VERSION " CR
    0 BORN !
    0 KILLED !
    0 AVG-X !
    0 AVG-Y !
    
    MAX-Y @ MIN-Y @
    MAX-X @ TEMP1 !
    MIN-X @ TEMP2 !
    ( SET CURRENT MIN TO THE MAX AND VICE VERSA )
    GRID-X @ MIN-X !
    GRID-Y @ MIN-Y !
    0 MAX-X !
    0 MAX-Y !

    FALSE HIT-EDGE !

    \ GRID-Y @ 0 DO
    \     GRID-X @ 0 DO
    \ CR .S
    DO
        TEMP1 @ TEMP2 @ DO
            J GRID-x @ * I + ARR-CELLS @ + c@       ( finds status of cell )
            dup 
            J GRID-x @ * I + ARR-NEIGH @ + c@       ( finds # of neighbours )
            LIFE-RULE                               ( does rules to leaves 1/0 on stack )
            dup 
            J GRID-x @ * I + ARR-CELLS @ + c!       ( writes value to arr-cells )
            1 pick 1 pick < IF 
                BORN @ 1 + BORN !                   ( if new status > old status cell has been born ) 
                ( check if at the border )
                I GRID-X @ 1 - MOD 0 =
                J GRID-Y @ 1 - MOD 0 =
                or IF 
                    CR ." HIT EDGE " TRUE HIT-EDGE ! 
                THEN
            THEN swap 1 pick > 
            IF KILLED @ 1 + KILLED ! 
            THEN                                    ( TODO: Could count alive cells here instead of wherever were doing that)
            1 = if                                  ( if cell is alive add x & y values to averages )
                AVG-X @ I + AVG-X !                 ( i think that x and y are swapped )
                AVG-Y @ J + AVG-Y !

                HIT-EDGE @ IF 
                        GRID-X @ MAX-X !
                        GRID-Y @ MAX-Y !
                        0 MIN-X !
                        0 MIN-Y !
                ELSE
                    ( GET MIN AND MAX X AND Y )
                    I MIN-X @ <= IF I 1 - MIN-X ! THEN
                    J MIN-Y @ <= IF J 1 - MIN-Y ! THEN
                    I MAX-X @ >= IF I 1 + MAX-X ! THEN
                    J MAX-Y @ >= IF J 1 + MAX-Y ! THEN
                THEN
            then
        LOOP
    LOOP
;

( updates the life array with dead/allive cells using the current status, # neighbours & life-rules )
: UPDATE-LIFE-ARRS ( -- )
    0 BORN !
    0 KILLED !
    0 AVG-X !
    0 AVG-Y !
    GRID-Y @ 0 DO
        GRID-X @ 0 DO
            J GRID-x @ * I + ARR-CELLS @ + c@               ( finds status of cell )
            dup 
            J GRID-x @ * I + ARR-NEIGH @ + c@               ( finds # of neighbours )
            LIFE-RULE                                       ( does rules to leaves 1/0 on stack )
            dup 
            J GRID-x @ * I + ARR-CELLS @ + c!               ( writes value to arr-cells )
            1 pick 1 pick < IF 
                BORN @ 1 + BORN !                           ( if new status > old status cell has been born ) 
                ( check if at the border )
                I GRID-X @ 1 - MOD 0 =
                J GRID-Y @ 1 - MOD 0 =
                or IF 
                    \ CR CURRENT-GEN @ . ." HIT EDGE " 
                    0 DROP 
                THEN
            THEN 
                swap 1 pick > IF                            ( if old status > new status cell has been killed )
                    KILLED @ 1 + KILLED ! 
                THEN                                       
                    1 = if                                  ( if cell is alive add x & y values to average totals )
                        AVG-X @ I + AVG-X !                 
                        AVG-Y @ J + AVG-Y !
                    then
        LOOP
    LOOP
;

( Draws the current stats of the cells array to the bitmap using ARRAY-TO-BMP-INV in GRAPHICS_TEST )
: DRAW-LIFE ( -- )
    ARR-CELLS @ ARRAY-TO-BMP-INV                            ( this also inverts the array to match the bitmap )
    bmp-address @ bmp-to-screen-stretch
    \ bmp-window-handle @ DestroyWindow drop
    drop
;

( Kills all cells inside the buffer )
: CLEAR-BUFFER ( -- )
    GRID-X @ 0 DO
        BUFFER @ 0 DO                                       ( kills cells along x = 0, x = GRID-X )
            J I REMOVE-CELL
            J GRID-X @ I - REMOVE-CELL
        LOOP
    LOOP
    GRID-Y @ 0 DO
        BUFFER @ 0 DO                                       ( kills cells along y = 0, y = GRID-Y )
            I J REMOVE-CELL
            GRID-Y @ I - J REMOVE-CELL
        LOOP
    LOOP
;

( Runs the game of life with drawing the generations )
: RUN-LIFE ( -- )
    0 BUFFER !                                  ( 0 for wrapping, n for buffer size n )
    SETUP-LIFE
    MAKE-TEST-FILE                              ( initialises file for writing stats )
    WRITE-FILE-HEADER
    DRAW-LIFE                                   ( draws initial configuration )
    1000 ms
    BEGIN                                       ( runs the game of life until key press )
        DRAW-LIFE
        SAVE-CELL-STATS
        COUNT-ALL-NEIGHBOURS
        UPDATE-LIFE-ARRS
        10 ms
        CURRENT-GEN @ 1 + CURRENT-GEN !
        KEY?
    UNTIL
    bmp-window-handle @ DestroyWindow drop      ( closes window and test file )
    CLOSE-TEST-FILE
;

( Runs the game of life for analysising phase transitions )
: RUN-LIFE-S ( S -- )
    0 BUFFER !                                  ( setup with wrapping edges )
    SETUP-LIFE
    MAKE-TEST-FILE                              ( initialises file for writing stats )
    WRITE-FILE-HEADER
    DRAW-LIFE
    1000 ms
    COUNT-ALL-NEIGHBOURS                        ( does an initial count )
    BEGIN                                       ( runs game of life until button pressed )
        DRAW-LIFE
        SAVE-CELL-STATS
        89 COUNT-S-NEIGHBOURS                   ( counts S percent of the cells and leaves the rest unchanged )
        UPDATE-LIFE-ARRS
        10 ms
        CURRENT-GEN @ 1 + CURRENT-GEN !
        KEY?
    UNTIL
    drop
    bmp-window-handle @ DestroyWindow drop      ( closes window and test file )
    CLOSE-TEST-FILE
;

( Runs the game of life without drawing each generation )
: RUN-LIFE-SILENT ( -- )
    SETUP-LIFE-SILENT                           ( setup with wrapping edges )
    MAKE-TEST-FILE                              ( initialises file for writing stats )
    WRITE-FILE-HEADER
    5000 0 DO                                   ( Processes N generations )
        SAVE-CELL-STATS
        COUNT-ALL-NEIGHBOURS
        UPDATE-LIFE-ARRS
        1 ms
        CURRENT-GEN @ 1 + CURRENT-GEN !
    LOOP
    ." This is the end "
    CLOSE-TEST-FILE                             ( closes file )
;

( Runs the game of life with absorbing edges using a buffer )
: RUN-LIFE-BUFFER ( -- )
    2 BUFFER !                                  ( setup with a buffer size N )
    SETUP-LIFE
    MAKE-TEST-FILE                              ( initialises file for writing stats )
    WRITE-FILE-HEADER
    DRAW-LIFE
    1000 ms
    BEGIN                                       ( Runs game of life till button press ) 
        DRAW-LIFE
        SAVE-CELL-STATS
        COUNT-ALL-NEIGHBOURS
        UPDATE-LIFE-ARRS
        CURRENT-GEN @ BUFFER @ 5 * MOD 0 = IF   ( Clears the buffer every 5 * buffer size generations )
            CLEAR-BUFFER
        THEN
        100 ms
        CURRENT-GEN @ 1 + CURRENT-GEN !
        KEY?
    UNTIL
    bmp-window-handle @ DestroyWindow drop      ( closes window and file )
    CLOSE-TEST-FILE
;

( Runs game of life for investigating cells in a line formation ) 
: LINE-INVESTIGATION ( -- )
    0 BUFFER !                                  ( Setup with wrapping so no buffer )
    SETUP-LIFE-SILENT                           ( Setup without drawing )
    MAKE-TEST-FILE                              ( Creates test file )
    WRITE-FILE-HEADER
    150 1 DO                                    ( Runs for n = 1 --> 150 )
        CR I . CR
        ARR-CELLS @ GRID-X @ GRID-Y @ * 0 FILL  ( Re-initialises arrays )
        ARR-NEIGH @ GRID-X @ GRID-Y @ * 0 FILL  
        I N-LINE                                ( Creates line with N cells )
        I 1000 *  CURRENT-GEN    !              ( Sets the current generation )
        0 STABLE-GENS !                         ( resets stable generation )
        SAVE-CELL-STATS-UNIQUE drop             ( saves stats )

        0 COUNTER !                             ( initialises counter )
        BEGIN                                   ( runs game of life until stable for > 3 generations )
            COUNT-ALL-NEIGHBOURS                ( or run for 1000 generations                        )
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
    TIME&DATE DROP DROP DROP 60 * + 60 * + START-TIME !     ( Records initial time /s )
    0 BUFFER !                                              ( initial setup with wrapping )
    SETUP-LIFE-SILENT
    MAKE-TEST-FILE
    WRITE-FILE-HEADER
    10000 0 DO                                              ( Runs for 10000 generations )
        SAVE-CELL-STATS
        COUNT-ALL-NEIGHBOURS
        UPDATE-LIFE-ARRS
        1 ms
        \ CURRENT-GEN @ BUFFER @ 5 * MOD 0 = IF             ( uncomment if using buffer to clear it )
        \     CLEAR-BUFFER
        \ THEN
        I 500 mod 0 = IF                                    ( prints progress )
            CR
            CURRENT-GEN @ .
            CR
        THEN
        CURRENT-GEN @ 1 + CURRENT-GEN !
    LOOP

    cr
    TIME&DATE DROP DROP DROP 60 * + 60 * + END-TIME !       ( records final time /s )
    CR
    END-TIME @ START-TIME @ - .                             ( prints total time )
    CLOSE-TEST-FILE
;

: S-INVESTIGATION ( S1 S2 -- )
    0 BUFFER !
    SETUP-LIFE-SILENT
    MAKE-TEST-FILE
    WRITE-FILE-HEADER
    DO 
        CR I .
        ARR-CELLS @ GRID-X @ GRID-Y @ * 0 FILL  
        ARR-NEIGH @ GRID-X @ GRID-Y @ * 0 FILL  
        ARR-CELLS @ GRID-X @ GRID-Y @ * FILL-RND
        I 10000 *  CURRENT-GEN    !
        0 STABLE-GENS !
        SAVE-CELL-STATS-UNIQUE drop
        0 COUNTER !
        drop
        COUNT-ALL-NEIGHBOURS
        BEGIN
            1 MS
            I COUNT-S-NEIGHBOURS
            UPDATE-LIFE-ARRS
            CURRENT-GEN @ 1+ CURRENT-GEN !
            COUNTER @ 1+ COUNTER !
            SAVE-CELL-STATS-UNIQUE dup IF ." break " then
            BORN @ KILLED @ = IF
                STABLE-GENS @ 1+ STABLE-GENS !
            ELSE 
                0 STABLE-GENS !
            THEN 
                STABLE-GENS @ 5 > OR COUNTER @ 5000 > OR
            \ COUNTER @ 10 mod IF CR .S THEN
        UNTIL
    LOOP 
    CLOSE-TEST-FILE
;

{ RUNNING BIT }

\ RUN-LIFE
\ RUN-LIFE-SILENT
RUN-LIFE-BUFFER
\ LINE-INVESTIGATION
\ RUN-LIFE-S

96 85 S-INVESTIGATION

\ TIME&DATE DROP DROP DROP 60 * + 60 * + START-TIME !

\ RUN-LIFE-SILENT

\ TIME&DATE DROP DROP DROP 60 * + 60 * + END-TIME !
\ CR
\ END-TIME @ START-TIME @ - .
\ GRID-SIZE-INVESTIGATION
\ RUN-LIFE-S

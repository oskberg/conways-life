{ ======= INSTRUCTIONS =======                                                          }
{ This file contains the logic for running different simulations of conways life.       }
{ The last section contains commented tests which can be uncommented to perform tests   }

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
VARIABLE    ALIVE-CELLS
VARIABLE    S-CURRENT
VARIABLE    STEP-SIZE
VARIABLE    FOUND-S
VARIABLE    STABLE-GENS
VARIABLE    MAX-X
VARIABLE    MIN-X
VARIABLE    MAX-Y
VARIABLE    MIN-Y
VARIABLE    TEMP1
VARIABLE    TEMP2
VARIABLE    HIT-EDGE

{ IMPORTS }
INCLUDE     words-list.f
INCLUDE     GRAPHICS_TEST.F
INCLUDE     input-output.f
INCLUDE     seeds.f

{ WORDS }

( Initialises the setup of the board and arrays with drawing )
: SETUP-LIFE ( -- )
    ( set grid sizes in globals )
    ( HAVE TO BE DIVISABLE BY 4 )
    ( Buffer is used for absorbing, set to 0 for wrapping )
    500  BUFFER @ 2 * +  GRID-X      !
    300  BUFFER @ 2 * +  GRID-Y      !
    0   CURRENT-GEN                 !
    0   STABLE-GENS                 !
    0   AVG-X                       !
    0   AVG-Y                       !
    0   ALIVE-CELLS                 !

    ( used when optimisation is active )
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
    ACORN-400
    \ WRAPPED-EDGES-TEST
    \ GLIDER-SETUP
    \ GLIDER-SETUP-NOT-CORNER
    \ SPACESHIP-SETUP
    \ 43 N-LINE

    ( RANDOM START )
    \ ARR-CELLS @ GRID-X @ GRID-Y @ * FILL-RND
    \ FILL-50-RND         ( CREATES A 50 X 50 RANDOM GRID IN MIDDLE of the array )

    ( Initialise the display window )
    GRID-X @ bmp-x-size !
    GRID-Y @ bmp-y-size !
    Setup-Test-Memory

    New-bmp-Window-stretch
    bmp-window-handle !
; 

( Initialises the setup of the board and arrays without drawing to bmp Window )
: SETUP-LIFE-SILENT ( -- ) 
    ( set grid sizes in globals )
    ( HAVE TO BE DIVISABLE BY 4 )
    64  GRID-X        !
    64  GRID-Y        !
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
            dup 1000 RND > IF                       ( UPDATE ) 
                I J COUNT-NEIGHBOURS-WRAP           ( number of neighbours )
                J GRID-X @ * I + ARR-NEIGH @ +      ( location in arr-neigh )
                c!                                  ( write to that location )
                UPDATE-COUNT @ 1+ UPDATE-COUNT !    ( incrament # cells counted )
            THEN
        LOOP
    LOOP
    drop
    \ CR UPDATE-COUNT @ .                           ( can be used to check fraction counted )
;

( Optimised version which just checks the "active" area of the grid )
( requires UPDATE-LIFE-ARRS-OPT when running )
( replace COUNT-ALL-NEIGHBOURS with COUNT-ALL-NEIGHBOURS-OPT to run )
: COUNT-ALL-NEIGHBOURS-OPT ( -- )
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

( updates the life array with dead/allive cells using the current status, # neighbours & life-rules )
: UPDATE-LIFE-ARRS ( -- )
    0 BORN !
    0 KILLED !
    0 AVG-X !
    0 AVG-Y !
    0 ALIVE-CELLS !
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
                        ALIVE-CELLS @ 1+ ALIVE-CELLS !
                        AVG-X @ I + AVG-X !                 
                        AVG-Y @ J + AVG-Y !
                        ALIVE-CELLS @ 1+ ALIVE-CELLS !      ( count alive cells )

                    then
        LOOP
    LOOP
;

( Optimised version which updates the life array with dead/allive cells )
( replace UPDATE-LIFE-ARRS with UPDATE-LIFE-ARRS-OPT to run )
: UPDATE-LIFE-ARRS-OPT ( -- )
    \ CR ." OPT VERSION " CR
    0 BORN !
    0 KILLED !
    0 AVG-X !
    0 AVG-Y !
    0 ALIVE-CELLS !
    
    MAX-Y @ MIN-Y @                                 ( put y values on stack for loop later )
    MAX-X @ TEMP1 !                                 ( store x range in temporaries )
    MIN-X @ TEMP2 !
    
    GRID-X @ MIN-X !                                ( prepare for finding min and max values )
    GRID-Y @ MIN-Y !
    0 MAX-X !
    0 MAX-Y !

    FALSE HIT-EDGE !                                ( reset hit-edge )

    DO
        TEMP1 @ TEMP2 @ DO                          ( loop over subgrid and not entire grid )
            I GRID-X @ > I 0 < OR                   ( error checking )
            J GRID-Y @ > J 0 < OR
            OR IF
                CR I . J . THEN
            J GRID-X @ * I + ARR-CELLS @ + c@       ( finds status of cell )
            dup 
            J GRID-X @ * I + ARR-NEIGH @ + c@       ( finds # of neighbours )
            LIFE-RULE                               ( does rules to leaves 1/0 on stack )
            dup 
            J GRID-X @ * I + ARR-CELLS @ + c!       ( writes value to arr-cells )
            1 pick 1 pick < IF 
                BORN @ 1 + BORN !                   ( if new status > old status cell has been born ) 
                I GRID-X @ 1 - MOD 0 =              ( check if cell is on a border )
                J GRID-Y @ 1 - MOD 0 =
                or IF 
                    CR ." HIT EDGE " TRUE HIT-EDGE ! ( if on edge update hit-edge )
                THEN
            THEN swap 1 pick > 
            IF KILLED @ 1 + KILLED ! 
            THEN                                    
            1 = IF                                  ( if cell is alive add x & y values to averages )
                AVG-X @ I + AVG-X !                 ( i think that x and y are swapped )
                AVG-Y @ J + AVG-Y !
                ALIVE-CELLS @ 1+ ALIVE-CELLS !      ( count alive cells )

                HIT-EDGE @ IF                       ( if hit edge, consider whole grid )
                    GRID-X @ MAX-X !
                    GRID-Y @ MAX-Y !
                    0 MIN-X !
                    0 MIN-Y !
                ELSE                                ( GET MIN AND MAX X AND Y )
                    I MIN-X @ <= IF I I 0 > + MIN-X ! THEN              ( if finding smaller x than current min-x, update )
                    J MIN-Y @ <= IF J J 0 > + MIN-Y ! THEN              ( similar for rest)
                    I MAX-X @ 1- >= IF I 1 + I GRID-X @ < - MAX-X ! THEN
                    J MAX-Y @ 1- >= IF J 1 + J GRID-Y @ < - MAX-Y ! THEN
                THEN
            then
        LOOP
    LOOP
    ALIVE-CELLS @ 0= IF         ( if no cells are alive, consider whole grid to avoid errors )
        GRID-X @ MAX-X !
        GRID-Y @ MAX-Y !
        0 MIN-X !
        0 MIN-Y !
    THEN
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

{ Investigations- words which are specific for a certain investigation }

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
        COUNT-ALL-NEIGHBOURS-OPT
        UPDATE-LIFE-ARRS-OPT
        1 ms
        CURRENT-GEN @ 1 + CURRENT-GEN !
        KEY?
    UNTIL
    bmp-window-handle @ DestroyWindow drop      ( closes window and test file )
    CLOSE-TEST-FILE
;

( Runs the game of life for analysising phase transitions )
( Only counts neighbours for a fraction S / 1000 cells    )
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
        COUNT-ALL-NEIGHBOURS                    ( add -opt for optimised version)
        UPDATE-LIFE-ARRS                        ( add -opt for optimised version)
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

( Runs game of life for investigating seed in a line formation ) 
: LINE-INVESTIGATION ( -- )
    0 BUFFER !                                  ( Setup with wrapping so no buffer )
    SETUP-LIFE-SILENT                           ( Setup without drawing, remove silent for animation )
    MAKE-TEST-FILE                              ( Creates test file )
    WRITE-FILE-HEADER
    100 1 DO                                    ( Runs for n = 1 --> 150 )
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
            CURRENT-GEN @ 1 + CURRENT-GEN !     ( updates current generations number)
            COUNTER @ 1 + COUNTER !
            SAVE-CELL-STATS-UNIQUE dup IF ." break " then   ( if all cells are dead, break by leaving true)
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

( algorithm which runs life with different S values between S1 and S2 )
: S-INVESTIGATION ( S1 S2 -- )
    0 BUFFER !  
    SETUP-LIFE-SILENT                                       ( Setup life )
    MAKE-TEST-FILE
    WRITE-FILE-HEADER
    DO                                                      ( run in range s1 -> s2 - 1 )
        CR I .
        ARR-CELLS @ GRID-X @ GRID-Y @ * 0 FILL              ( Clear arrays )
        ARR-NEIGH @ GRID-X @ GRID-Y @ * 0 FILL  
        ARR-CELLS @ GRID-X @ GRID-Y @ * FILL-RND            ( setup random seed )
        I 10000 * CURRENT-GEN    !                          ( Separate different runs in stats file )
        0 STABLE-GENS !
        SAVE-CELL-STATS-UNIQUE drop
        0 COUNTER !
        drop
        COUNT-ALL-NEIGHBOURS                                ( count all neighbours before start )
        BEGIN
            1 MS                                            
            I 10 * COUNT-S-NEIGHBOURS                       ( count fraction of neighbours, change 10 to change step size)
            UPDATE-LIFE-ARRS
            CURRENT-GEN @ 1+ CURRENT-GEN !
            COUNTER @ 1+ COUNTER !
            SAVE-CELL-STATS-UNIQUE dup IF ." break " then   ( if stable, break)
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

( Finds the critical s value for a grid of dimensions GRID-X x GRID-Y )
( starts at s_0 which must be higher than s_c )
( must specify s-current and step-size variables )
: FIND-S-CRITICAL ( -- s_c)
    CR CR ." LOOKING FOR S_c " CR
    0 BUFFER !                                      ( setup life for simulation )
    SETUP-LIFE-SILENT
    MAKE-TEST-FILE
    FALSE FOUND-S !
    BEGIN                                           ( clear life and initilise with random seed )
        ARR-CELLS @ GRID-X @ GRID-Y @ * 0 FILL  
        ARR-NEIGH @ GRID-X @ GRID-Y @ * 0 FILL  
        ARR-CELLS @ GRID-X @ GRID-Y @ * FILL-RND
        S-CURRENT @ 10000 *  CURRENT-GEN    !
        0 STABLE-GENS !
        SAVE-CELL-STATS
        0 COUNTER !
        drop
        COUNT-ALL-NEIGHBOURS
        BEGIN
            1 MS
            S-CURRENT @ COUNT-S-NEIGHBOURS           ( count fraction of neighbours )
            UPDATE-LIFE-ARRS
            CURRENT-GEN @ 1+ CURRENT-GEN !
            COUNTER @ 1+ COUNTER !
            SAVE-CELL-STATS

            BORN @ KILLED @ = IF                    ( if activity is stable for longe enough, break )
                STABLE-GENS @ 1+ STABLE-GENS !
            ELSE 
                0 STABLE-GENS !
            THEN 

            ALIVE-CELLS 0= BORN @ KILLED @ + 0= OR IF   ( if activity is 0, change s by step-size )
                S-CURRENT @ STEP-SIZE @ - S-CURRENT !
                TRUE                                    ( break out of loop )
            ELSE FALSE
            THEN 

            STABLE-GENS @ 5 >                           ( if stable we have found s_c )
            COUNTER @ 5000 > 
            OR IF
                S-CURRENT                               ( we know that s-current is s_c )
                TRUE FOUND-S !
            ELSE FALSE
            THEN
            OR
        UNTIL
        FOUND-S @                                       ( when s_c is found, break )
    UNTIL  
    CLOSE-TEST-FILE
    S-CURRENT @                                         ( put s_c on the stack )
;

{ RUNNING BIT }

( simple animation of life, choose seed in SETUP-LIFE )
RUN-LIFE

( run life simulation without animation, choose seed in SETUP-LIFE-SILENT )
\ RUN-LIFE-SILENT

( run life with an absorbing buffer )
\ RUN-LIFE-BUFFER

( run line investigation )
\ LINE-INVESTIGATION

( run life with asynchronus updating with animation )
\ RUN-LIFE-S

( find the s_c value for a certain grid size. Takes range to investigate, s * 1000, as input )
\ 850 950 S-INVESTIGATION 

( run grid investigation )
\ GRID-SIZE-INVESTIGATION

( Find critical s value for the current grid size specified in setup )
\ 950 S-CURRENT !                   ( start value of s, must be above s_c)
\ 10 STEP-SIZE !                    ( specify precision, the smaller the slower but more accurate )
\ FIND-S-CRITICAL .                 ( prints the s_c value )


\ TIME&DATE DROP DROP DROP 60 * + 60 * + START-TIME ! ( times the function inbetween with second precision )

\ RUN-LIFE-SILENT

\ TIME&DATE DROP DROP DROP 60 * + 60 * + END-TIME !
\ CR END-TIME @ START-TIME @ - .
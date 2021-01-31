{ File which contains different starting seeds }

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
    dup 0 DO 
        GRID-Y @ 2 / GRID-X @ 2 / 2 pick 2 / - I + ADD-CELL
    LOOP drop
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
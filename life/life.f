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

;

: SHOW-LIFE-ARRS ( -- )
    CR CR CR
    ARR-CELLS @ GRID-Y @ GRID-X @ SHOW-ARRAY-Y-X DROP DROP
    CR CR CR
    ARR-NEIGH @ GRID-Y @ GRID-X @ SHOW-ARRAY-Y-X DROP DROP
    CR CR CR
;

( counts neighbours of all the cells in the arr-cells and puts them into arr-neigh )
: COUNT-ALL-NEIGHBOURS- ( -- )

;

{ RUNNING BIT }

SETUP-LIFE
SHOW-LIFE-ARRS


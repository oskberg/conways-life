{ File for outputting statistics        }
{ change file name to fit your computer }

variable TEST-FILE-ID                             { Create Variable to hold file id handle }


: MAKE-TEST-FILE                                  { Create a test file to read / write to  }
  s" C:\Users\Oskar\Desktop\conways-life\conways-life\output\210130-4.csv" r/w create-file drop  { Create the file                        } 
  \ s" C:\Users\lukem\OneDrive - Imperial College London\Year 3\Labs\Cycle 1\Proper Code\conways-life\output\cell_stats.csv" r/w create-file drop  { Create the file                        } 
  TEST-FILE-ID !                                  { Store file handle for later use        }
;

 
: OPEN-TEST-FILE                                  { Open the file for read/write access    }
  s" C:\Users\Oskar\Desktop\conways-life\conways-life\output\210130-4.csv" r/w open-file drop    { Not needed if we have just created     }
  \ s" C:\Users\lukem\OneDrive - Imperial College London\Year 3\Labs\Cycle 1\Proper Code\conways-life\output\cell_stats.csv" r/w open-file drop    { Not needed if we have just created     }
  TEST-FILE-ID !                                  { file.                                  }
;


: CLOSE-TEST-FILE                                 { Close the file pointed to by the file  }
  TEST-FILE-ID @                                  { handle.                                }
  CLOSE-FILE drop
; 


: TEST-FILE-SIZE                                  { Leave size of file on top of stack as  }
  TEST-FILE-ID @                                  { a double prescision integer if the     }
  file-size drop                                  { file is open.                          }
;

( writes the file header )
: WRITE-FILE-HEADER ( -- )
  s" generation #, # alive cells, # dead cells, BORN, KILLED, avg x pos, avg y pos " TEST-FILE-ID @ write-line drop
;

( writes the # of alive and dead cells to a csv file in output )
: SAVE-CELL-STATS ( -- )
    CURRENT-GEN @ (.) TEST-FILE-ID @ write-file drop            ( writes the current generation )
    s" ," TEST-FILE-ID @ write-file drop              

    0
    GRID-Y @ 0 DO
        GRID-X @ 0 DO
            I GRID-x @ * J + ARR-CELLS @ + c@                   ( finds the value of the cell )
            +                                                   ( adds it to the total )
        LOOP
    LOOP
    dup dup dup
    (.) TEST-FILE-ID @ write-file drop                          ( writes # alive cells to csv file )
    s" ," TEST-FILE-ID @ write-file drop

    ( # alive cells )
    GRID-X @ BUFFER @ 2 * - GRID-Y @ BUFFER @ 2 * - * swap -    ( finds the # dead cells )
    (.) TEST-FILE-ID @ write-file drop                          ( writes to csv file )
    s" ," TEST-FILE-ID @ write-file drop
    
    BORN @ (.) TEST-FILE-ID @ write-file drop
    s" ," TEST-FILE-ID @ write-file drop                        ( writes the # born cells )

    KILLED @ (.) TEST-FILE-ID @ write-file drop
    s" ," TEST-FILE-ID @ write-file drop                        ( writes the # killed cells )

    dup 0 = IF DROP DROP ." No alive cells "                    ( checkes if cells dead or alive )
      0 (.) TEST-FILE-ID @ write-file drop
      s" ," TEST-FILE-ID @ write-file drop                      ( writes 0 by default if no cells alive )
      0 (.) TEST-FILE-ID @ write-line drop                      ( for average x & y position            )
    ELSE 
      AVG-X @ swap / (.) TEST-FILE-ID @ write-file drop         ( find avg x & y by doing total/# alive cells )
      s" ," TEST-FILE-ID @ write-file drop
      AVG-Y @ swap / (.) TEST-FILE-ID @ write-line drop
    THEN 
    \ CR .S
;

( writes the # of alive and dead cells to a csv file in output )
: SAVE-CELL-STATS-UNIQUE ( -- stable )
    CURRENT-GEN @ (.) TEST-FILE-ID @ write-file drop            ( writes the current generation )
    s" ," TEST-FILE-ID @ write-file drop              

    ALIVE-CELLS @
    
    dup dup dup

    (.) TEST-FILE-ID @ write-file drop                          ( writes # alive cells to csv file )
    s" ," TEST-FILE-ID @ write-file drop

    GRID-X @ GRID-Y @ * swap -                                  ( finds the # dead cells )
    (.) TEST-FILE-ID @ write-file drop                          ( writes to csv file )
    s" ," TEST-FILE-ID @ write-file drop
    
    BORN @ (.) TEST-FILE-ID @ write-file drop
    s" ," TEST-FILE-ID @ write-file drop                        ( writes the # born cells )

    KILLED @ (.) TEST-FILE-ID @ write-file drop
    s" ," TEST-FILE-ID @ write-file drop                        ( writes the # killed cells )

    dup 0 = IF DROP DROP ." No alive cells "                    ( checkes if cells dead or alive )
      0 (.) TEST-FILE-ID @ write-file drop
      s" ," TEST-FILE-ID @ write-file drop                      ( writes 0 by default if no cells alive )
      0 (.) TEST-FILE-ID @ write-line drop                      ( for average x & y position            )
      TRUE
    ELSE 
      AVG-X @ swap / (.) TEST-FILE-ID @ write-file drop         ( find avg x & y by doing total/# alive cells )
      s" ," TEST-FILE-ID @ write-file drop
      AVG-Y @ swap / (.) TEST-FILE-ID @ write-line drop
      FALSE
    THEN 
;

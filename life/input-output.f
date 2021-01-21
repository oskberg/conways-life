variable TEST-FILE-ID                             { Create Variable to hold file id handle }


: MAKE-TEST-FILE                                  { Create a test file to read / write to  }
  s" C:\Users\Oskar\Desktop\conways-life\conways-life\output\cell_stats.csv" r/w create-file drop  { Create the file                        } 
  \ s" C:\Users\lukem\OneDrive - Imperial College London\Year 3\Labs\Cycle 1\Proper Code\conways-life\output\cell_stats.csv" r/w create-file drop  { Create the file                        } 
  TEST-FILE-ID !                                  { Store file handle for later use        }
;

 
: OPEN-TEST-FILE                                  { Open the file for read/write access    }
  s" C:\Users\Oskar\Desktop\conways-life\conways-life\output\cell_stats.csv" r/w open-file drop    { Not needed if we have just created     }
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
: WRITE-FILE-HEADER
  s" # alive cells, # dead cells " TEST-FILE-ID @ write-line drop
;

( writes the # of alive and dead cells to a csv file in output )
: SAVE-CELL-STATS ( -- )
    0
    GRID-Y @ 0 DO
        GRID-X @ 0 DO
            I GRID-x @ * J + ARR-CELLS @ + c@   ( finds the value of the cell )
            +                                   ( adds it to the total )
        LOOP
    LOOP

    dup GRID-X @ GRID-Y @ * swap -              ( finds the # dead cells )
    swap
    CURRENT-GEN @ (.) TEST-FILE-ID @ write-file drop
    s" ," TEST-FILE-ID @ write-file drop
    (.) TEST-FILE-ID @ write-file drop          ( writes data to csv file )
    s" ," TEST-FILE-ID @ write-file drop
    (.) TEST-FILE-ID @ write-line drop
    
;

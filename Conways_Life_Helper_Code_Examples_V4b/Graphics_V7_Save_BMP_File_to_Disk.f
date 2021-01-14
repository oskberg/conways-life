
{ ---------------------------------------------------------------------------------------- }
{                                                                                          }
{ Words to create bitmap image files ( .bmp) in memory and save them to disk to allow a    } 
{ snapshot visualisation of data.  This may be helpful for Mac users who have isues with   }
{ SwiftForth.  If you do not specify a path, .bmp files will be saved in the               }
{ \SwiftForth\Bin directory.  Hard coded default here is c:\temp\bmp_output_0001.bmp       }
{                                                                                          }
{ There are also alternative Forth versions that may work more effectively on a Mac        }
{ including VFX Forth (full commerial package) and GForth                                  }
{                                                                                          }
{  https://www.mpeforth.com/software/pc-systems/vfx-forth-for-windows/                     }
{                                                                                          }
{  https://www.gnu.org/software/gforth/                                                    }
{                                                                                          }
{ Note that bmp x size must be integer divisible by 4 to avoid display glitches without    }
{ padding line ends - this is a general feature of the bmp file format and is not codes    }
{ specific.  bmp x sizes will be rounded down to the nearest factor of as a result 4.      }
{                                                                                          }
{                                                                                          }
{          Roland Smith, V7 revised 03/12/2020 For 3rd Year Lab D3 Experiment              }
{                                                                                          }
{ ---------------------------------------------------------------------------------------- }
{                                                                                          }
{                                Global constants and variables                            }

variable bmp-x-size     { x dimension of bmp file # 1                                    }

variable bmp-y-size     { y dimension of bmp file # 1                                    }

variable bmp-size       { Total number of bmp elements = (x * y)                         }

variable bmp-address    { Stores start address of bmp file # 1                           }

variable bmp-length     { Total number of chars in bmp including header block            }

variable bmp-x-start    { Initial x position of upper left corner                        }

variable bmp-y-start    { Initial y position of upper left corner                        }

variable bmp-file-handle { Stores handle of bmp file opened for disk operations          }

200 bmp-x-size !                               { Set x size of bmp in pixels             }

200 bmp-y-size !                               { Set y size of bmp in pixels             }

bmp-x-size @ 4 / 1 max 4 *  bmp-x-size !       { Trim x-size to integer product of 4     }

bmp-x-size @ bmp-y-size @ * bmp-size !         { Find number of pixels in bmp            }

bmp-size   @ 3 * 54 +       bmp-length !       { Find length of bmp in chars inc. header }


{ -------------------------  Random number routine for testing ------------------------- } 

CREATE SEED  123475689 ,

: RND ( n -- rnd )   { Returns single random number less than n }
   SEED              { Minimal version of SwiftForth Rnd.f      }
   DUP >R            { Algorithm Rick VanNorman  rvn@forth.com  }
   @ 127773 /MOD 
   2836 * SWAP 16807 * 
   2DUP > IF - 
   ELSE - 2147483647 +  
   THEN  DUP R> !
   SWAP MOD ;

{ --------------------------- Words to create a bmp file in memory ----------------------- }


: Make-Memory-bmp  ( x y  -- addr )        { Create 24 bit (RGB) bitmap in memory          }
  0 Locals| bmp-addr y-size x-size |
  x-size y-size * 3 * 54 +                 { Find number of bytes required for bmp file    }
  chars allocate                           { Allocate  memory = 3 x size + header in chars }
  drop to bmp-addr
  bmp-addr                                 { Set initial bmp pixels and header to zero     }
  x-size y-size * 3 * 54 + 0 fill

  { Create the bmp file header block }

  66 bmp-addr  0 + c!                      { Create header entries - B                     }
  77 bmp-addr  1 + c!                      { Create header entries - M                     }
  54 bmp-addr 10 + c!                      { Header length of 54 characters                } 
  40 bmp-addr 14 + c!   
   1 bmp-addr 26 + c!
  24 bmp-addr 28 + c!                      { Set bmp bit depth to 24                       }
  48 bmp-addr 34 + c!
 117 bmp-addr 35 + c!
  19 bmp-addr 38 + c!
  11 bmp-addr 39 + c!
  19 bmp-addr 42 + c!
  11 bmp-addr 43 + c!
 
  x-size y-size * 3 * 54 +                 { Store file length in header as 32 bit Dword   }
  bmp-addr 2 + !
  x-size                                   { Store bmp x dimension in header               }
  bmp-addr 18 + ! 
  y-size                                   { Store bmp y dimension in header               }
  bmp-addr 22 + ! 
  bmp-addr                                 { Leave bmp start address on stack at exit      }
  ;


{ --------------------------- Basic Words to Color bmp Pixels -----------------------------}


: Reset-bmp-Pixels  ( addr -- )    { Set all color elements of bmp at addr to zero = black }
  dup 54 + swap
  2 + @ 54 - 0 fill
  ;


: Random_BMP_Green   ( addr -- )                          { Set bmp to random green pixels }
  Reset-bmp-Pixels
  bmp-length @ 55 do
  256 Rnd bmp-address @ 0 + i + c!
  3 +loop ;


{ ------------------------------------- Basic File IO Words  ------------------------------ }


: temp-directory                     { leave start address + length of string on stack      }
  s" c:\temp\"                       { for bmp directory path                               }
  ;


: default-bmp-name                   { leave start address + length of string on stack      }
  s" bmp_output_0001.bmp"            { for bmp name                                         }
  ;


: default-bmp-path ( -- addr n )     { leave start address + length of string on stack      }
  s" c:\temp\bmp_output_0001.bmp"    { for bmp path + file name                             }
  ;


: save-bmp-file   ( -- )             { Save a bmp file starting at bmp-address to disk      }
  bmp-address dup @ swap             { using default bmp-path Will overwrite files          }
  @ 2 + @
  default-bmp-path 
  R/W create-file drop dup pad !
  write-file 
  0= if    cr    s" File successfuly saved " type cr
     else  cr    s" File not saved " type cr
     then
  pad @ close-file drop
  ;



{ ---------------------------------- Stand Alone Test Routines --------------------------- }


 : Setup-Test-Memory                                { Create bmps in memory to start with   }
   bmp-x-size @ bmp-y-size @ make-memory-bmp
   bmp-address ! 
   cr ." Created Test bmp " cr
   ;



{ ----------------------------- Run Test Output Routines -------------------------------- }


: go                     { Demo to save a bmp file as c:\temp\bmp_output_0001.bmp }
  Setup-Test-Memory
  bmp-address @ Random_BMP_Green
  save-bmp-file 
  ;





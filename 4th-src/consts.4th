 ;; print- and read- base -------------------------------------------

 : !base   => base ;
 : decimal #10r10 !base ;
 : hex     #10r16 !base ;
 : octal   #10r8  !base ;
 : binary  #10r2  !base ;
 : print-with-radix ( v r -- )
      base swap !base swap . !base ;
 : .decimal #10r10 print-with-radix ;
 : .hex     #10r16 print-with-radix ;
 : .octal   #10r8  print-with-radix ;
 : .binary  #10r2  print-with-radix ;

 decimal

 ;; constants -------------------------------------------------------

  : constant ( val -- )
      (const) define-word ;

 -7 constant -7
 -6 constant -6
 -5 constant -5
 -4 constant -4
 -3 constant -3
 -2 constant -2
 -1 constant -1
  0 constant 0
  1 constant 1
  2 constant 2
  3 constant 3
  4 constant 4
  5 constant 5
  6 constant 6
  7 constant 7
 -1.0 acos constant pi
  1.0 asin constant pi/2
  1.0 atan constant pi/4
  
 : 1+ 1 + ;
 : 1- 1 - ;
          
 ;; variables -------------------------------------------------------

 : variable     ( v -- )
     1vec constant ;

 0 variable r0  0 variable r1  0 variable r2  0 variable r3

 : ?  ( v -- )
     @ . ;
 : exch  ( v addr -- v' )
      dup @ -rot ! ;
 ;; : sw!  swap ! ;
 : ov!  over ! ;
 : swov swap over ;
 : ovsw over swap ;
 : incr  ( addr -- )
      dup @ 1+ sw! ;
 : decr  ( addr -- )
      dup @ 1- sw! ;
 : +!   ( n addr -- )
      swap-over @ + sw! ;
 : -!   ( n addr -- )
      swap neg swap +! ;
 : @++ dup @ dup 1+ rot ! ;
 : --@ dup @ 1- dup rot ! ;
 : 0! 0 sw! ;

 ;; printing and introspection -------------------------------------

 code inspect (inspect sp@+) }
 code cr      (terpri) }
 code space   (princ #\space) }
 code cls     (!sp nil) }
 
 code (show) (decompile sp@+) }
 : show ' (show) ;

 code .s
    (with-users-base
      (dolist (item (reverse sp))
        (write item)
        (princ #\space))
      (princ "<-Top")) }


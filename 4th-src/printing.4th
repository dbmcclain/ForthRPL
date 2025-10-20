;; --------------------------------------------------------------

 : spaces    0 do space loop ;

 : "  #\" word
     compiling @ if compile literal , then ; immediate

 : ?compile compiling @ if rtos)+ , then ;

 : ."     [compile] " ?compile . ; immediate

 code error
     (report-error sp@+) }

 : error" [compile] " ?compile error ; immediate

;; --------------------------------------------

: .base
       base
       case { #10r16 =  } { ." Hex"     }
            { #10r10 =  } { ." Decimal" }
            { #10r8  =  } { ." Octal"   }
            { #10r2  =  } { ." Binary"  }
            otherwise     { base >base< decimal . }
       esac ;

: verify-stack-empty
     depth 0/= if cr .s error"   Something is dirty..." then ;

;; --------------------------------------------

0 variable tstvar

: tst-unw
    << tstvar 511. >> dyn-bind
   base tstvar 2 ->lst . cr
   if error" Wjat!!" then ;


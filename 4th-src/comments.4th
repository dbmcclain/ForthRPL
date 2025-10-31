;; --------------------------------------------
;; Proper Comments & Conditional Compilation

-- Flow Control -----------------------------

code rdrop    (pop *rstack*) }  ;; we shall return to *Your* caller, instead of to you.
: maybe?      if r> continuation then rdrop ;

-- TCO --------------------------------------------

code (jmp)
  (!ip ip@+) }
' (jmp) mark-skip

code @cfa
    (setf tos (beh-of tos)) }

code @ifa
    (setf tos (icode-of tos)) }

: known-colon ;
' known-colon @cfa constant colon-beh

: jmp  ' dup @cfa colon-beh 
	eq if compile (jmp) @ifa then , ; immediate

-- Generalized Comments Recognizer ---------------

: skipper ;
: stest  over string= dup if drop then ;
: ?>     stest jmp maybe? ;
: skip-to-word  ( word-delim -- )
       begin
          bl-word over stest
       not while
	  skipper
       repeat drop ;
 : #|          "|#"  skip-to-word ; immediate
 : skip-to-fi  "FI#" skip-to-word ;
 : skip-quote  #\" word drop ;

;; MAYBE? evaluates each sub-block in sequence until it finds a non-null
;; result from the test. At that point, it performs the continuation (to 
;; the right of ?>) and then exits the entire block, back to the caller
;; of SKIPPER.

{ { "#|"      ?> [compile] #|  }
  { ";;"      ?> [compile] ;;  }
  { ";;;"     ?> [compile] ;;; }
  { "--"      ?> [compile] --  }	
  { "("       ?> [compile] (   }
  { "#+IF"    ?> skip-to-fi    }
  { "#-IF"    ?> skip-to-fi    }
  { "\""      ?> skip-quote    }
  { ".\""     ?> skip-quote    }
  { "s\""     ?> skip-quote    }
  { "error\"" ?> skip-quote    }
  drop }
' skipper patch


 code feature?
     (setf tos (member tos *features*)) }
     
 : #-IF   if skip-to-fi then ; immediate
 : #+IF   not [compile] #-IF ; immediate
 : FI# ; immediate
 
 nil #+IF diddly dodah! FI#
 
 

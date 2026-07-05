
 ;; indefinite looping -----------------------------------------------

 code mark-skip
   #+:LISPWORKS
   (sys:atomic-push spop *skip-words*)
   #-:LISPWORKS
   (push spop *skip-words*) }
 
 code here 
	(spush (arena-last-pos %cur-icode%)) }

 code backpatch
  (let* ((location  spop)
         (addr      spop))
    (setf (car location) addr)) }

 code (br)
  (lea ip (@ ip ]) ) }
 ' (br) mark-skip

 ;; begin .. end
 ;; begin .. again
 : begin     compile-cnop here ; immediate
 : end       compile (br) , ; immediate
 : again     [compile] end ; immediate

 ;; conditionals ------------------------------------------------------

 code (ift)
  (let ((tclause (@ ip ]+)))
    (when spop
       (execute-word tclause))) }

 code (ifnt)
  (let ((tclause (@ ip ]+)))
    (unless spop
        (execute-word tclause))) }

 code (ifte)
  (let* ((tclause  (@ ip ]+))
         (fclause  (@ ip ]+)))
    (execute-word (if spop tclause fclause)) ) }

 : ift       compile (ift) ; immediate
 : ifte      compile (ifte) ; immediate
 : ifnt      compile (ifnt) ; immediate
 
 code (if)
  (if spop
      (@ ip ]+)
    (lea ip (@ ip ]))) }
 ' (if) mark-skip

 code (ifnot)
  (if spop
      (lea ip (@ ip ]))
    (@ ip ]+)) }
 ' (ifnot) mark-skip

 code (?dup-if)
   (if tos
       (@ ip ]+)
     (progn
       spop
       (lea ip (@ ip ]) ))) }
' (?dup-if) mark-skip

 code nop }

 : compile-nop compile nop ;

  ;; if .. then
  ;; if .. else .. then
 : if        compile (if) compile-nop here ; immediate
 : ifnot     compile (ifnot) compile-nop here ; immediate
 : then      compile-cnop here swap backpatch ; immediate
 : else      compile (br) compile-nop here swap [compile] then ; immediate
 : ?dup-if   compile (?dup-if) compile-nop here ; immediate
 : ;else     compile exit [compile] then ; immediate  -- for "IF xxx ;ELSE yyy ;" then-less programming
 
  ;; begin .. while .. repeat
  ;; begin .. until .. repeat
 : while      [compile] if swap ; immediate
 : until      [compile] ifnot swap ; immediate
 : repeat     [compile] end [compile] then ; immediate
 : ?dup-while [compile] ?dup-if swap ; immediate

 ;; recursion --------------------------------------------------
 
 code cur-word
   (spush  %cur-def%) }
 
 : recurse  cur-word , ; immediate

 : fact     dup 0= if drop 1 else dup 1- recurse * then ;

 code tail
   (lea ip (icode-of (@ ip ]))) }

 : tfact    1 swap { dup 0= if drop else swap over * swap 1- tail recurse then } ;
   
 ;; do-loops ----------------------------------------------------

 code (do)
  (let* ((ix    spop)
         (limit spop))
    (if (= ix limit)
        (lea ip (@ ip ]) )
      (progn
        (rpush limit)
        (rpush ix) 
        (@ ip ]+)))) }
 ' (do) mark-skip

 code (loop)
  (let ((ix (1+ rtos)))
    (if (< ix (cadr rp))
        (progn
          (!rtos ix)
          (lea ip (@ ip ]) ))
      (progn
        (lea rp (cddr rp))
        (@ ip ]+))
      )) }
 ' (loop) mark-skip

 code (+loop)
  (let* ((incr spop)
         (ix   (+ incr rtos)))
    (if (minusp incr)
        (if (>= ix (cadr rp))
            (progn
              (!rtos ix)
              (lea ip (@ ip ]) ))
          (progn
            (lea rp (cddr rp))
            (@ ip ]+)))

      (if (< ix (cadr rp))
          (progn
            (!rtos ix)
            (lea ip (@ ip ]) ))
        (progn
          (lea rp (cddr rp))
          (@ ip ]+))))
    ) }
 ' (+loop) mark-skip

 : do     compile (do) compile-nop here compile-cnop here ; immediate
 : loop   compile (loop) , [compile] then ; immediate
 : +loop  compile (+loop) , [compile] then ; immediate

 code leave
   (!rnos rtos) }
   
 ;; code replacement --------------------------------------------------

code patch
  ;; very un-Forth-like
  (let* ((wd    spop)
         (icode spop))
    (setf (icode-of wd) (icode-of icode))) }

code patch-behavior
   ;; ... just because we can...
   (let* ((wd    spop)
          (ccode spop))
     (setf (beh-of wd) (beh-of ccode))) }

;; --------------------------------------------

: ['] ' compiling @ if compile literal , then ; immediate
: [,] compiling @ if compile literal then , ; immediate

: =>
   compiling @ if compile =>
   else ' code{ (to-oper tos nos)
                (lea sp (cddr sp)) }
   then ; immediate

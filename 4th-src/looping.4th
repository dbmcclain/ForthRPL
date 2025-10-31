
 ;; indefinite looping -----------------------------------------------

 code mark-skip
   #+:LISPWORKS
   (sys:atomic-push sp@+ *skip-words*)
   #-:LISPWORKS
   (push sp@+ *skip-words*) }
 
 code here 
	(sp-! (arena-last-pos %cur-icode%)) }

 code backpatch
  (let* ((location  sp@+)
         (addr      sp@+))
    (setf (car location) addr)) }

 code (br)
  (!ip ip@) }
 ' (br) mark-skip

 ;; begin .. end
 ;; begin .. again
 : begin     compile-cnop here ; immediate
 : end       compile (br) , ; immediate
 : again     [compile] end ; immediate

 ;; conditionals ------------------------------------------------------

 code (ift)
  (let ((tclause ip@+))
    (when sp@+
       (execute-word tclause))) }

 code (ifnt)
  (let ((tclause ip@+))
    (unless sp@+
        (execute-word tclause))) }

 code (ifte)
  (let* ((tclause  ip@+)
         (fclause  ip@+))
    (execute-word (if sp@+ tclause fclause)) ) }

 : ift       compile (ift) ; immediate
 : ifte      compile (ifte) ; immediate
 : ifnt      compile (ifnt) ; immediate
 
 code (if)
  (if sp@+
      ip@+
    (!ip ip@)) }
 ' (if) mark-skip

 code (ifnot)
  (if sp@+
      (!ip ip@)
    ip@+) }
 ' (ifnot) mark-skip

 code (?dup-if)
   (if tos
       ip@+
     (progn
       sp@+
       (!ip ip@))) }
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
 
  ;; begin .. while .. repeat
  ;; begin .. until .. repeat
 : while      [compile] if swap ; immediate
 : until      [compile] ifnot swap ; immediate
 : repeat     [compile] end [compile] then ; immediate
 : ?dup-while [compile] ?dup-if swap ; immediate

 ;; recursion --------------------------------------------------
 
 code cur-word
   (sp-!  %cur-def%) }
 
 : recurse  cur-word , ; immediate

 : fact     dup 0= if drop 1 else dup 1- recurse * then ;

 code tail
   (setf ip (icode-of ip@)) }

 : tfact    1 swap { dup 0= if drop else swap over * swap 1- tail recurse then } ;
   
 ;; do-loops ----------------------------------------------------

 code (do)
  (let* ((ix    sp@+)
         (limit sp@+))
    (if (= ix limit)
        (!ip ip@)
      (progn
        (rp-! limit)
        (rp-! ix)
        ip@+))
    ) }
 ' (do) mark-skip

 code (loop)
  (let ((ix (1+ rp@)))
    (if (< ix (cadr rp))
        (progn
          (setf rp@ ix)
          (!ip ip@))
      (progn
        (!rp (cddr rp))
        ip@+)
      )) }
 ' (loop) mark-skip

 code (+loop)
  (let* ((incr sp@+)
         (ix   (+ incr rp@)))
    (if (minusp incr)
        (if (>= ix (cadr rp))
            (progn
              (setf rp@ ix)
              (!ip ip@))
          (progn
            (!rp (cddr rp))
            ip@+))

      (if (< ix (cadr rp))
          (progn
            (setf rp@ ix)
            (!ip ip@))
        (progn
          (!rp (cddr rp))
          ip@+)))
    ) }
 ' (+loop) mark-skip

 : do     compile (do) compile-nop here compile-cnop here ; immediate
 : loop   compile (loop) , [compile] then ; immediate
 : +loop  compile (+loop) , [compile] then ; immediate

 code leave
   (setf rnos rtos) }
   
 ;; code replacement --------------------------------------------------

code patch
  ;; very un-Forth-like
  (let* ((wd    sp@+)
         (icode sp@+))
    (setf (icode-of wd) (icode-of icode))) }

code patch-behavior
   ;; ... just because we can...
   (let* ((wd    sp@+)
          (ccode sp@+))
     (setf (beh-of wd) (beh-of ccode))) }

;; --------------------------------------------

: ['] ' compiling @ if compile literal , then ; immediate
: [,] compiling @ if compile literal then , ; immediate

: =>
   compiling @ if compile =>
   else ' code{ (to-oper tos nos)
                (setf sp  (cddr sp)) }
   then ; immediate

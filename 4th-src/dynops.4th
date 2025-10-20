;; --------------------------------------------
;; Special Return Stack Ops

code rtos)+
   ;; when performed in a colon-def, assuming that def was called by
   ;; another colon-def, this fetches the next ip-code op from caller,
   ;; and advances the return address.
   (sp-! (pop rp@)) }

code >r<
   ;; when performed in a colon-def, assuming that def was called by
   ;; another colon-def, this swizzles the ip and return address, to
   ;; resume the caller and set up to resume ourself on its exit
   (rotatef rtos ip) }
   
code <<r
   ;; stash stack item beneath ret addr
   ;; equiv of inline: r> swap <r <r
   (rp-! rtos)
   (setf rnos sp@+) }

code sp><rp
   ;; exchange TOS with RTOS
   (rotatef tos rtos) }
   
code .r
  (inspect rp) }

code (>>r<<)
    (rotatef rtos rnos) ;; rswap
    (!ip sp@+) }
    
: >>r<<   ( fn -- )
    ;; Expect an ip continuation on stack. Re-order return addrs, to
    ;; make continuation function happen first, then pop-prot, then
    ;; restoring code in caller of this function.
    ;;
    ;; Used by words that guard other words with protect-unwind.
    ;;
    ;; Same as: r> r> swap <r <r <r
    (>>r<<) ;

;; "r> continuation" does the same thing as >r<
: continuation  <r ;

;; --------------------------------------------

: >base<
     ;; Save base, perform caller's code, restore base on its exit.
     r> base <r
     continuation
     r> !base ;

;; --------------------------------------------
;; Unwind-Protect

code (protect)
    (forth-protect (cdr rp@)) }

code (pop-prot)
   (let ((state up@+))
     (assert (prot-frame-p state))
     (restore-state state)) }
     
: protect
    ;; Protect one word following the use of protect in the caller's code.
    ;; All the rest of the caller's code will be used as unwind code.
    (protect)
    r> continuation
    (pop-prot) ;
    
;; --------------------------------------------
;; Rebinding Dynvars - Closer to real dynamic binding.
;;
;; Uses an FPL Red-Black Tree to contain dynvars. Copying all bindings
;; is as simple as making a copy of the tree root pointer.
;;
;; Updates cause the tree to non-destructively morph into a different
;; tree.  The prior version remains if anyone has a binding to it.
;; O(Log2 N) lookup times.
;;
;; REBINDING takes a quotation whose list of i-codes should be
;; dynvars. The current dyn-tree is duplicated on the UP stack, and
;; each dynvar in the quotation has its binding copied. Any mutation
;; to these will affect only the copy, not the original binding.
;;

code (rebinding)
  (up-! (make-dynvar-sav                     ;; save current bindings for restore
         :vars *dynvars*))
  (push dynvar-tree *dynvars*) }             ;; copy bindings into new context

code (pop-rebindings)
  (let ((sav up@+))
    (assert (dynvar-sav-p sav))
    (setf *dynvars* (dynvar-sav-vars sav))) }

: with-new-dynvars
    (rebinding)
    r> continuation
    (pop-rebindings) ;

;; --------------------------------------------
;; Example REBINDING:
;; The following is roughly equivalent to:
;;
;;   (defvar x 0)  ;; <-- x, y, z are "Special" bindings
;;   (defvar y 0)
;;   (defvar z 0)
;;
;;   (let ((x  x)  ;; rebinding of specials
;;         (y  y)
;;         (z  z))
;;      (setf x  5  ;; mutation of rebound specials
;;            y 15  ;; does not affect the outer global bindings
;;            z 32)
;;      ...
;;     )

code .u
   (inspect (list up *dynvars*)) }

0 dynvar x  0 dynvar y  0 dynvar z

: .vars
    << x y z >>vec . ;
    
: tst-reb
   with-new-dynvars
   .vars cr
   .u
    5 => x
   13 => y
   32 => z
   .vars ;
   
;; --------------------------------------------
;; Replacement Bindings for Forth - change a var and have it restored on exit.

code (dyn-bind)
  ;; construct a dyn-restore list, exchange with RTOS to place
  ;; the restore struct on the r-stack, and the return addr on the p-stack.
  ;; Will be used by the following [ PROTECT >>R<< ].
  (nlet iter ((lst  sp@+)
              (acc  nil))
    (if (endp lst)
        (up-! (make-dynbind-sav :lst acc))
      (destructuring-bind (var val . rest) lst
        (go-iter rest
                 (list* var
                        (shiftf (@fcell var) val)
                        acc)))
      )) }

code (dyn-restore)
   ;; Expect a dyn-restore list on top of R-stack.
   ;; Restore the vars in the list, popping the r-stack.
   (restore-dynbinds up@+) }
   
;;; : dyn-bind ( vec -- sav )
;;;     (dyn-bind) r>
;;;     protect
;;;     >>r<<
;;;     (dyn-restore) ;

: dyn-bind ( lst -- sav )
    (dyn-bind)
    r> continuation
    (dyn-restore) ;


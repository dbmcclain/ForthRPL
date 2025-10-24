;; forth-itc-rpl-forthcode.lisp - Forth System Bootstrap Code
;;
;; This file is intended to contain only Forth definitions. Any Lisp
;; code should be for constructive purposes only, not for definitional
;; bindings
;; ------------------------------------------------------------------
(in-package #:forthrpl)
;; ------------------------------------------------------------------
;;
;; A Forth "Word" in the "Dictionary"
;; ----------------------------------
#|                         
   ┌─  Generic Word   ─┐    
   │                   │    
   │  ┌─────────────┐  │    
   │  │     NFA     ├──┼───▶ Name, string or symbol
   │  └─────────────┘  │    
   │  ┌─────────────┐  │    
   │  │     LFA     ├──┼───▶ Link, points to prior word in dictionary
   │  └─────────────┘  │    
   │  ┌─────────────┐  │    
   │  │     CFA     ├──┼───▶ Code, points to word's behavior (Lisp code)
   │  └─────────────┘  │    
   │  ┌─────────────┐  │    
   │  │     DFA     ├──┼───▶ Data, contains assoc data, if any
   │  └─────────────┘  │    
   │  ┌─────────────┐  │    
   │  │     IFA     ├──┼───▶ ICode list, if any
   │  └─────────────┘  │    
   └───────────────────┘
|#
;; All words have NFA, LFA, and the CFA behavior, slots.
;;
;; Some words may not have DFA or IFA slots. E.g., CODE and :-DEFS
;; have no DFA. CODE, ;CODE, and CONSTANT have no IFA. ;:-DEFS have
;; both DFA and IFA.
;;
;; Some of the CFA slots may exist as shared behavior in Class
;; allocated slots, instead of per-Instance slots. E.g., CFA in the
;; case of :-DEFS, CONSTANTS, and most others. CODE and ;CODE defs
;; have custom CFA (per-Instance) behaviors.
;;
;; In the case of CONSTANT's the DFA contains the value.  For
;; VARIABLES, the DFA points to a vector of Lisp values, with length
;; at least one. ARRAYS would have longer vectors.
;;
;; The , (comma operation) grows the last def's data vector and plants
;; the TOS value in the next ascending position of the vector.
;;
;; IFA exists only in higher level defs, like :-DEFS and ;:-DEFS.
;; The ICode list is a Lisp list of Forth Words like these.
;;
;; System DEF Classes are provided for specific patterns of behavior:
;;
;;  <CODE-DEF>       - used by CODE defs
;;
;;  <SCODE-DEF>      - used by ;CODE defs. On entry the WP register points
;;                     to the word so that you can access its DFA value.
;;
;;  <VOCABULARY>     - used by VOCABULARY defs
;;
;;  <CONSTANT>       - used by all CONSTANT and VARIABLE defs
;; 
;;  <COLON-DEF>      - used by all high-level : defs
;;
;;  <SCOLON-DEF>     - used by ;: defining words
;;
;;  <LOCAL-ACCESSOR> - references to local lexical bindings
;;
;;  <DYNVAR>         - akin to Lisp Special Bindings (dynamic vars)
;;
;; All behaviors are executed with one parameter - a pointer to the
;; Word itself, so that other associated slots may be accessed by the
;; behavior code.
;;
;; ForthRPL does *not* use a linear memory layout. It makes use of
;; Lisp's dynamically allocated, garbage collected, data structures.
;;
;; But within one VARIABLE or ARRAY DFA vector, we do have linear
;; memory.  And within that data vector, memory is accessed by ordinal
;; integer index.
;;
;; We have STACKS!
;;
;;   SP - parameter stack
;;   RP - return stack
;;   FP - frame stack,  for when we use local lexical bindings
;;   UP - dynamic stack, used for special bindings, and unwind protect
;;        frames
;;
;; Internally used registers:
;;
;;   IP - points to some NTHCDR of the currently executing ICode list
;;   WP - points to the currently executing Word
;;
;; This interpreter executes a facsimile of ITC Forth, who's low-level
;; machine code, reached within CODE and ;CODE defs, is Lisp.
;;
;; ------------------------------------------------------
;;
;; System State
;;
;;   - IP
;;   - WP
;;   - RP
;;   - SP
;;   - FP
;;   - UP

(initialize)

;; --------------------------------------------
;; Adding VOCABULARY FORTH - first word in the dictionary

(let* ((pair (vector nil nil))
       (v  (make-instance '<vocabulary>
                         :nfa "FORTH"
                         :lfa nil
                         :dfa pair
                         )))
  (setf (aref pair 0) v)
  (vector-push-extend v *dict*)
  (setf *tic-forth*   v
        (current-voc) v
        (context-voc) v))

;; --------------------------------------------
;; Adding DYNVAR BASE

(let ((v (link-derived-word '<dynvar>
                            :nfa "BASE")))
  (setf *tic-base*  v)
  (add-dynvar v 10.)
  (save-dynvars))

;; --------------------------------------------

(const nil       nil)
(const t         t)
(const bl        #\space)
(const newline   #\newline)

;; --------------------------------------------

(code context
  (sp-! *context*))

(code current
  (sp-! *current*))

(code compiling
  (sp-! *compiling*))

;; --------------------------------------------

(code literal
  (sp-! ip@+))

(code exit
  (!ip rp@+))

;; CNOP -- a replaceable nop we need a placeholder in some cases but
;; when compiled into an i-code list this will be replaced by the next
;; i-code to be compiled.
(code cnop)

(setf *tic-lit*   (must-find 'literal)
      *tic-exit*  (must-find 'exit)
      *tic-cnop*  (must-find 'cnop))

;; -------------------------------------------------------------------
(define-unary-ops abs sin cos tan asin acos atan log exp sqrt sinh cosh tanh asinh acosh atanh cis
                  (inv /) (neg -) (com lognot) realpart imagpart phase
                  truncate floor ceiling round
                  (float dfloat) not car cdr cadr cddr caddr cadddr oddp evenp null
                  first second third rest endp length reverse
                  type-of
                  (int? integerp) (float? floatp) (rational? rationalp)
                  (real? realp) (complex? complexp) (number? numberp)
                  (string? stringp) (char? characterp)
                  (list? listp) (cons? consp) (vector? vectorp)
                  (0= zerop) (0< minusp) (0> plusp) (0/= not-zerop))

(define-binary-ops + - * / expt (logb log) (atan2 atan) ash
                   complex mod rem (truncate-by truncate) (floor-by floor)
                   (ceiling-by ceiling) (round-by round)
                   = < <= >= > /= eq equal eql equalp max min
                   (and logand) (or logior) (xor logxor) aref cons)
;; -------------------------------------------------------------------

(code skip-to-eol
  (skip-to-eol))
  
(code word
  (let* ((delim tos)
         (w     (next-word delim)))
    ;; (format t "~&word = ~S" w)
    (setf tos w)))

(code find
  (sp-! (forth-lookup tos)))

(code quit
  (throw 'done sp))

(code find-or-quit
  (let ((name tos))
    ;; (format t "~%     ~A" name)
    (if name
        (sp-! (forth-lookup name))
      (throw 'done (cdr sp))
      )))

(code must-find
  (let ((name sp@+))
    (if name
        (sp-! (must-find name))
      (report-error " EOF error"))))

(code interpret
  (let* ((ans  sp@+)
         (arg  sp@+))
    (if ans
        (forth-handle-found ans)
      (forth-handle-not-found arg)) ))

(colon bl-word
       bl word)

(code outer-again
  (!ip (icode-of *tic-outer*)))

(colon outer
       bl-word find-or-quit interpret outer-again)
(setf *tic-outer* (must-find 'outer))

;; -----------------------------------------------

(code create-{
  (sp-! (derive-word '<colon-def>)))

(code create-code
  (setf tos
        (derive-word '<code-def>
                     :cfa (compile-lisp-text tos)) ))

(code def
  (let* ((name sp@+)
         (w    sp@+))
    (setf (name-of w) name)
    (link w)
    ))

(code immediate
  (immediate))

(code ","
  (forth-compile-in sp@+))

(code compile
  (forth-compile-in ip@+))

(code swap
  (rotatef tos nos))

(code drop
  sp@+)

(code @
  (setf tos (@fcell tos)))

(code !
  (let* ((loc sp@+)
         (val sp@+))
    (setf (@fcell loc) val)))

(code =>
  (to-oper ip@+ sp@+))

;; -------------------------------------------------------------
;; try supporting nested compiles...

(code push-compile-context
  (push (new-frame) *display*))

(code import-icode
  (setf (icode-of %cur-def%) (arena-get %cur-icode%)))

(code pop-compile-context
  (when (toplevel?)
    (report-error " Compile context state error: ~A" (name-of (last-def))))
  (let ((curdef %cur-def%)) ;; cur-def is stored in the current frame
    (pop *display*)
    (if (toplevel?)
        (setf %cur-def% curdef) ;; copy down to previous state
      ;; else
      (progn
        (setf (compiling?) t)
        (forth-compile-in sp@+))
      )))

;; -------------------------------------------------------------
;;
;; define the minimum number of colon definitions needed to get us going
;;

(colon !compiling
       compiling ! )
       
(colon [
       nil !compiling )
(immediate)

(colon ]
       t !compiling )

(colon set-current-context
       current @ context ! )

(colon {
       push-compile-context
       create-{
       set-current-context ] )
(immediate)

(colon }
        compile exit
        [  import-icode
        set-current-context
        pop-compile-context )
(immediate)

;; ---------------------------------------------------

(colon compile-cnop
       ;; this is needed because doing it in Forth ends up
       ;; replacing the CNOP after a "... compile cnop ... " construct
       compile cnop)

;; ---------------------------------------------------------
;; ... the rest directly in Forth...
;; ---------------------------------------------------------

(inhale "base.4th")

;; --------------------------------------------


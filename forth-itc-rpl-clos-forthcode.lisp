;; forth-itc-rpl-forthcode.lisp -- this file is intended to contain only Forth definitions
;; Any Lisp code should be for constructive purposes only, not for definitional bindings
;; ------------------------------------------------------------------
(in-package #:forthrpl)
;; ------------------------------------------------------------------

;; ------------------------------------------------------
;; forth

(initialize)

;; VOCABULARY FORTH
(let* ((v   (vocabulary forth))
       (dfa (fw-dfa v)))
  ;; (!fcell dfa v)
  (!fcell *context* dfa)
  (!fcell *current* dfa))

(const nil       nil)
(const t         t)
(const bl        #\space)
(const newline   #\newline)

(code context
  (push *context* *pstack*))

(code current
  (push *current* *pstack*))

(code compiling
  (push *compiling* *pstack*))

(code base
  (push *base* *pstack*))
  
(code literal
  (push (pop *reg-i*) *pstack*))

(code exit
  (setf *reg-i* (pop *rstack*)))

;; CNOP -- a replaceable nop
;; we need a placeholder in some cases
;; but when compiled into an i-code list
;; this will be replaced by the next i-code
;; to be compiled.
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
                   (and logand) (or logior) (xor logxor) aref)
;; -------------------------------------------------------------------

(code word
  (let* ((delim (car *pstack*))
         (w     (next-word delim)))
    ;; (format t "~&word = ~S" w)
    (setf (car *pstack*) w)))

(code find
  (push (forth-lookup (car *pstack*)) *pstack*))

(code quit
  (throw 'done *pstack*))

(code find-or-quit
  (let ((name (car *pstack*)))
    ;; (format t "~%     ~A" name)
    (if name
        (push (forth-lookup name) *pstack*)
      (throw 'done (cdr *pstack*))
      )))

(code must-find
  (let ((name (car *pstack*)))
    (if name
        (setf (car *pstack*) (must-find name))
      (report-error " EOF error"))))

(code interpret
  (destructuring-bind (ans arg . rest) *pstack*
    (setf *pstack* rest)
    (if ans
        (forth-handle-found ans)
      (forth-handle-not-found arg)) ))

(colon bl-word
       bl word)

(colon outer
       bl-word find-or-quit interpret )
(setf *tic-outer* (must-find 'outer))

;; -----------------------------------------------

(code create-{
  (push (derive-word '<colon-def>) *pstack*))

(code create-code
  (setf (car *pstack*)
        (derive-word '<code-def>
                     :cfa (compile-lisp-text (car *pstack*))) ))

(code def
  (destructuring-bind (name w . rest) *pstack*
    (setf (fw-nfa w) name
          *pstack*   rest)
    (link w)
    ))

(code immediate
  (immediate))

(code ","
  (forth-compile-in (pop *pstack*)))

(code compile
  (forth-compile-in (pop *reg-i*)))

(code swap
  (setf *pstack* (roll 1 *pstack*)))

(code drop
  (pop *pstack*))

(code @
  (setf (car *pstack*) (aref (car *pstack*) 0)))

(code !
  (destructuring-bind (loc val . rest) *pstack*
    (setf (aref loc 0) val
          *pstack*     rest)))

;; -------------------------------------------------------------
;; try supporting nested compiles...

(code push-compile-context
  (push (make-nested-frame) *display*))

(code import-icode
  ;; (setf (fw-ifa %cur-def%) (funcall %cur-icode% :get))
  (setf (fw-ifa %cur-def%) (arena-get %cur-icode%)))

(code pop-compile-context
  (when (toplevel?)
    (report-error " Compile context state error: ~A" (fw-nfa (last-def))))
  (let ((curdef %cur-def%)) ;; cur-def is stored in the current frame
    (pop *display*)
    ;; (inspect curdef)
    (if (toplevel?)
        (setf %cur-def% curdef) ;; copy down to previous state
      ;; else
      (progn
        (set-compile t)
        (forth-compile-in (pop *pstack*)))
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
        ;; try this out... at the end of a definition
        ;; the context is reset to current. Vocabulary switching
        ;; inside the definition does not carry forward after the
        ;; end of the new definition.
        compile exit [
        import-icode
        pop-compile-context
        set-current-context )
(immediate)

;; ---------------------------------------------------

(colon compile-cnop
       ;; this is needed because doing it in Forth ends up
       ;; replacing the CNOP after a "... compile cnop .. " construct
       compile cnop)

;; ---------------------------------------------------------
;; ... the rest directly in Forth...
;; ---------------------------------------------------------

(interpret #1>.end
 { bl-word def } 'define-word def
 { #\newline word drop }  define-word ;; immediate  ;; we now have comments to end of line...
 { #\) word drop }        define-word ( immediate   ( we now have embedded comments )
 { }                      define-word ) immediate   ( for confused Lisp reader )
 { bl-word must-find }    define-word '                    ( -- verb )
 { ' , }                  define-word [compile] immediate  ( -- ) ;; valid only in compile mode
 { bl-word [compile] { }  define-word :                    ( -- name verb -> compile mode )
 { [compile] } swap def } define-word ; immediate          ( name verb -- ) ;; ends compile mode
 
 : -- [compile] ;; ; immediate
 
 ;; at this point we can do colon defs ---------------------------------
 
 : }-word #\} word ;
 : code{ }-word
         push-compile-context
         create-code
         pop-compile-context ; immediate
 : code bl-word [compile] code{ swap def ;

 code ?immed ;; ( wd -- t/f )
   (setf (car *pstack*) (is-immed (car *pstack*))) }
   
 ;; now we can do code defs --------------------------------------------
 ;; so generate the inner workings for defining-words
 
 code (const)
   (setf (car *pstack*)
         (derive-word '<constant>
                      :dfa (car *pstack*))) }

 code (;code)
  (let ((behav (fw-cfa (pop *pstack*))))
    (setf (car *pstack*)
          (derive-word '<scode-def>
                       :cfa (lambda (self)
                              (doval self)
                              (funcall behav self))
                       :dfa (car *pstack*))
          *reg-i*  (pop *rstack*)
          )) }
  
 ) ;; Lparen for confused Lisp reader
 
 : ;code{ compile literal
          [compile] code{
          compile (;code)
          [compile] ;
          ; immediate
          
  ) ;; another Lparen for confused Lisp reader
          
 code ;:
  ;; stack contains data
  ;; this performs EXIT and the follow i-code
  ;; will be the actions for the newly defined verb
  (setf (car *pstack*)
        (derive-word '<scolon-def>
                     :dfa (car *pstack*)
                     :ifa *reg-i*)
        *reg-i* (pop *rstack*)
        ) }

 ;; handy access to the host environment... ---------------------------
 
 code lisp
  (let ((e  (read-from-string (pop *pstack*))))
    (funcall (compile nil `(lambda () ,e)))) }

 : lisp{ }-word lisp ;

 ;; various stack manipulation verbs -----------------------------------
 
 code execute
  (execute-word (pop *pstack*)) }

 code (jmp)
     (do-jmp (pop *reg-i*)) }
  
 code <r
      (push (pop *pstack*) *rstack*) }

 code r>
      (push (pop *rstack*) *pstack*) }

 code i
      (push (car *rstack*) *pstack*) }

 code j
      (push (third *rstack*) *pstack*) }

 code over
  (destructuring-bind (a b . rest) *pstack*
    (setf *pstack* (list* b a b rest))) }

 code swap-over
  (destructuring-bind (a b . rest) *pstack*
    (setf *pstack* (list* a b a rest))) }

 code dupnos
  (destructuring-bind (a b . rest) *pstack*
    (setf *pstack* (list* a b b rest))) }

 code dup   (push (car *pstack*) *pstack*) }
 code roll  (setf *pstack* (roll (pop *pstack*) *pstack*)) }
 code rot   (setf *pstack* (roll 2 *pstack*)) }
 code -rot  (setf *pstack* (roll -2 *pstack*)) }
 
 code ndrop
  (let ((n (car *pstack*)))
    (setf *pstack* (nthcdr (1+ n) *pstack*))) }

 code 2dup
  (destructuring-bind (a b . rest) *pstack*
    (setf *pstack* (list* a b a b rest))) }

 code 2swap
  (destructuring-bind (a b c d . rest) *pstack*
    (setf *pstack* (list* c d a b rest))) }

 code 2over
  (destructuring-bind (a b c d . rest) *pstack*
    (setf *pstack* (list* c d a b c d rest))) }

 code 2rot
  (destructuring-bind (a b c d e f . rest) *pstack*
    (setf *pstack* (list* e f a b c d rest))) }

 code -2rot
  (destructuring-bind (a b c d e f . rest) *pstack*
    (setf *pstack* (list* c d e f a b rest))) }

 code 2swap-over
  (destructuring-bind (a b c d . rest) *pstack*
    (setf *pstack* (list* a b c d a b rest))) }

 code 2drop
  (setf *pstack* (cddr *pstack*)) }

 code depth
  (push (length *pstack*) *pstack*) }

 code pick
  (let ((n (car *pstack*)))
    (setf (car *pstack*) (nth (1+ n) *pstack*))) }

 code ->lst
  (let ((nel (pop *pstack*)))
    (multiple-value-bind (hd tl)
        (um:split nel *pstack*)
      (setf *pstack* (cons (nreverse hd) tl))
      )) }

 code ->lst*
  (let ((nel (pop *pstack*)))
    (multiple-value-bind (hd tl)
        (um:split nel *pstack*)
      (setf *pstack* (cons (append (nreverse (cdr hd)) (car hd)) tl))
      )) }

 code lst->
   (let ((lst  (pop *pstack*)))
     (setf *pstack* (cons (length lst) (nconc (reverse lst) *pstack*)) )) }

 code ->vec
  (let ((nel (pop *pstack*)))
    (multiple-value-bind (hd tl)
        (um:split nel *pstack*)
      (setf *pstack* (cons (make-array nel
                                     :initial-contents (nreverse hd))
                         tl)))) }

 code vec->
  (let* ((seq  (pop *pstack*))
         (lst  (coerce seq 'list)))
    ;; be careful here... this could also be applied to a list
    ;; in which case the (coerce seq 'list) would return the original argument
    ;; might not be safe to nreverse that original list
    (setf *pstack* (cons (length seq) (nconc (reverse lst) *pstack*)))) }

 code copy-vec
   (setf (car *pstack*) (copy-seq (car *pstack*))) }

 code 1vec
   (setf (car *pstack*) (vector (car *pstack*))) }

 code 2vec
   (let* ((snd  (pop *pstack*))
          (fst  (car *pstack*)))
     (setf (car *pstack*) (vector fst snd))) }

 code string=
 ;; case insensitive
  (let* ((s1 (pop *pstack*))
         (s2 (car *pstack*)))
    (setf (car *pstack*) (string-equal s1 s2))) }

 code /mod
  (destructuring-bind (d n . rest) *pstack*
    (setf *pstack* (nconc (nreverse (multiple-value-list (truncate n d)))
                        rest))) }

 code .
   (with-users-base
    (princ (pop *pstack*))) }


 ;; print- and read- base -------------------------------------------

 : @base   ( -- n ) base @ ;
 : !base   ( n -- ) base ! ;
 : decimal #10r10 !base ;
 : hex     #10r16 !base ;
 : binary  #10r2  !base ;
 : print-with-radix ( v r -- )
      @base swap !base swap . !base ;
 : .decimal #10r10 print-with-radix ;
 : .hex     #10r16 print-with-radix ;
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
 : sw!  swap ! ;
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

 ;; 1-D arrays -------------------------------------------------------

 code allot (setf (car *pstack*) (make-array (car *pstack*))) }

 : array        ( n -- )
     allot constant ;

 code fill     ;; ( v arr -- )
   (destructuring-bind (arr v . rest) *pstack*
       (setf *pstack* rest)
       (fill arr v)) }

 code i@
      (let ((ix (pop *pstack*)))
        (setf (car *pstack*) (aref (car *pstack*) ix))) }

 code i!
  (destructuring-bind (ix loc val . rest) *pstack*
    (setf (aref loc ix) val
          *pstack*      rest)) }

 : fst   ( 2vec -- obj )
    @ ;

 : snd   ( 2vec -- obj )
    1 i@ ;

 : !fst  ( obj 2vec -- )
    ! ;

 : !snd  ( obj 2vec -- )
    1 i! ;
    
 code nth-pair ;;  ( pair n -- pair )
   ;; using 2vecs as pairs (car,cdr), do the pointer chasing to find
   ;; the nth pair.
   (let* ((n     (pop *pstack*))
          (pair  (car *pstack*)))
     (when (plusp n)
       (loop repeat n do
               (setf pair (aref pair 1)))
       (setf (car *pstack*) pair))) }
   
 ;; vocabularies ------------------------------------------------

 code latest
       (push (last-def) *pstack*) }

 : vocabulary   ( -- )
     0 1vec dup { ;: context ! }
     define-word immediate
     latest sw! ;

 : definitions  ( -- )
     context @ current ! ;

 code vlist   (vlist) }

 ;; structs --------------------------------------------------------

 0 variable current-struct-template
 : structure-template  { 0 1vec dup current-struct-template ! ;: @ * } define-word ;
 : field   { current-struct-template @ @++ ;: + } define-word ;

   ( intended to work as:
              structure-template thing
                field t1
                field t2
                field t3
              1 thing 20 * array things
              things 15 thing t2 i@ )

 ;; printing and introspection -------------------------------------

 code inspect (inspect (pop *pstack*)) }
 code cr      (terpri) }
 code space   (princ #\space) }
 code cls     (setf *pstack* nil) }
 
 code (show) (decompile (pop *pstack*)) }
 : show ' (show) ;

 code .s
    (with-users-base
      (dolist (item (reverse *pstack*))
        (write item)
        (princ #\space))
      (princ "<-Top")) }

 ;; indefinite looping -----------------------------------------------

 code mark-skip
   (push (pop *pstack*) *skip-words*) }
 
 ;; code here (push (funcall %cur-icode% :last-pos) *pstack*) }
 code here (push (arena-last-pos %cur-icode%) *pstack*) }

 code backpatch
  (destructuring-bind (location addr . rest) *pstack*
    (setf *pstack* rest
          (car location) addr)) }

 code (br)
  (setf *reg-i* (car *reg-i*)) }
 ' (br) mark-skip

 ;; begin .. end
 ;; begin .. again
 : begin     compile-cnop here ; immediate
 : end       compile (br) , ; immediate
 : again     [compile] end ; immediate

 ;; conditionals ------------------------------------------------------

 code (ift)
  (let ((tclause (pop *reg-i*)))
    (when (pop *pstack*)
        (execute-word tclause))) }

 code (ifnt)
  (let ((tclause (pop *reg-i*)))
    (unless (pop *pstack*)
        (execute-word tclause))) }

 code (ifte)
  (destructuring-bind (tclause fclause . rest) *reg-i*
    (setf *reg-i* rest)
    (execute-word (if (pop *pstack*) tclause fclause)) ) }

 : ift       compile (ift) ; immediate
 : ifte      compile (ifte) ; immediate
 : ifnt      compile (ifnt) ; immediate
 
 code (if)
  (if (pop *pstack*)
      (setf *reg-i* (cdr *reg-i*))
    (setf *reg-i* (car *reg-i*))) }
 ' (if) mark-skip

 code (ifnot)
  (if (pop *pstack*)
      (setf *reg-i* (car *reg-i*))
    (setf *reg-i* (cdr *reg-i*))) }
 ' (ifnot) mark-skip

 code nop }

 : compile-nop compile nop ;


  ;; if .. then
  ;; if .. else .. then
 : if        compile (if) compile-nop here ; immediate
 : ifnot     compile (ifnot) compile-nop here ; immediate
 : then      compile-cnop here swap backpatch ; immediate
 : else      compile (br) compile-nop here swap [compile] then ; immediate

  ;; begin .. while .. repeat
  ;; begin .. until .. repeat
 : while     [compile] if swap ; immediate
 : until     compile (ifnot) compile-nop here swap ; immediate
 : repeat    [compile] end [compile] then ; immediate

 ;; code replacement --------------------------------------------------

 code patch
  ;; very un-Forth-like
  (destructuring-bind (wd icode . rest) *pstack*
    (setf (fw-ifa wd) (fw-ifa icode)
          *pstack*    rest)) }

 : ?dup dup if dup then ;

 ;; now give us a proper outer interpreter
 { begin bl-word ?dup while find interpret repeat quit } ' outer patch

 ;; properties -----------------------------------------------------
 ;; Now that we are Self-like object based, let's add properties to any Forth word...

 : #| ; immediate
 : |# ; immediate  ;; for confused Lisp reader

 
 { begin bl-word
     dup "#|" string=
        if drop [compile] #| [ over ] again then
     dup ";;" string=
        if drop [compile] ;; [ over ] again then
     dup "(" string=
        if drop [compile] (  [ over ] again then -- ) for confused Lisp reader
     dup "--" string=
        if drop [compile] -- [ over ] again then
     "|#" string= if exit then
    again }
 ' #| patch

  |# ;; " for confused Lisp reader
    
 #|
 code prop@  ;; ( word key -- val )
   (let ((key  (pop *pstack*)))
     (setf (car *pstack*) (ro:prop (car *pstack*) key))) }

 code prop!  ;; ( val word key -- )
   (destructuring-bind (key word val . rest) *pstack*
     (setf *pstack* rest
           (ro:prop word key) val)) }

   ( e.g., 15 ' BL ':x15 prop!  -- sets property with key :X15 on word BL to 15 )
 |#
      
 ;; local vars -------------------------------------------------

: collect-words ( delim-str -- words nbr )
     <r 0
     begin
       bl-word dup i string=
     not while
       swap 1+
     repeat drop r> drop ;

 : collect-locals ( -- ... lcl lcl lcl n )
       "{" collect-words ;

 : (call-with-frame)  i car ->vec
                      code{ (push (pop *pstack*) *frstack*) }
                      i cadr execute
                      code{ (pop *frstack*) }
                      r> cddr <r ;

 code create-local-frame
    (setf (frame-locals (car *display*)) (pop *pstack*)) }
         
 : ->    collect-locals 
         compile (call-with-frame) dup ,
         ->vec [compile] { swap create-local-frame ; immediate

  ( to be used as:
       { -> a b c { a b + . c a / } }

    Within the inner block, the named locals act like constants.
    The stack is trimmed by the number of locals appearing after the ->
    The rightmost local corresponds to the current TOS. )

  code toplevel?
    (push (toplevel?) *pstack*) }
    
  : ?execute ( fn -- fn or closure )
    -- used by some compiling words that construct functions
    -- sometimes we need them to execute at toplevel
    toplevel? if execute then ;


 ;; do-loops ----------------------------------------------------

 code (do)
  (destructuring-bind (ix limit . rest) *pstack*
    (setf *pstack* rest)
    (if (= ix limit)
        (setf *reg-i* (car *reg-i*))
      (progn
        (push limit *rstack*)
        (push ix    *rstack*)
        (setf *reg-i* (cdr *reg-i*))))
    ) }
 ' (do) mark-skip

 code (loop)
  (let ((ix (1+ (car *rstack*))))
    (if (< ix (cadr *rstack*))
        (progn
          (setf (car *rstack*) ix)
          (setf *reg-i* (car *reg-i*)))
      (progn
        (setf *rstack* (cddr *rstack*))
        (setf *reg-i* (cdr *reg-i*)))
      )) }
 ' (loop) mark-skip

 code (+loop)
  (let* ((incr (pop *pstack*))
         (ix   (+ incr (car *rstack*))))
    (if (minusp incr)
        (if (>= ix (cadr *rstack*))
            (progn
              (setf (car *rstack*) ix)
              (setf *reg-i* (car *reg-i*)))
          (progn
            (setf *rstack* (cddr *rstack*))
            (setf *reg-i* (cdr *reg-i*))))

      (if (< ix (cadr *rstack*))
          (progn
            (setf (car *rstack*) ix)
            (setf *reg-i* (car *reg-i*)))
        (progn
          (setf *rstack* (cddr *rstack*))
          (setf *reg-i* (cdr *reg-i*)))))
    ) }
 ' (+loop) mark-skip

 : do        compile (do) compile-nop here compile-cnop here ; immediate
 : loop      compile (loop) , [compile] then ; immediate
 : +loop     compile (+loop) , [compile] then ; immediate

  ;; --------------------------------------------------------------

 : spaces    0 do space loop ;

 code patch-behavior
   ;; ... just because we can...
   (destructuring-bind (wd ccode . rest) *pstack*
     (setf (fw-cfa wd) (fw-cfa ccode)
           *pstack*      rest)) }

 : "  #\" word
     compiling @ if compile literal , then ; immediate -- " for confused Lisp reader 

 : ?compile compiling @ if r> dup car , cdr <r then ;

 : ."     [compile] " ?compile . ; immediate

 code error (report-error (pop *pstack*)) }
 : error" [compile] " ?compile error ; immediate

 : .base   base @ dup #10r16 = if drop ." Hex"
             else dup #10r10 = if drop ." Decimal"
             else dup #10r8  = if drop ." Octal"
             else dup #10r2  = if drop ." Binary"
             else dup decimal . base !
           then then then then ;

 : verify-stack-empty
     depth 0/= if cr .s error" Something is dirty..." then ;

 code @nfa
    (setf (car *pstack*) (fw-nfa (car *pstack*))) }
   
 code @lfa
      (setf (car *pstack*) (fw-lfa (car *pstack*))) }

 code @dfa ;; as in: ' wwww dfa
    (setf (car *pstack*) (fw-dfa (car *pstack*))) }

 code @cfa
    (setf (car *pstack*) (fw-cfa (car *pstack*))) }
   
 code @ifa
    (setf (car *pstack*) (fw-ifa (car *pstack*))) }
   
 code !lfa
      (destructuring-bind (w new-prev . rest) *pstack*
        (setf *pstack*   rest
              (fw-lfa w) new-prev)) }

 code !ifa ;; ( val w -- )
      (destructuring-bind (w val . rest) *pstack*
        (setf *pstack*   rest
              (fw-ifa w) val)) }

 code !dfa ;; ( val w -- )
      (destructuring-bind (w val . rest) *pstack*
        (setf *pstack*   rest
              (fw-dfa w) val)) }

 ;; closures -------------------------------------------------------------
#|
 code create-closure
    (push (derive-word '<closure-def>) *pstack*) }

 code exit-closure
    (setf *reg-i*   (pop *rstack*)
          *frstack* (pop *rstack*)) }
     
 code @env
    (push *frstack* *pstack*) }

 : form-closure ( fn -- closure )
     @env over !dfa ;
       
 : '{
    push-compile-context        ;; make new static frame
    create-{                    ;; make new anon clondef as curdef, leave on stack
    set-current-context ]
    compile literal             ;; compiles into curdef
    push-compile-context        ;; make new static frame
    create-closure ; immediate  ;; make new anon closure curdef, leave on stack

 : }'
    compile exit-closure        ;; end the closure code
    import-icode                ;; reify the closure obj code
    pop-compile-context         ;; restore prior static frame
    compile form-closure        ;; compiles into containing anon colondef
    compile exit [compile] [
    import-icode                ;; reify colondef obj code
    pop-compile-context         ;; restore prior static frame
    set-current-context ; immediate
|#
      
 : jmp
    ?compile (jmp) ; immediate

 : ['] ' compiling @ if compile literal , then ; immediate

 
 -- Easy construction of Vectors ---
 
 : <<  '<< ;

 code >>
   (let ((pos (position '<< *pstack*)))
     (unless pos
       (report-error " Missing '<< stack mark"))
     (multiple-value-bind (hd tl)
         (um:split pos *pstack*)
       (let ((vec  (coerce (nreverse hd) 'vector)))
         (setf  *pstack*       tl
                (car *pstack*) vec))
       )) }
   

 ;; dictionary ----------------------------------------------------------
 
 code gild
       (setf *gild* (cons (@fcell *current*) (last-def))) }
 
 code empty
       (destructuring-bind (voc . last) *gild*
         (!fcell *current* voc)
         (!fcell *context* voc)
         (!fcell voc last)) }

 : dict-state  ( -- vec )
     current @ latest 2vec ;

 : restore-dict ( vec -- )
     dup 1 i@   ;; the last word
     swap @     ;; the voc ptr
     dup context !
     dup current !
     ! ; 

 : marker
     { dict-state ;: restore-dict }
     define-word ;

 : remember
     marker
     dict-state latest !dfa ;
 
 code (forget)
     (destructuring-bind (w name . rest) *pstack*
       (setf *pstack* rest)
       (let ((wf (forth-lookup-from-word name (cdr *gild*))))
         (if (and wf
                  (eql w wf))
             (print " Protected def")
           ;; else
           (let ((prev (fw-lfa w))
                 (voc  (@fcell *current*)))
             (!fcell voc prev)))
         )) }

 : forget bl-word find
     ?dup ifte
          { #| forth definitions |# (forget) }
          { ." Not found: " . cr } ;

remember overlay
gild

 ;; parting wishes... -----------------------------------------------
 .s ;; should report "<- Top" if we are clean
 verify-stack-empty

;; ------------------------------------------------------------

.end)

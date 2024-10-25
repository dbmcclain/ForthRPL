;; forth-itc-rpl-forthcode.lisp -- this file is intended to contain only Forth definitions
;; Any Lisp code should be for constructive purposes only, not for definitional bindings
;; ------------------------------------------------------------------
(in-package #:forthrpl)
;; ------------------------------------------------------------------

;; ------------------------------------------------------
;; forth
(initialize)

;; --------------------------------------------
;; VOCABULARY FORTH

(let ((v  (make-instance '<vocabulary>
                         :nfa  "FORTH"
                         :lfa  nil)))
  (setf *tic-forth*   v
        (current-voc) v
        (context-voc) v))

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

(code base
  (sp-! *base*))

;; --------------------------------------------

(code literal
  (sp-! ip@+))

(code exit
  (!ip rp@+))

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
                   (and logand) (or logior) (xor logxor) aref cons)
;; -------------------------------------------------------------------

(code skip-to-eol
  (skip-to-eol))
  
(code word
  (let* ((delim tos)
         (w     (next-word delim)))
    ;; (format t "~&word = ~S" w)
    (setf tos w)))

(code "(find)"
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
  (let ((name tos))
    (if name
        (setf tos (must-find name))
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

(colon finalize-code
        [  import-icode
        set-current-context
        pop-compile-context )

(colon }
        ;; try this out... at the end of a definition
        ;; the context is reset to current. Vocabulary switching
        ;; inside the definition does not carry forward after the
        ;; end of the new definition.
        compile exit
        finalize-code )
(immediate)

;; ---------------------------------------------------

(colon compile-cnop
       ;; this is needed because doing it in Forth ends up
       ;; replacing the CNOP after a "... compile cnop .. " construct
       compile cnop)

;; ---------------------------------------------------------
;; ... the rest directly in Forth...
;; ---------------------------------------------------------
;;  { #\newline word drop }  define-word ;; immediate  ;; we now have comments to end of line...

(interpret #1>.end
 { bl-word def } 'define-word def
 { skip-to-eol }          define-word ;; immediate  ;; we now have comments to end of line...
 { #\) word drop }        define-word ( immediate   ( we now have embedded comments )
 { }                      define-word ) immediate   ( for confused Editor )
 { bl-word must-find }    define-word '                    ( -- verb )
 { ' , }                  define-word [compile] immediate  ( -- ) ;; valid only in compile mode
 { bl-word [compile] { }  define-word :                    ( -- name verb -> compile mode )
 { swap def }             define-word swdef       
 { [compile] } swdef }    define-word ; immediate          ( name verb -- ) ;; ends compile mode
 
 : -- [compile] ;; ; immediate
 : ;;; [compile] ;; ; immediate
 
 ;; at this point we can do colon defs ---------------------------------
 
 : }-word #\} word ;
 : code{ }-word
         push-compile-context
         create-code
         pop-compile-context ; immediate
 : code bl-word [compile] code{ swdef ;

 code ?immed ;; ( wd -- t/f )
   (setf tos (is-immed tos)) }

 ;; now we can do code defs --------------------------------------------
 ;; so generate the inner workings for defining-words
 
 code (;code)
  (let ((behav (beh-of ip@)))
    (setf tos
          (derive-word '<scode-def>
                       :cfa (lambda (self)
                              (doval self)
                              (funcall behav self))
                       :dfa tos)
          ip  rp@+
          )) }

 : ;code{ compile (;code)
          [compile] code{
          finalize-code swdef ; immediate
          
 code ;:
  ;; stack contains data
  ;; this performs EXIT and the follow i-code
  ;; will be the actions for the newly defined verb
  (setf tos
        (derive-word '<scolon-def>
                     :dfa tos
                     :ifa ip)
        ip rp@+
        ) }

;;;  : (const) ;code{ }

 code (const)
   (setf tos (derive-word '<constant>
                          :dfa tos)) }
  
 ;; handy access to the host environment... ---------------------------
 ;;
 ;; How is this different from inline code{ ?
 ;;
 ;;   CODE{   produces compiled Lisp code and places it on the stack.
 ;;           You can turn around and EXECUTE that code. The code is always
 ;;           called with one parameter - the <CODE> structure representing the
 ;;           Forth code object, called SELF.
 ;;
 ;;   LISP{   performs an immediate call to compiled Lisp code an has no
 ;;           ostensible effects on the stack (unless the code itself does).
 ;;           The code is not called with any parameters.
 
 code lisp
  (let ((e  (read-from-string sp@+)))
    (funcall (compile nil `(lambda () ,e)))) }

 : lisp{ }-word lisp ;

 ;; various stack manipulation verbs -----------------------------------

 code execute
  (execute-word sp@+) }

 code (jmp)
     (do-jmp ip@+) }
  
 code <r
      (rp-! sp@+) }

 code r>
      (sp-! rp@+) }

 code i
      (sp-! rtos) }

 code j
      (sp-! (third rp)) }

;; --------------------------------------------
;; Stack twiddling

 code over
  (sp-! nos) }

 code swap-over
    (rotatef tos nos)
    (sp-! nos) }

 code swap-drop
   (let ((x  sp@+))
     (setf tos x)) }
   
 code dupnos
   (let ((a tos))
     (setf tos nos)
     (sp-! a)) }

 code dup   (sp-! tos) }
 code roll  (!sp (let ((ct sp@+))
                   (roll ct sp))) }
 code rot   (!sp (roll 2 sp)) }
 code -rot  (!sp (roll -2 sp)) }
 
 code ndrop
  (let ((n tos))
    (!sp (nthcdr (1+ n) sp))) }

 code 2dup
   (sp-! nos)
   (sp-! nos) }

 code 2swap
  (destructuring-bind (a b c d . rest) sp
    (!sp (list* c d a b rest))) }

 code 2over
  (destructuring-bind (a b c d . rest) sp
    (!sp (list* c d a b c d rest))) }

 code 2rot
  (destructuring-bind (a b c d e f . rest) sp
    (!sp (list* e f a b c d rest))) }

 code -2rot
  (destructuring-bind (a b c d e f . rest) sp
    (!sp (list* c d e f a b rest))) }

 code 2swap-over
  (destructuring-bind (a b c d . rest) sp
    (!sp (list* a b c d a b rest))) }

 code 2drop
  (!sp (cddr sp)) }

 code depth
  (sp-! (length sp)) }

 code pick
  (let ((n tos))
    (setf tos (nth (1+ n) sp))) }

;; --------------------------------------------
;; Lists

 code ->lst
  (let ((nel sp@+))
    (multiple-value-bind (hd tl)
        (um:split nel sp)
      (!sp (cons (nreverse hd) tl))
      )) }

 code ->lst*
  (let ((nel sp@+))
    (multiple-value-bind (hd tl)
        (um:split nel sp)
      (!sp (cons (append (nreverse (cdr hd)) (car hd)) tl))
      )) }

 code lst->
   (let ((lst  sp@+))
     (!sp (cons (length lst) (nconc (reverse lst) sp)) )) }

 : pop   ( lst -- [cdr lst] [car lst] )
    dup cdr
    swap car ;

 : push  ( lst x -- [cons x lst] )
    swap cons ;
    
;; --------------------------------------------
;; Vectors

 code ->vec
  (let ((nel sp@+))
    (multiple-value-bind (hd tl)
        (um:split nel sp)
      (!sp (cons (make-array nel
                             :initial-contents (nreverse hd))
                         tl)))) }

 code vec->
  (let* ((seq  sp@+)
         (lst  (coerce seq 'list)))
    ;; be careful here... this could also be applied to a list
    ;; in which case the (coerce seq 'list) would return the original argument
    ;; might not be safe to nreverse that original list
    (!sp (cons (length seq) (nconc (reverse lst) sp)))) }

 code copy-vec
   (setf tos (copy-seq tos)) }

 code 1vec
   (setf tos (vector tos)) }

 code 2vec
   (let* ((snd  sp@+)
          (fst  tos))
     (setf tos (vector fst snd))) }

;; --------------------------------------------
;; Easy construction of Vectors
;;
;; For #(a b c) enter: << a b c >>
;; Same as:  a b c 3 ->vec
 
 : <<  '<< ;

 code >>
   (let ((pos (position '<< sp)))
     (unless pos
       (report-error " Missing '<< stack mark"))
     (multiple-value-bind (hd tl)
         (um:split pos sp)
       (let ((vec  (coerce (nreverse hd) 'vector)))
         (setf  sp  tl
                tos vec))
       )) }
   
;; --------------------------------------------

 code string=
  ;; case insensitive
  (let* ((s1 sp@+)
         (s2 tos))
    (setf tos (string-equal s1 s2))) }

 code /mod
  (let ((d  tos)
        (n  nos))
    (multiple-value-bind (q r)
        (truncate n d)
      (setf nos q
            tos r))) }

 code */
   (let* ((d  sp@+)
          (n2 sp@+))
     (setf tos (/ (* tos n2) d))) }

 code */mod
   (let* ((d sp@+))
    (multiple-value-bind (q r)
        (truncate (* tos nos) d)
      (setf nos q
            tos r))) }

 code sw-
   (let ((a  sp@+))
     (setf tos (- a tos))) }

 code sw/
    (let ((a sp@+))
      (setf tos (/ a tos))) }

 code sw-mod
    (let ((a sp@+))
      (setf tos (mod a tos))) }

 code sw!
    (let* ((val  sp@+)
           (loc  sp@+))
      (setf (@fcell loc) val)) }
 

 code .
   (with-users-base
    (princ sp@+)) }


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

 : s:abc  ;
 : s:acb  swap ;
 : s:bca  rot ;
 : s:bac  rot swap ;
 : s:cab  -rot ;
 : s:cba  swap rot ;

 : s:ab ;
 : s:ba   swap ;

 : s:abb  dup ;
 : s:bba  dup rot ;
 : s:bab  swap over ;
 
 : s:aba  over ;
 : s:aab  over swap ;
 : s:baa  swap dup ;

 : s:abab 2dup ;
 : s:abba 2dup swap ;
 : s:aabb 2dup rot ;
 : s:baab swap 2dup swap ;
 : s:baba swap 2dup ;
 : s:bbaa swap 2dup rot ;
 
;; --------------------------------------------
;; DynVars

code (dynvar)
   (let* ((var (derive-word '<dynvar>))
          (lst (list (vector tos)))
          (key (data-of var)))
     (setf (car *dynvars*)
           (maps:add (car *dynvars*) key lst))
     (setf tos var)) }

: dynvar   ( val -- )
   (dynvar) define-word ;
   
   
 ;; 1-D arrays -------------------------------------------------------

 code allot (setf tos (make-array tos)) }

 : array        ( n -- )
     allot constant ;

 code fill     ;; ( v arr -- )
   (let* ((arr sp@+)
          (v   sp@+))
     (fill arr v)) }

 code i@
      (let ((ix sp@+))
        (setf tos (@fcell tos ix))) }

 code i!
  (let* ((ix  sp@+)
         (loc sp@+)
         (val sp@+))
    (setf (@fcell loc ix) val)) }

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
   (let* ((n     sp@+)
          (pair  tos))
     (when (plusp n)
       (loop repeat n do
               (setf pair (@fcell pair 1)))
       (setf tos pair))) }
   
 ;; vocabularies ------------------------------------------------

 code latest
       (sp-! (last-def)) }

 code (vocabulary)
       (sp-! (derive-word '<vocabulary>)) }
                    
 : vocabulary   ( -- )
     (vocabulary)
     define-word ;

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

 ;; indefinite looping -----------------------------------------------

 code mark-skip
   #+:LISPWORKS
   (sys:atomic-push sp@+ *skip-words*)
   #-:LISPWORKS
   (push sp@+ *skip-words*) }
 
 ;; code here (push (funcall %cur-icode% :last-pos) sp) }
 code here (sp-! (arena-last-pos %cur-icode%)) }

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

 ;; CASE Statement -----------------------------------------------------

 : (case)
     r>     ;; x a
     begin
       s:aab      ;; x x a
       pop        ;; x x [cdr a] [car a]
       dup        ;; x x [cdr a] [car a] [car a]
    while
       s:bac execute  ;; x [cdr a] t/f
       if
          swap-drop      ;; [cdr a]
          pop  swap      ;; [car [cdr a]] [cdr [cdr a]]
          begin
            pop
          while
            pop drop
          repeat
          <r
          execute
          exit
       else
          pop drop
       then
     repeat
     drop <r 2drop ;

 "OpenCase" constant OpenCase
 
 : case  OpenCASE compile (case) ; immediate
 
 : esac  drop code{ (forth-compile-in nil) } ; immediate
 
 : otherwise  drop t ;

 ;; code replacement --------------------------------------------------

 code patch
  ;; very un-Forth-like
  (let* ((wd    sp@+)
         (icode sp@+))
    (setf (icode-of wd) (icode-of icode))) }

 ;; : ?dup dup if dup then ;
 code ?dup
    (when tos
      (sp-! tos)) }

 ;; now give us a proper outer interpreter
 { begin bl-word ?dup-while (find) interpret repeat quit } ' outer patch

;; --------------------------------------------
;; Special Return Stack Ops

code rp)+
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
    (rotatef rtos rnos)
    ;; (rp-! sp@+)
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

;; --------------------------------------------

: >base<
     ;; Save base, perform caller's code, restore base on its exit.
     base @ <<r
     >r<
     r> base ! ;

;; --------------------------------------------
;; Unwind-Protect

code (protect)
    (forth-protect (cdr rp@)) }

code (pop-prot)
     up@+) }
     
: protect
    ;; Protect one word following the use of protect in the caller's code.
    ;; All the rest of the code will be used as unwind code.
    (protect)
    >r<
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
;; dynvars. Each of them has its current binding copied and stacked
;; for subsequent use, leaving prior bindings in place in the dynvar
;; stack.
;;

code (rebinding)
   (let* ((dvars  *dynvars*))
     (up-! dvars)                  ;; save current bindings for restore
     (push (car dvars) *dynvars*)  ;; copy bindings into new context
     (nlet iter ((lst  (icode-of (pop rp@)))) ;; get list of vars
       (when lst
         (let ((var (car lst)))
           (when (typep var '<dynvar>)
             (let* ((key   (data-of var))
                    (vecs  (maps:find (car *dynvars*) key)))
               (setf (car *dynvars*)
                     (maps:add (car *dynvars*) key (cons (copy-seq (car vecs)) vecs)))
               )))
         (go-iter (cdr lst))
         ))) }

code (pop-rebindings)
     (setf *dynvars* up@+) }

: rebinding
    (rebinding)
    >r<
    (pop-rebindings) ;

;; --------------------------------------------

code .u
   (inspect (list up *dynvars*)) }

0 dynvar x  0 dynvar y  0 dynvar z

: .vars
    << x @ y @ z @ >> . ;
    
: tst-reb
   rebinding { x y z }
   .u
    5 x !
   15 y !
   32 z !
   .vars ;
   
;; --------------------------------------------
;; Replacement Bindings for Forth - change a var and have it restored on exit.

code (dyn-bind)
  ;; construct a dyn-restore list, exchange with RTOS to place
  ;; the restore struct on the r-stack, and the return addr on the p-stack.
  ;; Will be used by the following [ PROTECT >>R<< ].
  (let* ((vec  sp@+)
         (nel  (length vec)))
    (nlet iter ((ct   0)
                (acc  nil))
      (if (>= ct nel)
          (up-! acc)
        (let ((var  (aref vec ct))
              (val  (aref vec (1+ ct))))
          (go-iter (+ ct 2)
                   (list* var
                          (shiftf (@fcell var) val)
                          acc)))
        ))) }

code (dyn-restore)
   ;; Expect a dyn-restore list on top of R-stack.
   ;; Restore the vars in the list, popping the r-stack.
   (nlet iter ((lst up@+))
     (when lst
       (destructuring-bind (var val . rest) lst
         (setf (@fcell var) val)
         (go-iter rest))
       )) }
   
: dyn-bind ( vars-vals n -- sav )
    (dyn-bind) r>
    protect
    >>r<<
    (dyn-restore) ;

;; --------------------------------------------------------------

 : spaces    0 do space loop ;

 code patch-behavior
   ;; ... just because we can...
   (let* ((wd    sp@+)
          (ccode sp@+))
     (setf (beh-of wd) (beh-of ccode))) }

 : "  #\" word
     compiling @ if compile literal , then ; immediate -- " for confused Editor

 : ?compile compiling @ if rp)+ , then ;

 : ."     [compile] " ?compile . ; immediate

 code error
     (report-error sp@+) }

 : error" [compile] " ?compile error ; immediate

;; --------------------------------------------

: .base
       base @
       case { #10r16 =  } { ." Hex"     }
            { #10r10 =  } { ." Decimal" }
            { #10r8  =  } { ." Octal"   }
            { #10r2  =  } { ." Binary"  }
            { otherwise } { >base< base @ decimal . }
       esac ;

: verify-stack-empty
     depth 0/= if cr .s error"   Something is dirty..." then ;

: ['] ' compiling @ if compile literal , then ; immediate
: [,] compiling @ if compile literal then , ; immediate

: =>
   compiling @ if compile =>
   else ' code{ (to-oper tos nos)
                (setf sp  (cddr sp)) }
   then ; immediate

;; --------------------------------------------

: tst-dyn
    << base 16 >> dyn-bind
    .base ;

0 variable tstvar

: tst-unw
    << base    16.
       tstvar 511. >> dyn-bind
   base tstvar 2 ->lst . cr
   if error" Wjat!!" then ;

;; --------------------------------------------
;; Proper Comments & Conditional Compilation

 : #| ; immediate  ;; |# for confused Editor
 : skip-to-fi ;
 : skip-quote  #\" word drop ;
 : skipper     
     case { "#|"      string= } { [compile] #|  }  ;; |# |#
          { ";;"      string= } { [compile] ;;  }
          { ";;;"     string= } { [compile] ;;; }
          { "--"      string= } { [compile] --  }
          { "("       string= } { [compile] (   }
          { "#+IF"    string= } { skip-to-fi    }
          { "#-IF"    string= } { skip-to-fi    }
          { "\""      string= } { skip-quote    }
          { ".\""     string= } { skip-quote    }
          { "error\"" string= } { skip-quote    }
      esac ;


 { begin bl-word dup
     case { "|#" string= } { drop r> r> 2drop }  ;; " dummy for Editor
          { otherwise    } { dup skipper      }
      esac
      drop
   again }
' #| patch         ;; |#


 { begin bl-word dup
     case { "FI#" string= } { drop r> r> 2drop }
          { otherwise     } { dup skipper      }
     esac
     drop
   again }
' skip-to-fi patch


 code feature?
     (setf tos (member tos *features*)) }
     
 : #-IF   if skip-to-fi then ; immediate
 : #+IF   not [compile] #-IF ; immediate
 : FI# ; immediate
 
 nil #+IF diddly dodah! FI#
 
 
;; local vars -------------------------------------------------

: collect-words ( delim-str -- words nbr )
     <r 0
     begin
       bl-word dup i string=
     until
       swap 1+
     repeat
     drop r> drop ;

 code <env
    (fp-! sp@+) }

 code env>
    fp@+ }

 : (call-with-frame)
      rp)+ ->vec
      <env rp)+ execute env> ;

 code create-locals-frame
    (setf (frame-locals (car *display*)) sp@+) }
         
 : ->    "{" collect-words
         compile (call-with-frame) dup ,
         ->vec [compile] { swap create-locals-frame
         ; immediate

;; --------------------------------------------
;; possibly cleaner way to do things...

code :->;:  ;; ( pend-: nlocals -- pend-;: )
   (let ((nel  sp@+))
     (change-class tos '<scolon-def>
                   :cfa       'doscol
                   :dfa       nel
                   :verb-type ";:"
                   )) }

 : (exec-with-frame)   ( lcl1 lcl2 ... nlocals -- )
     ->vec <env >r< env> ;
     
 : ->|    "|" collect-words ;; " for dumb Editor
          dup <r ->vec create-locals-frame r> :->;:
          compile (exec-with-frame) ; immediate

;; --------------------------------------------
  ( -> to be used as:

       { -> a b c { a b + . c a / } }

    Within the inner block, the named locals act like constants.
    The stack is trimmed by the number of locals appearing after the ->.
    The rightmost local corresponds to the current TOS. )

;; --------------------------------------------
;; Parameterized Functional Closures

 code @env
     (sp-! *frstack*) }

 code !env
     (!fp sp@+) }
    
 : make-closure ( fn env -- clos )
       2vec ;: @env <r
               dup snd !env
               fst execute
               r> !env ;

 : (closure)
    rp)+ @env ?dup-if make-closure then ;

  code toplevel?
    (sp-! (toplevel?)) }
    
 : '{  
    toplevel? ifnot compile (closure) then [compile] { ; immediate

 : }}  [compile] }  [compile] } ; immediate
 : }}} [compile] }} [compile] } ; immediate
 
;; --------------------------------------------
;; Example of a closure in action:

  { -> ct { '{ ct dup 1+ => ct }}} constant ctr-maker
  0 ctr-maker execute constant my-ctr
  : next-ct  my-ctr execute ;

  { ->| ct |
     '{ ct dup 1+ => ct }} constant new-ctr-maker
  0 new-ctr-maker execute constant new-ctr
  : next-new-ct  new-ctr execute ;
  
;; --------------------------------------------
   ( To construct a parameterized closure, you must first enclose 
     the '{ ... } within outer braces, e.g., 

            { -> a { '{ a + } } }

     The arrow -> looks for an opening { to terminate the list of local vars.
     The inner closured gets pushed on the stack and can be executed.

     Otherwise, a non-parameterized function literal, at either top level
     of within a colon def:
         : doit ... '{ ... } ... ; )
   
;; ----------------------------

  : ?execute ( fn -- fn or closure )
    -- used by some compiling words that construct functions
    -- sometimes we need them to execute at toplevel
    toplevel? if execute then ;


;; --------------------------------------------

 code @nfa
    (setf tos (name-of tos)) }
   
 code @lfa
    (setf tos (prev-of tos)) }

 code @dfa ;; as in: ' wwww @dfa
    (setf tos (data-of tos)) }

 code @cfa
    (setf tos (beh-of tos)) }
   
 code @ifa
    (setf tos (icode-of tos)) }

: name-of  @nfa ;
: prev-of  @lfa ;
: data-of  @dfa ;
: beh-of   @cfa ;
: icode-of @ifa ;

    
 code !nfa
      (let* ((w        sp@+)    ;; LET* because we need sequential oper
             (new-name sp@+))
        (setf (name-of w) new-name)) }
      
 code !lfa
      (let* ((w        sp@+)
             (new-prev sp@+))
        (setf (prev-of w) new-prev)) }

 code !cfa
       (let* ((w        sp@+)
              (new-code sp@+))
         (setf (beh-of w) new-code)) }
       

 code !ifa ;; ( val w -- )
      (let* ((w   sp@+)
             (val sp@+))
        (setf (icode-of w) val)) }

 code !dfa ;; ( val w -- )
      (let* ((w   sp@+)
             (val sp@+))
        (setf (data-of w) val)) }

: !name-of !nfa ;
: !prev-of !lfa ;
: !data-of !dfa ;
: !beh-of  !cfa ;
: !icode-of !ifa ;

;; --------------------------------------------
;; Dictionary Management
#|
                                                                     
                                                            
                              ┌─ Vocabulary ─────────────────┐
                              │ ┌────────────┐┌────────────┐ │
                              │ │   Latest   ││   Parent   │ │
                              │ └────────────┘└────────────┘ │
                              └───────────────▲──────────────┘
                                              │               
                                              │               
                       ┌─ Vocabulary ─────────┼───────┐       
  ┌────────────┐       │ ┌────────────┐┌──────┴─────┐ │       
  │  Current   ├───┬───▶ │   Latest   ││   Parent   │ │       
  └────────────┘   │   │ └─────┬──────┘└────────────┘ │       
                   │   └───────┼──────────────────────┘       
  ┌────────────┐   │           │                              
  │  Context   ├───┘           │     Dictionary Words         
  └────────────┘               │      ┌────────────┐          
                               └──────▶    Def     │          
                                      └──────┬─────┘          
                                             │ LFA            
                                             │                
                                      ┌──────▼─────┐          
                                      │    ...     │          
                                      └──────┬─────┘          
                                             │ LFA            
                                             │                
                                             ▼

CONTEXT is used for dict searches.
CURRENT is the voc being extended with new defs.

A VOCABULARY is a Forth word, just like any others, but its behavior
sets CONTEXT to itself when executed.

VOCABULARY is a defining word, which produces an immediate
context-switching word, so that CONTEXT for searches can be switched
while compiling.

Vocabularies are linked together along the LFA chain with all other
words in their parent dictionary. They also form a secondary chain
through their parent vocabularies. (Parent = CURRENT, at the time of
their definition.)

VOCABULARY words reside in their parent's vocabulary branch of the
dictionary, and can start a new branch of their own when you execute
DEFINITIONS.  DEFINITIONS sets CURRENT to the value of CONTEXT. So a
VOCABULARY word is visible in two branches - its own and its parent's.

Example showing a chain of 3 vocabularies:
------------------------------------------

                    ┌──────────────┐                                              
                    │              │                                              
                    │              │                                              
                    ▼              │                                              
              Forth Words          │                                              
             ┌────────────┐        │                                              
             │    Def     │        │                                              
             └──────┬─────┘        │                                              
                    │ LFA          │                                              
                    │              │                                              
             ┌──────▼─────┐        │                                              
             │    ...     │        │                                              
             └──────┬─────┘        │                                              
                    │ LFA          │                                              
                    │              │                                              
                    ▼              │                                              
                   ...             │                                              
                                   │                                              
                    │   ┌──────────┼─────────────────────────────────────────────┐
                    │   │          │                                             │
                    │   │          │                                             │
       Parent┌──────▼───▼──┐ Latest│                                             │
 ┌───────────┤ Voc Camera  ├───────┼──────────────────────────────────┐          │
 │           └──────┬──────┘       │                                  │          │
 │                  │ LFA          │                                  │          │
 │                  │              │                                  ▼          │
 │                  ▼              │                            Camera Words     │
 │                 ...             │                           ┌────────────┐    │
 │                                 │                           │    Def     │    │
 │                  │  ┌───────────┼────────────────────────┐  └──────┬─────┘    │
 │                  │  │           │                        │         │ LFA      │
 │                  │  │           │                        │         │          │
 │     Parent┌──────▼──▼───┐ Latest│                        │  ┌──────▼─────┐    │
 │  ┌────────┤Voc Assembler├───────┼─────────────┐          │  │    ...     │    │
 │  │        └──────┬──────┘       │             │          │  └──────┬─────┘    │
 │  │               │ LFA          │             │          │         │ LFA      │
 │  │               │              │             ▼          │         │          │
 │  │               ▼              │      Assembler Words   │         ▼          │
 │  │              ...             │      ┌────────────┐    │        ...         │
 │  │                              │      │    Def     │    │                    │
 │  │               │              │      └──────┬─────┘    │         │          │
 │  │               │              │             │ LFA      │         │          │
 │  │               │              │             │          │         │          │
 │  │        ┌──────▼─────┐        │      ┌──────▼─────┐    │  ┌──────▼─────┐    │
 │  │        │    ...     │        │      │    ...     │    │  │    ...     │    │
 │  │        └──────┬─────┘        │      └──────┬─────┘    │  └──────┬─────┘    │
 │  │               │ LFA          │             │ LFA      │         │ LFA      │
 │  │               │              │             │          │         │          │
 │  │               ▼              │             ▼          │         └──────────┘
 │  │              ...             │            ...         │                     
 │  │                              │                        │                     
 │  │               │              │             │          │                     
 │  │               │              │             │          │                     
 │  │               │              │             │          │                     
 │  │        ┌──────▼──────┐ Latest│      ┌──────▼─────┐    │                     
 │  └────────▶             ├───────┘      │    ...     │    │                     
 └───────────▶  Voc Forth  │              └──────┬─────┘    │                     
        ┌────┤             │                     │ LFA      │                     
  Parent│    └──────┬──────┘                     │          │                     
      ━━▼━━         │ LFA                        └──────────┘                     
                  ━━▼━━

In this example vocabularies ASSEMBLER and CAMERA both link to parent
FORTH. They can see their own definitions, and those of parent FORTH
that existed at the time of their creation. CURRENT was pointing at
FORTH when they were defined.

When in a vocabulary branch, you cannot see the definitions belonging
to any other branch vocabularies, nor to any definitions of your
parent's vocabulary that were defined later.

Since CAMERA was defined later than ASSEMBLER, CAMERA and its own
definitions can see ASSEMBLER directly, but not ASSEMBLER's
definitions.  ASSEMBLER and its definitions can see neither CAMERA nor
its definitions.

However, both can ask to see the other and their definitiions by using
FORTH ASSEMBLER and FORTH CAMERA, since both vocabularies are visible
to FORTH. Since both have FORTH as a parent, they can both switch
directly to FORTH, then FORTH can switch to either of them.

Similarly, FORTH cannot see the definitions in ASSEMBLER or CAMERA
without first executing ASSEMBLER or CAMERA.

Executing a VOCABULARY word sets the CONTEXT to its own branch of the
dictionary, viewing all that have been defined for the VOCABULARY,
even as later defintions extend the vocabulary. A vocabulary word
always points to its latest dictionary entry.

Executing DEFINITIONS after switching context with a VOCABULARY word,
sets the system to extend that particular branch of the dictionary as
you compile new definitions.

Vocabulary FORTH is at the base of the dictionary tree. It has no
parent vocabulary, and no LFA predecessor in the dictionary tree.
|#

;; --------------------------------------------
;; Some useful dictionary probing words

 : exists?
     ;; return true if word on stack exists in the dictionary
     bl-word (find) swap-drop ;

 code string-contains
   (let ((str  (string sp@+)))
     (setf tos (search tos str :test #'char-equal))) }

 : last-def  ( voc -- wrd )
     data-of @ ;
     
 : current-last   ( -- wrd )
     current @ last-def ;
     
 : context-last   ( -- wrd )
     context @ last-def ;
     
 code (apropos)
    ;; returns a list of words sorted in increasing length
    (setf tos (forth-apropos tos)) }

 : apropos
     ;; print of list of approximate matches to a word
     bl-word (apropos)
     begin
     ?dup-while
       pop . space
     repeat ;

 code (catalog)
    (sp-! (catalog)) }

 : catalog 
     (catalog)
     begin
     ?dup-while
	pop . cr
     repeat ;

;; --------------------------------------------
;; Dictionary State
#|
   Dict State is a copy of CURRENT and a pointer
   to the latest def in that vocabulary at the time of the state
   snapshot.
   
   ┌─ Dict State ────────────────┐
   │ ┌───────────┐ ┌───────────┐ │
   │ │   Vocab   │ │   Last    │ │
   │ └───────────┘ └───────────┘ │
   └─────────────────────────────┘                                                             
|#

 : dict-state  ( -- vec )
     current @ dup data-of @ 2vec ;

 : restore-dict ( vec -- )
     dup snd    ;; the last word
     swap fst   ;; the voc ptr
     dup context !
     dup current !
     data-of ! ; 

 ;; --------------------------------------------
 ;; REMEMBER & MARKER
 ;;
 ;; MARKER creates a word that forgets down through itself when
 ;; executed. Avoids GILD checking, and so should be used for defs
 ;; beyond the GILD point.
 ;;
 ;; REMEMBER creates a MARKER that retains itself on execution.
 
 #|
  ┌─ Marker ────────────────────┐
  │ ┌───────────┐ ┌───────────┐ │
  │ │   Vocab   │ │   Last    │ │
  │ └───────────┘ └───────────┘ │
  └─────────────────────────────┘
 |#

 : marker
     { dict-state ;: restore-dict }
     define-word ;

 : remember
     marker
     dict-state latest !data-of ; ;; patch to include ourself

 ;; --------------------------------------------
 ;; GILD & EMPTY -- GILD should only ever be applied to the FORTH
 ;; vocabulary. That way any vocabularies defined below the gild point
 ;; are also protected against FORGET.
     
 nil variable gilded-state

 : gild
      ['] FORTH data-of @ gilded-state ! ;

 : empty
      gilded-state @
      ?dup-if ['] FORTH data-of ! then ;

 : ungild
      nil gilded-state ! ;

 ;; --------------------------------------------
 ;; FORGET - a bit more complicated...
 
 : parent-voc-of  ( voc -- voc' )
    data-of snd ;

 : ancestor?   ( w1 w2 -- t/f )
    ;; return true if w1 is w2, or found in chain starting at w2
    begin
      2dup eq if drop exit then
      dup
    while
      prev-of
    repeat
    swap-drop ;
    
 : beneath?   ( w1 w2 -- t/f )
    ;; return true if w2 beneath w1
    swap prev-of ancestor? ;
    
 : find-current-voc  ( w -- voc )
    current @
    begin
      2dup beneath? if swap-drop exit then
      parent-voc-of
    again ;

 : is-forth?  ( w -- t/f )
     ['] FORTH eq ;

 : attachment  ( w -- w' )
     ;; If W resides in the main Forth vocabulary trunk then it is its
     ;; own attachment point.
     ;;
     ;; Otherwise, find the ancestor vocabulary that resides in the
     ;; main Forth trunk as the attachment point.
     ;;
     ;; An ancestor vocabulary residing in the main trunk will have a
     ;; parent vocabulary that is FORTH.
     ;;
     dup find-current-voc dup is-forth? if drop exit then
     swap-drop
     begin
        dup parent-voc-of is-forth? if exit then
        parent-voc-of
     again ;
     
 : protected? ( w -- t/f )
     ;; W is protected if it lives in the main Forth trunk and it is
     ;; at or beneath the GILD point, or else W's ancestor vocabulary
     ;; that resides in the main Forth trunk is at or beneath the GILD
     ;; point.
     attachment gilded-state @ ancestor? ;
     
 : (forget)   ( w -- )
     dup protected? if drop error" Protected def" then
     dup prev-of swap find-current-voc
     dup current ! dup context !
     data-of ! ;
     
 : forget
     set-current-context
     bl-word (find)
     dup ifte
          { swap-drop (forget) }
          { drop ." Not found: " . cr } ;

;; --------------------------------------------
;; PAD and numeric output formatting

code pad
   (sp-! *pad*) }
          
code !fill-ptr
     (let* ((arr   sp@+)
            (val   sp@+))
       (setf (fill-pointer arr) val)) }

code (c,)
   (let* ((arr  sp@+)
          (val  sp@+))
     (vector-push-extend val arr)) }
   
code ch->code
   (setf tos (char-code tos)) }

code code->ch
   (setf tos (code-char tos)) }

: <pad  0 pad !fill-ptr ;
: c,    pad (c,) ;
: pad>  pad ;
code <<pad
   (ps-! (fill-pointer *pad*)) }
code pad>>
   (sp-! (subseq *pad* ps@))
   (setf (fill-pointer *pad*) ps@+) }
    
: <#  <pad ;
: #>  pad> reverse ;
: <<# <<pad ;
: #>> pad>> reverse ;
: ->dig
    dup 9. >
    if [ #\A ch->code 10. - ] [,]
    else [ #\0 ch->code ] [,]
    then + code->ch ;
: #   base @ /mod ->dig c, ;
: #.  # #\. c, ;    
: #:  6 base ! # decimal #\: c, ; ;; only makes sense in decimal context
: ##: # #: ;
: #s  begin
        #
        dup 0=
      until
      repeat drop ;
: n#  0 do # loop ;
: #sign  0< if #\- c, then ;
: #sign+ 0< if #\- else #\+ then c, ;

: ndp  ( val n -- )
    2dup base @ swap expt swap abs * round
    <<# swap n# #\. c, #s #sign #>> ;
: 2dp  2 ndp ;

: unipolar  1.0 mod ;
: bipolar   0.5 + unipolar 0.5 - ;
    
: dms  ( turns -- )
    >base< decimal
    bipolar dup abs 1296000. * round
      <<# ##: ##: #s #sign+ #>> ;
      
: hms  ( turns -- )
    >base< decimal
    unipolar 864000. * round 
      <<# #. ##: ##: # # #>> ;
    
;; --------------------------------------------

':LISPWORKS feature? #+IF

code cwd
   (sp-! (hcl:get-working-directory)) }

: pwd  cwd . ;

code cd
   (let ((path sp@+))
     (if path
         (eval `(hcl:cd ,path))
       (hcl:cd))) }

code ls
  (sys:call-system-showing-output "ls -lF") }
   
code load 
      (interpret (hcl:file-string sp@+)) }
FI#

;; --------------------------------------------
;; Actors

code send
  (ac:send* (coerce sp@+ 'list)) }

code println
  (sp-! ac:println) }

code writeln
  (sp-! ac:writeln) }

<< println " Hello from ForthRPL!!" >> send

;; --------------------------------------------

code trace+
   (trace+) }

code trace-
   (trace-) }

remember overlay
gild

 ;; parting wishes... -----------------------------------------------
 
 .s ;; should report "<- Top" if we are clean
 10 spaces ." !! Should just show:<-Top !!"
verify-stack-empty
 
;; ------------------------------------------------------------

.end)


;; --------------------------------------------

(defun doit ()
  (goforth)
  (interactive))

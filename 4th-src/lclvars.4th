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

 code create-locals-frame
    (setf (frame-locals (car *display*)) sp@+) }

;; local vars [2] -----------------------------------------------

: local          r> swap dup <r @ <r continuation r> r> ! ;
#|
Example:

   0 variable a
   0 variable b
   
   : tst   ( a b -- )
       a local b local
       b ! a !
       ( do something with a, b)
       ;
       
|#
;; --------------------------------------------
#|
 : (call-with-frame)
      rtos)+ ->vec
      <env rtos)+ execute env> ;

 : ->    "{" collect-words
         compile (call-with-frame) dup ,
         ->vec [compile] { swap create-locals-frame
         ; immediate
|#
;; --------------------------------------------
;; possibly cleaner way to do things...

code :->;:  ;; ( pend-: nlocals -- pend-;: )
   (let ((nel  sp@+))
     (change-class tos '<scolon-def>
                   :cfa       (ensure-compiled-function 'doscol)
                   :dfa       nel
                   :verb-type ";:"
                   )) }

 : (exec-with-frame)   ( lcl1 lcl2 ... nlocals -- )
     ->vec <env r> continuation env> ;
     
 : (->|)  "|" collect-words                          
          dup <r ->vec create-locals-frame r> :->;:
          compile (exec-with-frame) ;

 : {|   [compile] { (->|) ; immediate

;; --------------------------------------------
  ( {| to be used as:

       {| a b c |
         a b + . c a / }

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
    rtos)+ @env ?dup-if make-closure then ;

  code toplevel?
    (sp-! (toplevel?)) }

  : closure?
      toplevel? ifnot compile (closure) then ;
    
 : '{  
    closure? [compile] { ; immediate

 : '{|
    closure? [compile] {| ; immediate

 : }}  [compile] }  [compile] } ; immediate
 : }}} [compile] }} [compile] } ; immediate
 
;; --------------------------------------------
;; Example of a closure in action:

  {| ct |
     '{ ct dup 1+ => ct }} constant new-ctr-maker
  0 new-ctr-maker execute constant new-ctr
  : next-new-ct  new-ctr execute ;

: ctr  { 0 1vec ;: dup @ 1 rot +! } define-word ;
  ctr my-ctr

;; --------------------------------------------
   ( To construct a parameterized closure, you must first enclose 
     the '{ ... } within outer braces, e.g., 

            {| a | '{ a + } } }

     The inner closure gets pushed on the stack and can be executed.

     Otherwise, a non-parameterized function literal, at either top level
     of within a colon def:
         : doit ... '{ ... } ... ; )
   

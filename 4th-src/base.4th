
 { bl-word def } 'define-word def

 { skip-to-eol }          define-word ;; immediate  ;; we now have comments to end of line...
 { #\) word drop }        define-word ( immediate   ( we now have embedded comments )

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
   (!tos (is-immed tos)) }

 ;; now we can do code defs --------------------------------------------
 ;; so generate the inner workings for defining-words
 
 code (;code)
  (let ((behav (beh-of (@ ip ] ))))
    (!tos (derive-word '<scode-def>
                       :cfa (lambda (self)
                            	(doval self)
                                (funcall behav self))
                       :dfa tos))
    (lea ip rpop)) }

 : ;code{ compile (;code)
          [compile] code{ ; immediate

 code ;:
  ;; stack contains data
  ;; this performs EXIT and the follow i-code
  ;; will be the actions for the newly defined verb
  (!tos (derive-word '<scolon-def>
                     :dfa tos
                     :ifa ip))
  (lea ip rpop) }

 ;;;  : (const) ;code{ }

 code (const)
   (!tos (derive-word '<constant>
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
 ;;   LISP{   performs an immediate call to compiled Lisp code and has no
 ;;           ostensible effects on the stack (unless the code itself does).
 ;;           The code is not called with any parameters.
 
 code lisp
  (let ((e  (read-from-string spop)))
    (funcall (compile nil `(lambda () ,e)))) }

 : lisp{ }-word lisp ;
 : l{  lisp{ ;

;;  lisp{ (inspect *dynvars*) }

;; --------------------------------------------

code interp-string
   	(interpret spop) }

code load 
      	(inhale spop) }

;; --------------------------------------------

"core-stack" load
"dynvars"    load
"consts"     load
"structs"    load
"looping"    load
"dynops"     load
"comments"   load
"printing"   load
"lclvars"    load
"vocabs"     load
"picnum"     load
"misc"       load

;; --------------------------------------------
;; now give us a proper outer interpreter

{ begin bl-word ?dup-while find interpret repeat quit } ' outer patch

;; --------------------------------------------

remember overlay
gild

;; parting wishes... -----------------------------------------------
 
.s ;; should report ".<- Top" if we are clean
10 spaces ." !! Should just show: <-Top !!" cr
verify-stack-empty
 
;; ------------------------------------------------------------


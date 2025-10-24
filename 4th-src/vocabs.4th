;; -*- Mode : Forth ; Encoding : UTF-8 -*-

;; vocabularies ------------------------------------------------

 code latest
       (sp-! (last-def)) }

 code (vocabulary)
       (sp-! (derive-word '<vocabulary>
	        :dfa (vector nil (current-voc)) )) }
                    
 : vocabulary   ( -- )
     (vocabulary)
     define-word ;

 : definitions  ( -- )
     context @ current ! ;

 code voc-of 
   (let ((wp sp@+))
     (sp-! (voc-of-wrd wp))) }

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

  code @mfa
    (setf tos (mempos-of tos)) }

: name-of   @nfa ;
: prev-of   @lfa ;
: data-of   @dfa ;
: beh-of    @cfa ;
: icode-of  @ifa ;
: mempos-of @mfa ;

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

 code !mfa
       (let* ((w   sp@+)
              (val sp@+))
          (setf (mempos-of w) val)) }

: !name-of    !nfa ;
: !prev-of    !lfa ;
: !data-of    !dfa ;
: !beh-of     !cfa ;
: !icode-of   !ifa ;
: !mempos-of  !mfa ;

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

code next-word-this-line
	(sp-! (next-word-this-line #\space)) }

 : exists?
     ;; return true if word on stack exists in the dictionary
     bl-word find swap-drop ;

 code string-contains
   (let ((str  (string sp@+)))
     (setf tos (search tos str :test #'char-equal))) }

 : last-def  ( voc -- wrd )
     data-of fst ;
     
 : current-last   ( -- wrd )
     current @ last-def ;
     
 : context-last   ( -- wrd )
     context @ last-def ;
     
 : .list
     begin
     ?dup-while
       pop . space
     repeat ;

 : catalog 
     code{ (sp-! (catalog)) }
     begin
     ?dup-while
	pop . cr
     repeat ;

 : vlist
    next-word-this-line ?dup
    if code{ (setf tos (forth-apropos tos)) } .list
    else drop code{ (vlist) } 
    then ;

: words vlist ;

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

;; --------------------------------------------

 code dict-index
   (sp-! *mempos*) }

 code (forgetter)
    (forgetter sp@+)) }

;; --------------------------------------------

 : dict-state  ( -- vec )
     dict-index current @ 2vec ;

 : restore-dict ( vec -- )
     dup fst (forgetter)
     snd context ! definitions ;

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
      dict-index gilded-state ! ;

 : empty
      gilded-state @
      ?dup-if [compile] FORTH definitions
              (forgetter)
      then ;

 : ungild
      nil gilded-state ! ;

 ;; --------------------------------------------
 ;; FORGET - a bit more complicated...
 
 : protected? ( w -- t/f )
     ;; W is protected if it lives in the main Forth trunk and it is
     ;; at or beneath the GILD point, or else W's ancestor vocabulary
     ;; that resides in the main Forth trunk is at or beneath the GILD
     ;; point.
     mempos-of gilded-state @ ?dup if < else 2drop nil then ;  
     
 : .name   @nfa . ;

 : (forget)   ( w -- )
     dup protected? if ." No, " .name error" is protected" then
     mempos-of (forgetter) ;
     
 : forget
     bl-word find
     dup ifte
          { swap-drop (forget) }
          { drop ." Not found: " . cr } ;


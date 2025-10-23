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

 code vlist   (vlist) }

 code voc-of 
   (let ((wp sp@+))
     (sp-! (voc-of-wrd wp))) }

;; ----------------------------

  : ?execute ( fn -- fn or closure )
    -- used by some compiling words that construct functions
    -- sometimes we need them to execute at toplevel
    toplevel? if execute then ;

;; --------------------------------------------

 code @nfa
    (setf tos (name-of tos)) }

#|   
 code @lfa
    (setf tos (prev-of tos)) }
|#

 code @dfa ;; as in: ' wwww @dfa
    (setf tos (data-of tos)) }

 code @cfa
    (setf tos (beh-of tos)) }
   
 code @ifa
    (setf tos (icode-of tos)) }

: name-of  @nfa ;
;; : prev-of  @lfa ;
: data-of  @dfa ;
: beh-of   @cfa ;
: icode-of @ifa ;

: defs-of  ( voc -- list )
     data-of @ ;

code memq  ;; ( item list -- list' )
	(let* ((lst sp@+)
	       (item sp@+))
           (sp-!  (member item lst))) }
	
: prevs-of   context @ defs-of memq cdr ;
: prev-of    prevs-of car ;
    
 code !nfa
      (let* ((w        sp@+)    ;; LET* because we need sequential oper
             (new-name sp@+))
        (setf (name-of w) new-name)) }

#|      
 code !lfa
      (let* ((w        sp@+)
             (new-prev sp@+))
        (setf (prev-of w) new-prev)) }
|#

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
;; : !prev-of !lfa ;
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
     bl-word find swap-drop ;

 code string-contains
   (let ((str  (string sp@+)))
     (setf tos (search tos str :test #'char-equal))) }

 : last-def  ( voc -- wrd )
     defs-of car ;
     
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

;; --------------------------------------------

 code dict-index
   (sp-! (fill-pointer *dict*)) }

 code dict-entry
   (let ((ix  sp@+))
      (sp-!  (aref *dict* ix))) }

 code dict-pos
   (let* ((wp sp@+)
         (pos (position wp *dict*)))
     (sp-! pos)) }

 code (forgetter)
    (forgetter sp@+)) }

;; --------------------------------------------

 : dict-state  ( -- vec )
     dict-index current @ 2vec ;

 : restore-dict ( vec -- )
     dup fst dict-entry (forgetter)
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
              dict-entry (forgetter)
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
     dict-pos gilded-state @ ?dup if < else 2drop nil then ;  
     
 : .name   @nfa . ;

 : (forget)   ( w -- )
     dup protected? if ." No, " .name error" is protected" then
     (forgetter) ;
     
 : forget
     set-current-context
     bl-word find
     dup ifte
          { swap-drop (forget) }
          { drop ." Not found: " . cr } ;


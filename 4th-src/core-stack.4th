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
      ;; refers to the caller's "i", just above the return addr, which
      ;; itself is above our "i" and limit.
      (sp-! (fourth rp)) }

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

 code copy-seq
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
;; For (list a b c) enter: << a b c >>lst
;; For (vector a b c) enter: << a b c >>vec
;; Same as:  a b c 3 ->vec
 
 : <<  '<< ;

 code >>lst
   (let ((pos (position '<< sp)))
     (unless pos
       (report-error " Missing '<< stack mark"))
     (multiple-value-bind (hd tl)
         (um:split pos sp)
       (let ((lst (nreverse hd)))
         (setf  sp  tl
                tos lst))
       )) }
   
 : >>vec
     >>lst  code{ (setf tos (coerce tos 'vector)) } ;

 : >>  >>lst ;
 
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
 : s:baab swap s:abba ;
 : s:baba swap s:abab ;
 : s:bbaa swap s:aabb ;
 
 ;; : ?dup dup if dup then ;
 code ?dup
    (when tos
      (sp-! tos)) }


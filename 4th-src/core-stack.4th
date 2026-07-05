 ;; various stack manipulation verbs -----------------------------------

 code execute
  (execute-word spop) }

 code (jmp)
     (do-jmp (@ ip ]+)) }
  
 code <r
      (rpush spop) }

 code r>
      (spush rpop) }

 code i
      (spush rtos) }

 code j
      ;; refers to the caller's "i", just above the return addr, which
      ;; itself is above our "i" and limit.
      (spush (fourth rp)) }

;; --------------------------------------------
;; Stack twiddling

 code over
  (spush nos) }

 code swap-over
    (rotatef tos nos)
    (spush nos) }

 code swap-drop
   (let ((x  spop))
     (!tos x)) }
   
 code dupnos
   (let ((a tos))
     (!tos  nos)
     (spush a)) }

 code dup   (spush tos) }
 code roll  (lea sp (let ((ct spop))
                   	(roll ct sp))) }
 code rot   (lea sp (roll 2 sp)) }
 code -rot  (lea sp (roll -2 sp)) }
 
 code ndrop
  (let ((n tos))
    (lea sp (nthcdr (1+ n) sp))) }

 code 2dup
   (spush nos)
   (spush nos) }

 code 2swap
  (destructuring-bind (a b c d . rest) sp
    (lea sp (list* c d a b rest))) }

 code 2over
  (destructuring-bind (a b c d . rest) sp
    (lea sp (list* c d a b c d rest))) }

 code 2rot
  (destructuring-bind (a b c d e f . rest) sp
    (lea sp (list* e f a b c d rest))) }

 code -2rot
  (destructuring-bind (a b c d e f . rest) sp
    (lea sp (list* c d e f a b rest))) }

 code 2swap-over
  (destructuring-bind (a b c d . rest) sp
    (lea sp (list* a b c d a b rest))) }

 code 2drop
  (lea sp (cddr sp)) }

 code depth
  (spush (length sp)) }

 code pick
  (let ((n tos))
    (!tos (nth (1+ n) sp))) }

;; --------------------------------------------
;; Lists

 code ->lst
  (let ((nel spop))
    (multiple-value-bind (hd tl)
        (um:split nel sp)
      (lea sp (cons (nreverse hd) tl))
      )) }

 code ->lst*
  (let ((nel spop))
    (multiple-value-bind (hd tl)
        (um:split nel sp)
      (lea sp (cons (append (nreverse (cdr hd)) (car hd)) tl))
      )) }

 code lst->
   (let ((lst  spop))
     (lea sp (cons (length lst) (nconc (reverse lst) sp)) )) }

 code car 
   (!tos (car tos)) }

 code cdr
   (!tos (cdr tos)) }

 code cons
   (let ((a  spop))
      (!tos (cons a tos))) }

 : pop   ( lst -- [cdr lst] [car lst] )
    dup cdr
    swap car ;

 : push  ( lst x -- [cons x lst] )
    swap cons ;
    
;; --------------------------------------------
;; Vectors

 code ->vec
  (let ((nel spop))
    (multiple-value-bind (hd tl)
        (um:split nel sp)
      (lea sp (cons (make-array nel
                             :initial-contents (nreverse hd))
                         tl)))) }

 code vec->
  (let* ((seq  spop)
         (lst  (coerce seq 'list)))
    ;; be careful here... this could also be applied to a list
    ;; in which case the (coerce seq 'list) would return the original argument
    ;; might not be safe to nreverse that original list
    (lea sp (cons (length seq) (nconc (reverse lst) sp)))) }

 code copy-seq
   (!tos (copy-seq tos)) }

 code 1vec
   (!tos (vector tos )) }

 code 2vec
   (let* ((snd  spop)
          (fst  tos))
     (!tos (vector fst snd))) }

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

code l->v 
     (!tos (coerce tos 'vector)) }
   
code v->l
     (!tos (coerce tos 'list)) }

 : >>vec
     >>lst  l->v ; 

 : >>  >>lst ;
 
;; --------------------------------------------

 code string=
  ;; case insensitive
  (let* ((s1 spop)
         (s2 tos))
    (!tos (string-equal s1 s2))) }

 code /mod
  (let ((d  tos)
        (n  nos))
    (multiple-value-bind (q r)
        (truncate n d)
      (setf nos q
            tos r))) }

 code */
   (let* ((d  spop)
          (n2 spop))
     (!tos (/ (* tos n2) d))) }

 code */mod
   (let* ((d spop))
    (multiple-value-bind (q r)
        (truncate (* tos nos) d)
      (setf nos q
            tos r))) }

 code sw-
   (let ((a  spop))
     (!tos (- a tos))) }

 code sw/
    (let ((a spop))
      (!tos (/ a tos))) }

 code sw-mod
    (let ((a spop))
      (!tos (mod a tos))) }

 code sw!
    (let* ((val  spop)
           (loc  spop))
      (setf (@fcell loc) val)) }

;; --------------------------------------------
;;    +--- NOS
;;    |+-- TOS
;;    ||
;;    VV
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
      (spush tos)) }


 ;; 1-D arrays -------------------------------------------------------

 code allot (!tos (make-array tos)) }

 : array        ( n -- )
     allot constant ;

 code fill     ;; ( v arr -- )
   (let* ((arr spop)
          (v   spop))
     (fill arr v)) }

 code i@
      (let ((ix spop))
        (!tos (@fcell tos ix))) }

 code i!
  (let* ((ix  spop)
         (loc spop)
         (val spop))
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
   (let* ((n     spop)
          (pair  tos))
     (when (plusp n)
       (loop repeat n do
               (setf pair (@fcell pair 1)))
       (!tos pair))) }
   
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


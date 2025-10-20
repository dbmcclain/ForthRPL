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


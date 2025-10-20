;; --------------------------------------------
;; DynVars

code (dynvar)
   (let* ((var (link-derived-word '<dynvar>
                                  :nfa sp@+)))
     (add-dynvar var sp@+)) }

: dynvar   ( val -- )
   bl word (dynvar) ;
   

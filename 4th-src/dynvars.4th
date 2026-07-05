;; --------------------------------------------
;; DynVars

code (dynvar)
   (let* ((var (link-derived-word '<dynvar>
                                  :nfa spop)))
     (add-dynvar var spop)) }

: dynvar   ( val -- )
   bl word (dynvar) ;
   

;; --------------------------------------------
  
':LISPWORKS feature? #+IF

code cwd
   (sp-! (hcl:get-working-directory)) }

: pwd  cwd . ;

code cd
   (let ((path sp@+))
     (if path
         (eval `(hcl:cd ,path))
       (hcl:cd))) }

code system
  (sys:call-system-showing-output sp@+) }

: ls  " ls -lF" system ;
   
FI#

;; --------------------------------------------
;; Actors

code send   ;; ( msg-lst targ -- )
  (let* ((targ sp@+)
         (msg  sp@+))
    (ac:send* targ msg)) }

code println
  (sp-! ac:println) }

code writeln
  (sp-! ac:writeln) }

<< " Hello from ForthRPL!!" >>lst println send

;; --------------------------------------------

code trace+
   (trace+) }

code trace-
   (trace-) }

;; --------------------------------------------

: catch ( tag -- nil )
    code{ (forth-catch) }
    execute
    code{ (forth-uncatch) } ;
    
code ?throw  ;; ( ans tag -- )
   ;; throws to catch tag if ans non-null
   (forth-throw) }

;; --------------------------------------------

code save-dynvars
  (save-dynvars) }

save-dynvars

;; --------------------------------------------
  
':LISPWORKS feature? #+IF

code cwd
   (spush (hcl:get-working-directory)) }

: pwd  cwd . ;

code cd
   (let ((path spop))
     (if path
         (eval `(hcl:cd ,path))
       (hcl:cd))) }

code system
  (sys:call-system-showing-output spop) }

: ls  " ls -lF" system ;
   
FI#

;; --------------------------------------------
;; Actors

code send   ;; ( msg-lst targ -- )
  (let* ((targ spop)
         (msg  spop))
    (ac:send* targ msg)) }

code println
  (spush ac:println) }

code writeln
  (spush ac:writeln) }

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

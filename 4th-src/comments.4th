;; --------------------------------------------
;; Proper Comments & Conditional Compilation

 : #| ; immediate
 : skip-to-fi ;
 : skip-quote  #\" word drop ;
 : skipper     
     case { "#|"      string= } { [compile] #|  }  ;; |# |#
          { ";;"      string= } { [compile] ;;  }
          { ";;;"     string= } { [compile] ;;; }
          { "--"      string= } { [compile] --  }
          { "("       string= } { [compile] (   }
          { "#+IF"    string= } { skip-to-fi    }
          { "#-IF"    string= } { skip-to-fi    }
          { "\""      string= } { skip-quote    }
          { ".\""     string= } { skip-quote    }
          { "error\"" string= } { skip-quote    }
      esac ;

 { begin bl-word dup
     case { "|#" string= } { drop r> r> 2drop }
          otherwise        { dup skipper      }
     esac
     drop
   again }
' #| patch


 { begin bl-word dup
     case { "FI#" string= } { drop r> r> 2drop }
          otherwise         { dup skipper      }
     esac
     drop
   again }
' skip-to-fi patch


 code feature?
     (setf tos (member tos *features*)) }
     
 : #-IF   if skip-to-fi then ; immediate
 : #+IF   not [compile] #-IF ; immediate
 : FI# ; immediate
 
 nil #+IF diddly dodah! FI#
 
 

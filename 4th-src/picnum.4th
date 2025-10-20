;; --------------------------------------------
;; PAD and numeric output formatting

code pad
   (sp-! *pad*) }
          
code !fill-ptr
     (let* ((arr   sp@+)
            (val   sp@+))
       (setf (fill-pointer arr) val)) }

code (c,)
   (let* ((arr  sp@+)
          (val  sp@+))
     (vector-push-extend val arr)) }
   
code ch->code
   (setf tos (char-code tos)) }

code code->ch
   (setf tos (code-char tos)) }

: <pad  0 pad !fill-ptr ;
: c,    pad (c,) ;
: pad>  pad ;
code <<pad
   (ps-! (fill-pointer *pad*)) }
code pad>>
   (sp-! (subseq *pad* ps@))
   (setf (fill-pointer *pad*) ps@+) }
    
: <#  <pad ;
: #>  pad> reverse ;
: <<# <<pad ;
: #>> pad>> reverse ;
: ->dig
    dup 9. >
    if [ #\A ch->code 10. - ] [,]
    else [ #\0 ch->code ] [,]
    then + code->ch ;
: #   base /mod ->dig c, ;
: #.  # #\. c, ;    
: #:  6 !base # decimal #\: c, ; ;; only makes sense in decimal context
: ##: # #: ;
: #s  begin
        #
        dup 0=
      until
      repeat drop ;
: n#  0 do # loop ;
: #sign  0< if #\- c, then ;
: #sign+ 0< if #\- else #\+ then c, ;

: ndp  ( val n -- )
    2dup base swap expt swap abs * round
    <<# swap n# #\. c, #s #sign #>> ;
: 2dp  2 ndp ;

: unipolar  1.0 mod ;
: bipolar   0.5 + unipolar 0.5 - ;
    
: dms  ( turns -- )
    >base< decimal
    bipolar dup abs 1296000. * round
      <<# ##: ##: #s #sign+ #>> ;
      
: hms  ( turns -- )
    >base< decimal
    unipolar 864000. * round 
      <<# #. ##: ##: # # #>> ;
    

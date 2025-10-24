;; forth.lisp -- Forth with Cons-threading in Lisp
;; Equivalent of ITC
;; DM/RAL  03/09
;; --------------------------------------------------------------------

;; ------------------------------------------------------------------
(in-package #:forthrpl)
;; ------------------------------------------------------------------

(defparameter *init-fns*
  (make-array 16.
              :adjustable   t
              :fill-pointer 0))

(defun do-add-init (fn)
  (vector-push-extend fn *init-fns*))

(defmacro add-init (&body body)
  `(do-add-init (lambda ()
                  ,@body)))

(defun initialize ()
  (map nil 'funcall *init-fns*))

;; ------------------------------------------------------------------
(defparameter *meta-present* nil)
;; --------------------------------------------------------------------

;; forward refs
(defparameter *tic-exit*  nil)
(defparameter *tic-lit*   nil)
(defparameter *tic-cnop*  nil)
(defparameter *tic-outer* nil)
(defparameter *tic-forth* nil)
(defparameter *tic-base*  nil)

;; --------------------------------------------
(add-init
 (setf *meta-present* nil
       *tic-exit*     nil
       *tic-lit*      nil
       *tic-cnop*     nil
       *tic-outer*    nil
       *tic-forth*    nil
       *tic-base*     nil))
;; --------------------------------------------

(defgeneric fw-nfa (x))
(defgeneric fw-lfa (x))
(defgeneric fw-cfa (x))
(defgeneric fw-dfa (x))
(defgeneric fw-ifa (x))

;; --------------------------------------------

(defmacro name-of (w)
  `(fw-nfa ,w))

(defmacro prev-of (w)
  `(fw-lfa ,w))

(defmacro data-of (w)
  `(fw-dfa ,w))

(defmacro beh-of (w)
  `(fw-cfa ,w))

(defmacro icode-of (w)
  `(fw-ifa ,w))

;; -----------------------------------------------------

(defclass named-linked-mixin ()
  ((nfa   :accessor  fw-nfa   :initarg :nfa)
   (lfa   :accessor  fw-lfa   :initarg :lfa))
  (:default-initargs
   :nfa  "<ANON>"
   :lfa  nil
   ))

(defgeneric find-entry-eq (wphd obj)
  (:method ((wphd null) obj)
   nil)
  (:method ((wphd named-linked-mixin) obj)
   (um:nlet iter ((hd  wphd))
     (when hd
       (when (eq obj hd)
         (return-from find-entry-eq obj))
       (go-iter (prev-of hd))))
   ))

(defgeneric find-entry-by-name (wphd name)
  (:method ((wphd null) name)
   nil)
  (:method ((wphd named-linked-mixin) name)
   (let ((sname (string name)))
     (um:nlet iter ((hd wphd))
       (when hd
         (when (string-equal sname (string (name-of hd)))
           (return-from find-entry-by-name hd))
         (go-iter (prev-of hd))))
     )))

(defgeneric %dolinks (wphd fn)
  (:method ((wphd null) fn)
   nil)
  (:method ((wphd named-linked-mixin) fn)
   (um:nlet iter ((hd wphd))
     (when hd
       (funcall fn hd)
       (go-iter (prev-of hd))))
   ))

(defmacro dolinks ((arg hd) &body body)
  `(%dolinks ,hd (lambda (,arg) ,@body)))

#+:LISPWORKS
(editor:setup-indent "dolinks" 1)

;; --------------------------------------------
  
(defclass <code-def> (named-linked-mixin)
  ((verb-type :accessor verb-type :initarg :verb-type)
   (has-data? :accessor has-data? :initarg :has-data?)
   (cfa       :accessor fw-cfa    :initarg :cfa      )
   (immed     :accessor is-immed  :initarg :immed    ))
  (:default-initargs
   :verb-type "CODE"
   :cfa       'no-behavior
   :immed     nil
   :has-data? nil
   ))

(defun no-behavior (self)
  (error "No behavior: ~A" (name-of self)))

(defmethod print-object ((self <code-def>) out-stream)
  (print-unreadable-object (self out-stream :type t :identity nil)
    (princ (name-of self) out-stream)))

(defclass <scode-def> (<code-def>)
  ((dfa       :accessor fw-dfa    :initarg :dfa))
  (:default-initargs
   :verb-type ";CODE"
   :has-data? t
   :dfa       nil
   ))

;; --------------------------------------------------------

(defgeneric print-name (self)
  (:method ((self <code-def>))
   (princ (name-of self)))
  (:method ((obj list))
   (princ "(")
   (dolist (item obj)
     (print-name item)
     (princ #\space))
   (princ ")"))
  (:method (obj)
   (write obj)))

;; -----------------------------------------------------
;; *SKIP-WORDS* -- a list of Forth defs that the decompiler should
;; skip over - typically are branch target addresses.

(defparameter *skip-words* nil)

;; --------------------------------------------
(add-init
 (setf *skip-words* nil))
;; --------------------------------------------

(defgeneric decompile-obj (obj)
  (:method ((obj <code-def>))
   (format t "~A ~A" (verb-type obj) (name-of obj))
   (when (is-immed obj)
     (princ " IMMEDIATE")))
  (:method ((obj <scode-def>))
   (format t "~A ~A := ~A" (verb-type obj) (name-of obj) (data-of obj))
   ))
  
;; -----------------------------------------------------------

(defmacro @fcell (name &optional (index 0))
  `(aref ,name ,index))

(defmacro !fcell (name val &optional (index 0))
  `(setf (aref ,name ,index) ,val))

(defun make-fcell (&optional val)
  (vector val))

;; -------------------------------------------------------------------
;; i-code arenas

(defclass arena ()
  ((hd  :accessor arena-hd  :initform nil)
   (tl  :accessor arena-tl  :initform nil
        :accessor arena-last-pos)))

(defun make-arena ()
  (make-instance 'arena))

(defgeneric arena-append (a obj)
  (:method ((a arena) obj)
   (with-accessors ((hd  arena-hd)
                    (tl  arena-tl)) a
     (if (eq (car tl) *tic-cnop*)
         (setf (car tl) obj)
       (let ((cell (list obj)))
         (setf tl
              (if tl
                  (setf (cdr tl) cell)
                (setf hd cell)))
         ))
     )))

(defgeneric arena-get (a)
  (:method ((a arena))
   (with-accessors ((hd  arena-hd)
                    (tl  arena-tl)) a
     (let ((cell (list *tic-cnop*)))
       (setf tl cell)
       (shiftf hd cell))
     )))

;; -------------------------------------------------------------------
;; Local Vars...

(defstruct frame
  def
  locals
  (icode-arena (make-arena)))

;; display - a stack of local frames
;; each frame is (cons compile-state . locals)
;; first one applies to toplevel non-compiling state

;; -----------------------------------------------------------
;; RPL Registers

(defparameter *reg-i*   nil)
(defparameter *pstack*  nil)
(defparameter *rstack*  nil)
(defparameter *frstack* nil)

;; --------------------------------------------
;; *dynvars* is a list, each element of which is a tree root pointer
;; encased in a CONS cell. This makes it possible to update that root
;; pointer while not affecting identity of the enclosing CONS cell.
;;
;; In that way, we can add bindings to the base *dynvars* and they
;; will be retained on an unwind.
;;
;; During REBIND we duplicate the tree root pointer at the head of the
;; list in *dynvars*. That way new trees constructed along the way
;; with rebound dynvar values, will affect only the top CONS cell of
;; the *dynvars* list,
;;
;; At all times, the relevant tree root pointer is located at:
;;
;;    (CAR *dynvars*)
;;
;; and that is the location that should receive the new tree root
;; pointer after any tree modifications. (Got that?)
;;
;; Similarly, each dynvar in the tree is a value encased in a CONS
;; cell. That way, on rebindings with the same value, a new CONS cell
;; is wrapped around the old value to make it appear as a fresh new
;; value to the FPL RB Tree. Same values, at the moment, but different
;; tree.
;;
;; This is similar to encasing a mutable cell inside a CONS cell so
;; that CAS can operate on the CONS cell. The identity of the CONS
;; cell remains fixed, while the inner contained value can mutate.

;; -----------------------------------------------------------
;; User Areas - one per machine thread

(defparameter *tic-initial-dynvars*  (maps:empty))

(defstruct user
  (context   (make-fcell *tic-forth*))
  (current   (make-fcell *tic-forth*))
  (compiling (make-fcell))
  (pad       (make-array 80.
                         :element-type 'character
                         :adjustable   t
                         :fill-pointer 0))
  pad-stack
  (display   (list (make-frame)))
  tracing
  reader
  (dynvars   (list *tic-initial-dynvars*))
  catch-pt
  unwind-chain)

(defparameter *user*  (make-user))

;; --------------------------------------------
(add-init
  (setf *user* (make-user)))
;; --------------------------------------------

(defmacro with-forth (&body body)
  `(let ((*user*    (copy-user *user*))
         (*reg-i*   nil)
         (*pstack*  nil)
         (*rstack*  nil)
         (*frstack* nil))
     ,@body))


(define-symbol-macro *context*          (user-context      *user*))
(define-symbol-macro *current*          (user-current      *user*))
(define-symbol-macro *compiling*        (user-compiling    *user*))
(define-symbol-macro *pad*              (user-pad          *user*))
(define-symbol-macro *display*          (user-display      *user*))
(define-symbol-macro *tracing*          (user-tracing      *user*))
(define-symbol-macro *next-word-reader* (user-reader       *user*))
(define-symbol-macro *pad-stack*        (user-pad-stack    *user*))
(define-symbol-macro *unwind-chain*     (user-unwind-chain *user*))
(define-symbol-macro *dynvars*          (user-dynvars      *user*))
(define-symbol-macro *catch-pt*         (user-catch-pt     *user*))

(define-symbol-macro dynvar-tree        (car *dynvars*))

;; --------------------------------------------

(define-symbol-macro sp    *pstack*)
(define-symbol-macro rp    *rstack*)
(define-symbol-macro ip    *reg-i*)
(define-symbol-macro fp    *frstack*)
(define-symbol-macro up    *unwind-chain*)

(define-symbol-macro tos   (car  sp))
(define-symbol-macro nos   (cadr sp))
(define-symbol-macro rtos  (car  rp))
(define-symbol-macro rnos  (cadr rp))

;; Ideally, these would be written like SP). But Lisp won't allow this.
(define-symbol-macro sp@   (car sp))
(define-symbol-macro rp@   (car rp))
(define-symbol-macro ip@   (car ip))
(define-symbol-macro fp@   (car fp))
(define-symbol-macro ps@   (car *pad-stack*))

;; Ideally, these would be written like SP)+. But Lisp won't allow this.
(define-symbol-macro sp@+  (pop sp))
(define-symbol-macro rp@+  (pop rp))
(define-symbol-macro ip@+  (pop ip))
(define-symbol-macro fp@+  (pop fp))
(define-symbol-macro ps@+  (pop *pad-stack*))
(define-symbol-macro up@+  (pop up))

;; And ideally, these would be written SP-)!. But Lisp won't allow this.
(defmacro sp-! (val)
  `(push ,val sp))

(defmacro rp-! (val)
  `(push ,val rp))

(defmacro fp-! (val)
  `(push ,val fp))

(defmacro ps-! (val)
  `(push ,val *pad-stack*))

(defmacro up-! (val)
  `(push ,val up))



(defmacro !sp (val)
  `(setf sp ,val))

(defmacro !rp (val)
  `(setf rp ,val))

(defmacro !ip (val)
  `(setf ip ,val))

(defmacro !fp (val)
  `(setf fp ,val))

;; --------------------------------------------

(defun get-base ()
  (lookup-dynvar *tic-base*))

(define-symbol-macro @base  (get-base))

;; ------------------------------------------------------

(declaim (inline compiling? set-compile))

(defun compiling? ()
  (@fcell *compiling*))

(defun set-compile (t/f)
  (!fcell *compiling* t/f))

(defsetf compiling? set-compile)

;; --------------------------------------------

(declaim (inline context-voc set-context-voc
                 current-voc set-current-voc
                 latest-in-voc set-latest-in-voc))

(defun context-voc ()
  (@fcell *context*))

(defun set-context-voc (voc)
  (!fcell *context* voc))

(defsetf context-voc set-context-voc)


(defun current-voc ()
  (@fcell *current*))

(defun set-current-voc (voc)
  (!fcell *current* voc))

(defsetf current-voc set-current-voc)

#|
(defun defs-of-voc (voc)
  (@fcell (data-of voc)))

(defun set-defs-of-voc (voc lst)
  (!fcell (data-of voc) lst)
  lst)

(defsetf defs-of-voc  set-defs-of-voc)


(defun latest-in-voc (voc)
  (car (defs-of-voc voc)))

(defun set-latest-in-voc (voc w)
  (push w (defs-of-voc voc))
  w)
|#

(defun latest-in-voc (voc)
  (@fcell (data-of voc)))

(defun set-latest-in-voc (voc w)
  (!fcell (data-of voc) w)
  w)

(defsetf latest-in-voc  set-latest-in-voc)

;; --------------------------------------------

#|
(defun forth-lookup-from-word (name w)
  (nlet iter ((w w))
    (when w
      (if (string-equal (string (name-of w)) name)
          w
        ;; else
        (go-iter (prev-of w)))
      )))

(defun forth-globals-lookup (w &optional (voc (context-voc)))
  (when-let (name (ignore-errors (string w))) ;; e.g., fails on numbers
    (when voc
      (forth-lookup-from-word name (latest-in-voc voc))
      )))
|#
#|
(defun forth-globals-lookup (w &optional (voc (context-voc)))
  (when-let (name (ignore-errors (string w))) ;; e.g., fails on numbers
    (nlet iter ((voc voc))
      (when voc
        (or (find name (@fcell (data-of voc))
                  :test #'string-equal
                  :key #'(lambda (w)
                           (string (name-of w))))
            (go-iter (@fcell (data-of voc) 1)))
        ))
    ))
|#
(defun forth-globals-lookup (w &optional (voc (context-voc)))
  (when-let (name (ignore-errors (string w))) ;; e.g., fails on numbers
    (nlet iter ((voc voc))
      (when voc
        (or (find-entry-by-name (latest-in-voc voc) name)
            (go-iter (@fcell (data-of voc) 1)))
        ))
    ))

;; -------------------------------------------------------------------
;; Local Vars...

(define-symbol-macro %cur-icode% (frame-icode-arena (car *display*)))
(define-symbol-macro %cur-def%   (frame-def (car *display*)))

(defun new-frame (&rest args)
  (apply 'make-frame :def %cur-def% args))

(defun toplevel? ()
  (null (cdr *display*)))

(defun forth-locals-lookup (w)
  (nlet iter ((frames *display*)
              (lvl    0))
    (when frames
      (destructuring-bind (hd . tl) frames
        (unless (frame-locals hd)
          (go-iter tl lvl))
        (if-let (pos (position w (frame-locals hd)
                               :test #'string-equal))
            (values lvl pos)
          ;; else
          (go-iter tl (1+ lvl)))
        ))))

;; -----------------------------------------------------------------------

(defun #1=forth-lookup (w &optional (voc (context-voc)))
  (when (compiling?)
    (multiple-value-bind (lvl pos)
        (forth-locals-lookup w)
      (when lvl
        (return-from #1# (local-accessor lvl pos)))
      ))
  (forth-globals-lookup w voc))

(defun do-with-users-base (fn)
  (let ((*print-base* @base))
    (funcall fn)))

(defmacro with-users-base (&body body)
  `(do-with-users-base (lambda () ,@body)))

(defgeneric report-error (fmt &rest args)
  (:method ((fmt string) &rest args)
   (with-users-base
     (apply #'format t fmt args)
     (terpri)
     (abort)))
  (:method (obj &rest ignored)
   ;; allows use of a named object for reporting
   (declare (ignore ignored))
   (report-error (name-of obj))))

(defun huh? (s)
  (report-error " ~A ?" s))

;; --------------------------------------------
;; Unwind-Protect support for Forth code

(defstruct prot-frame
  ip rp sp fp dp cp)

(defstruct dynvar-sav
  vars)

(defstruct catch-sav
  tag
  prot)

(defstruct dynbind-sav
  lst)

(defun forth-protect (ip)
  (up-! (make-prot-frame
         :ip   ip  ;; user's unwind ip
         :rp   rp
         :sp   sp
         :fp   fp
         :dp   *dynvars*
         :cp   *catch-pt*)
        ))

(defun restore-state (state)
  ;; assumes state is PROT-FRAME
  (with-accessors ((sav-ip   prot-frame-ip)
                   (sav-rp   prot-frame-rp)
                   (sav-sp   prot-frame-sp)
                   (sav-fp   prot-frame-fp)
                   (sav-dp   prot-frame-dp)
                   (sav-cp   prot-frame-cp)) state
    (setf *reg-i*    sav-ip
          ;; *pstack*  sav-sp
          *rstack*   sav-rp
          *frstack*  sav-fp
          *dynvars*  sav-dp
          *catch-pt* sav-cp)
    ))

(defun restore-dynbinds (state)
  (nlet iter ((lst  (dynbind-sav-lst state)))
    (when lst
      (destructuring-bind (var val . rest) lst
        (setf (@fcell var) val)
        (go-iter rest))
      )))

(defun forth-unwind (&optional target-state)
  (nlet iter ()
    (when-let (state up@+)
      (cond ((prot-frame-p state)
             (restore-state state)
             (inner-interp ip@+))
            ((dynvar-sav-p state)
             (setf *dynvars* (dynvar-sav-vars state)))
            ((dynbind-sav-p state)
             (restore-dynbinds state))
            ((catch-sav-p state)
             (restore-state (catch-sav-prot state))
             (setf *pstack* (prot-frame-sp (catch-sav-prot state)))) )
      (unless (eq state target-state)
        (go-iter))
      )))

(defparameter *dummy-prot*
  (list (make-instance '<code-def>
                       :lfa nil
                       :cfa (lambda (self)
                              (declare (ignore self))
                              (!ip nil)))
        ))

(defun forth-catch ()
  (let ((fn (pop rp@)))
    (up-! (setf *catch-pt*
                (make-catch-sav
                 :tag   sp@+
                 :prot  (make-prot-frame
                         :ip  rp@
                         :rp  rp
                         :sp  sp
                         :fp  fp
                         :dp  *dynvars*
                         :cp  *catch-pt*))
                ))
    (sp-! fn)))

(defun forth-uncatch ()
  (let ((csav  up@+))
    (setf *catch-pt* (prot-frame-cp (catch-sav-prot csav)))
    (sp-! nil)
    ))

(defun forth-throw ()
  (let* ((tag  sp@+)
         (ans  sp@+))
    ;; We have to make this conditional on ANS, since the execution
    ;; has unknown effect on the stack, while a (THROW NIL) would
    ;; appear the same way to the code following the CATCH as a normal
    ;; return, and there would be no way to know how to handle the
    ;; stack in that follow code.
    (when ans
      (nlet iter ((pt  *catch-pt*))
        (if pt
            (if (eq tag (catch-sav-tag pt))
                (progn
                  (forth-unwind pt)
                  (sp-! ans)
                  (inner-interp ip@+))
              (go-iter (prot-frame-cp (catch-sav-prot pt))))
          (error "No tag ~S" tag))
        ))))

;; --------------------------------------------

(defun must-find (w)
  (or (forth-globals-lookup w)
      (huh? w)))

(defun set-current-context ()
  (setf (context-voc) (current-voc)))

(defun forth-compile-words (wds)
  (set-current-context)
  (um:accum acc
    (dolist (wd wds)
      (let ((v  (forth-globals-lookup wd)))
        (cond (v
               (acc v))
                  
              ((and (consp wd)
                    (eq 'quote (car wd)))
               (acc *tic-lit*)
               (acc (cadr wd)))
                  
              (t
               (acc *tic-lit*)
               (acc wd))
              )))
    (acc *tic-exit*)))
      
;; --------------------------------------

(defun trace+ ()
  (setf *tracing* t))

(defun trace- ()
  (setf *tracing* nil))

(defun print-current-word (reg-w)
  (with-users-base
   (format t "~vT~A" (length *rstack*) (name-of reg-w))))

(defun execute-word (reg-w)
  (funcall (beh-of reg-w) reg-w))

(defun inner-interp (w)
  (nlet iter ((reg-w w))
    (when *tracing*
      (print-current-word reg-w))
    (execute-word reg-w)
    (when ip
      (go-iter ip@+))
    ))

;; --------------------------------------------
;; Calling into Forth from Lisp

(defun %call-into-forth (w &rest args)
  ;; Assumes *USER* has been established,
  ;; dictionary populated.
  (setf *pstack* (reverse args))
  (reverse (catch 'done
             (inner-interp w)
             *pstack*)))
  
(defun call-forth (w &rest args)
  (with-forth
    (apply #'%call-into-forth w args)))

(defun call-forth-by-name (name &rest args)
  (with-forth
   (let ((w  (must-find name)))
     (apply #'%call-into-forth w args))))

(defun call-forth-by-vocab-name (vocab name &rest args)
  (with-forth
   (let ((voc  (must-find vocab)))
     (assert (typep voc '<vocabulary>))
     (setf (context-voc) voc)
     (let ((w  (must-find name)))
       (apply #'%call-into-forth w args))
     )))

;; For more elaborate operations, use INTERPRET.

;; --------------------------------------------------
;; wtypes

(defun docol (self)
  ;; For : words
  (rp-! *reg-i*)
  (!ip (icode-of self)))

(defun doval (self)
  ;; For data words
  (sp-! (data-of self)))

(defun doscol (self)
  ;; For ;: words
  (doval self)
  (docol self))

(defun dovoc (self)
  ;; For VOCABULARY -- resets Context
  (!fcell *context* self))

(defun dolcl (self)
  ;; For LOCAL vars - act like Constants
  (destructuring-bind (lvl pos)
      (data-of self)
    (sp-! (aref (nth lvl *frstack*) pos))))

;; ----------------------------------------------------
;; defining words

(defvar *dict*)
(defvar *vocabs*)

(defun init-dict ()
  (setf *dict* (make-array 512
                           :adjustable   t
                           :fill-pointer 0)
        *vocabs* nil))

(defun last-def ()
  (latest-in-voc (current-voc)))

(defun link (w)
  (format t "Defining: ~A~&" (name-of w))
  (vector-push-extend w *dict*)
  (setf (latest-in-voc (current-voc)) w))

(defun immediate ()
  (when-let (last-def (last-def))
    (setf (is-immed last-def) t)))

(defun derive-word (parent &rest props)
  (setf %cur-def% (apply 'make-instance parent props)))

(defun link-derived-word (parent &rest parms)
  (link (apply #'derive-word parent parms)))

(defmethod initialize-instance :after ((obj named-linked-mixin) &key &allow-other-keys)
  (when (current-voc)
    (setf (prev-of obj) (last-def))))

;; ---------------------------------------------

(defclass <vocabulary> (<scode-def>)
  ()
  (:default-initargs
   :verb-type "VOCABULARY"
   :has-data? nil
   :immed     t
   :cfa       'dovoc
   ))

(defmethod initialize-instance :after ((self <vocabulary>) &key &allow-other-keys)
  (push self *vocabs*))


(defmacro vocabulary (name &rest args)
  (format t "vocabulary: ~A~&" name)
  `(link-derived-word '<vocabulary>
                      :nfa (string ',name)
                      ,@args))

;; ---------------------------------------------

(defmacro code (name &body body)
  (format t "code: ~A~&" name)
  `(link-derived-word '<code-def>
                      :nfa  (string ',name)
                      :cfa  (lambda (self)
                              (declare (ignorable self))
                              ,@body)
                      ))

;; ---------------------------------------------

(defclass <colon-def> (<code-def>)
  ((ifa       :accessor fw-ifa  :initarg :ifa))
  (:default-initargs
   :verb-type ":"
   :cfa       'docol
   :ifa       nil
   :has-data? nil
   ))

(defmacro colon (name &rest iwords)
  (format t "colon: ~A~&" name)
  `(link-derived-word '<colon-def>
                      :nfa   (string ',name)
                      :ifa   (forth-compile-words ',iwords)
                      ))

;; do-jmp -- an assist for tail calls
(let ((jv  (list nil *tic-exit*)))
  (defun default-jmp (obj)
    (setf (car jv)  obj
          (cadr jv) *tic-exit*)
    (!ip jv)))

(defgeneric do-jmp (self)
  (:method (self)
   (default-jmp self))
  (:method ((self <colon-def>))
   (!ip (icode-of self))))

;; ---------------------------------------------

(defclass <constant> (<scode-def>)
  ()
  (:default-initargs
   :verb-type "CONSTANT"
   :cfa       'doval
   :has-data? t
   ))

(defmacro const (name cval)
  (format t "const: ~A~&" name)
  `(link-derived-word '<constant>
                      :nfa  (string ',name)
                      :dfa  ,cval
                      ))

(defclass <scolon-def> (<scode-def> <colon-def>)
  ()
  (:default-initargs
   :verb-type ";:"
   :has-data? t
   :cfa       'doscol))

(defmethod do-jmp ((self <scolon-def>))
   (default-jmp self))

;; ---------------------------------------------

(defclass <local-accessor> (<scode-def>)
  ()
  (:default-initargs
   :verb-type "LOCAL"
   :has-data? nil
   :cfa       'dolcl
   ))

(defvar *local-cache-lock*  (mpc:make-lock))
(defvar *local-accessor-cache*
  #+:LISPWORKS
  (make-hash-table :test 'equalp
                   :single-thread t)
  #-:LISPWORKS
  (make-hash-table :test 'equalp))

(defun local-accessor (lvl pos)
  (let ((key (list lvl pos)))
    (or #1=(gethash key *local-accessor-cache*)
        (mpc:with-lock (*local-cache-lock*)
          (or #1#
              (setf #1#
                    (make-instance '<local-accessor>
                                   :dfa  key))
              ))
        )))

(defun not-a-var (dst)
  (error "Not a variable: ~A" (name-of dst)))

;; --------------------------------------------
;; Dynvars

(defclass <dynvar> (<scode-def>)
  ()
  (:default-initargs
   :verb-type "DYNVAR"
   :has-data? nil
   :dfa       nil
   :cfa       'do-dynvar
   ))

(defun do-dynvar (self)
  (sp-! (lookup-dynvar self)))

(defun lookup-dynvar (self)
  ;; return the list of bindings - the top one is the currently active
  ;; binding
  (maps:find dynvar-tree (name-of self)))

(defun add-dynvar (var val)
  (setf dynvar-tree
        (maps:add dynvar-tree (name-of var) val)))

(defun save-dynvars ()
  (setf *tic-initial-dynvars* dynvar-tree))

;; --------------------------------------------

(defgeneric to-oper (dst x)
  (:method ((dst <constant>) x)
   (let ((place (data-of dst)))
     (if (arrayp place) ;; are we actually a VARIABLE?
         (setf (@fcell place) x)
       (setf (data-of dst) x))
     ))
  (:method ((dst <local-accessor>) x)
   (destructuring-bind (lvl pos) (data-of dst)
     (setf (aref (nth lvl *frstack*) pos) x)
     ))
  (:method ((dst <dynvar>) x)
   (add-dynvar dst x))
  (:method (dst x)
   (not-a-var dst)))

;; ---------------------------------------------

(defun decompile (obj)
  (with-users-base
   (decompile-obj obj)))

(defmethod decompile-obj ((obj <constant>))
  (format t "~A ~A := ~A" (verb-type obj) (name-of obj) (data-of obj)))

(defmethod decompile-obj ((obj <colon-def>))
   (format t "~A ~A" (verb-type obj) (name-of obj))
   (decompile-ifa obj))

(defmethod decompile-obj ((obj <scolon-def>))
   (call-next-method)
   (decompile-ifa obj))

(defun decompile-ifa (obj)
  (nlet iter ((vs  (icode-of obj)))
    (when vs
      (let ((v   (first vs))
            (vtl (rest vs)))
        (princ #\space)
        (cond
         ((null v) ;; should only be termination of CASE clauses
          (princ "(esac)")
          (terpri))
         
         ((and (endp vtl)
               (eql v *tic-exit*))
          (princ ";"))
         
         ((is-immed v)
          (format t "[COMPILE] ~A" (name-of v)))
         
         ((string-equal (name-of v) "<ANON>")
          (terpri)
          (princ "{")
          (decompile-ifa v)
          (princ " }"))
         
         (t (print-name v))
         )
        (when (eql v *tic-lit*)
          (princ #\space)
          (prin1 (pop vtl)))
        (go-iter (if (member v *skip-words*)
                     (cdr vtl)
                   vtl)))
      ))
  (when (is-immed obj)
    (princ " IMMEDIATE")))

;; ----------------------------------------------------

(defun do-unary-op (op)
  (setf tos (funcall op tos)))

(defun do-binary-op (op)
  (let ((opnd2 sp@+))
    (setf tos (funcall op tos opnd2))))

(defun op-mapper (fn ops)
  `(progn
     ,@(mapcar #`(code ,(if (consp a1)
                            (car a1)
                          a1)
                   (,fn ',(if (consp a1)
                              (cadr a1)
                            a1)))
               ops)))

(defmacro define-unary-ops (&rest ops)
  (op-mapper 'do-unary-op ops))

(defmacro define-binary-ops (&rest ops)
  (op-mapper 'do-binary-op ops))

(defun dfloat (x)
  (float x 1d0))

(defun not-zerop (x)
  (not (zerop x)))

(defun roll (n lst)
  ;; destructive roll of list with no consing
  (labels ((stack-empty ()
             (report-error " stack empty")))
    (cond ((plusp n)
           (nlet iter ((n  n)
                       (l  lst)
                       (p  nil))
             (if (zerop n)
                 (progn
                   (unless l
                     (stack-empty))
                   (setf (cdr p) (cdr l)
                         (cdr l) lst)
                   l)
               ;; else
               (go-iter (1- n) (cdr l) l))))
          
          ((minusp n)
           (let ((ans (cdr lst)))
             (nlet iter ((n  n)
                         (l  ans)
                         (p  lst))
               (if (zerop n)
                   (progn
                     (unless p
                       (stack-empty))
                     (setf (cdr lst) l
                           (cdr p)   lst)
                     ans)
                 ;; else
                 (go-iter (1+ n) (cdr l) l)))))
          
          (t lst)
          )))

;; --------------------------------------------

(defun compile-lisp-text (text)
  (let ((text (concatenate 'string
                           "(lambda (self)"
                           "(declare (ignorable self))"
                           text
                           ")")))
    (compile nil (read-from-string text))))

;; ---------------------------------------------------
 
(defun run-interpreter (w)
  (catch 'done
    (forth-protect *dummy-prot*)
    (unwind-protect
        (inner-interp w)
      (forth-unwind))))

(defun run-interactive-interpreter (w)
  ;; make the interactive interpreter immune to bomb-outs
  (catch 'done
    (with-simple-restart (abort "Terminate Session")
      (um:nlet iter ()
        (with-simple-restart (abort "Continiue Session")
          (forth-protect *dummy-prot*)
          (unwind-protect
              (inner-interp w)
            (forth-unwind)))
        (reset-interpreter)
        (go-iter))
      )))

(defun reset-interpreter ()
  (setf *reg-i*     nil
        *rstack*    nil
        *frstack*   nil ;; leave *pstack* alone for now...
        *pad-stack* nil
        (fill-pointer *pad*) 0
        *display*   (list (make-frame))
        *unwind-chain* nil
        *dynvars*   (last *dynvars*))
  (setf (compiling?) nil)
  (reset-buffer))

(defgeneric init-interpreter ()
  (:method () ;; defmethod, to allow for :after methods in metacompiler
   (reset-interpreter)
   (!sp nil)))

;; -------------------------------------------------------------------

#+:LISPWORKS
(defun whitespace-char-p (c)
  (lw:whitespace-char-p c))

#|
(defun whitespace-char-p (c)
  (member c '(#\Tab #\Newline #\VT #\Page #\Return #\Space
                    #\Zero-Width-Space #\Line-Separator
                    #\Paragraph-Separator #\Ideographic-Space) ))
|#

#-:LISPWORKS
(defun whitespace-char-p (c)
  (member (char-code c) '(9 10 11 12 13 32 8203 8232 8233 12288) ))


(defun next-blank-delimited-word (buf pos)
  (unless (eq buf :eof)
    (nlet iter ()
      (when (or (null pos)
                (>= pos (length buf)))
        (setf buf (refill-buffer)
              pos 0))
      (unless (eq buf :eof)
        (if (setf pos (position-if (complement #'whitespace-char-p) buf
                                   :start pos))
            (let ((end (position-if #'whitespace-char-p buf
                                    :start (1+ pos))))
              (values (subseq buf pos end)
                      end))
          ;; else
          (go-iter)))
    )))
  

(defun next-blank-delimited-word-this-line (buf pos)
  (cond ((or (eq buf :eof)
             (null pos)
             (>= pos (length buf)))
         (values nil pos))
        
        ((setf pos (position-if (complement #'whitespace-char-p) buf
                                :start pos))
         (let ((end (position-if #'whitespace-char-p buf
                                 :start (1+ pos))))
           (values (subseq buf pos end)
                   end)))
        (t
         (values nil pos))
        ))
  

(defun word-to-end-of-line (buf pos)
  (unless (or (null pos)
              (>= pos (length buf)))
    (let ((end (position-if (curry #'char= #\newline) buf
                            :start pos)))
      (values (subseq buf pos end)
              end))
    ))

(defun word-to-delimiter (delim buf pos)
  (let ((test-fn (curry #'char= delim))
        (str  ""))
    (nlet iter ()
      (when (or (null pos)
                (>= pos (length buf)))
        (when pos
          (setf str (concatenate 'string str (list #\newline))))
        (setf buf (refill-buffer)
              pos 0))
      (if (eq buf :eof)
          str
        ;; else
        (let ((start pos))
          (setf pos (position-if test-fn buf
                                 :start start))
          (setf str (concatenate 'string str (subseq buf start pos)))
          (if pos
              (values str
                      (if (eql delim #\newline)
                          pos
                        (1+ pos)))
            (go-iter)))
        ))))

(defun word-to-delimiter-this-line (delim buf pos)
  (let ((test-fn (curry #'char= delim)))
    (cond ((or (eq buf :eof)
               (null pos)
               (>= pos (length buf)))
           (values nil pos))

          (t
           (let* ((start pos)
                  (end   (position-if test-fn buf
                                      :start start)))
             (values (subseq buf start end)
                     (if (eql delim #\newline)
                         pos
                       (1+ pos)))
             ))
          )))

(defun %next-word (delim buf pos)
  (case delim
    (#\space
     (next-blank-delimited-word buf pos))
    (#\newline
     (word-to-end-of-line buf pos))
    (t
     (word-to-delimiter delim buf pos))
    ))

(defun %next-word-this-line (delim buf pos)
  (case delim
    (#\space
     (next-blank-delimited-word-this-line buf pos))
    (#\newline
     (word-to-end-of-line buf pos))
    (t
     (word-to-delimiter-this-line delim buf pos))
    ))

;; --------------------------------------------

(defclass input-reader ()
  ((refill-fn  :accessor input-reader-refill-fn :initarg :refill)
   (buf        :accessor input-reader-buf       :initform nil)
   (pos        :accessor input-reader-pos       :initform nil)))

(defun make-input-reader (refill-fn)
  (make-instance 'input-reader
                 :refill refill-fn))

(defgeneric input-reader-read-next-word (rdr delim)
  (:method ((rdr input-reader) delim)
   (with-accessors ((buf  input-reader-buf)
                    (pos  input-reader-pos)) rdr
     (multiple-value-bind (str new-pos)
         (%next-word delim buf pos)
       (setf pos new-pos)
       str)
     )))

(defgeneric input-reader-read-next-word-this-line (rdr delim)
  (:method ((rdr input-reader) delim)
   (with-accessors ((buf  input-reader-buf)
                    (pos  input-reader-pos)) rdr
     (multiple-value-bind (str new-pos)
         (%next-word-this-line delim buf pos)
       (setf pos new-pos)
       str)
     )))

(defgeneric input-reader-refill-buffer (rdr)
  (:method ((rdr input-reader))
   (with-accessors ((buf       input-reader-buf)
                    (pos       input-reader-pos)
                    (refill-fn input-reader-refill-fn)) rdr
     (when (eq buf :eof)
       (report-error " EOF Error"))
     (setf pos 0
           buf (funcall refill-fn))
     )))

(defgeneric input-reader-reset (rdr)
  (:method ((rdr input-reader))
   (with-accessors ((buf  input-reader-buf)
                    (pos  input-reader-pos)) rdr
     (unless (eq buf :eof)
       (setf pos 0
             buf nil))
     )))

(defgeneric input-reader-skip-to-eol (rdr)
  (:method ((rdr input-reader))
   (with-accessors ((buf  input-reader-buf)
                    (pos  input-reader-pos)) rdr
     (when pos
       (let ((new-pos (position #\newline buf :start pos)))
         (setf pos (and new-pos
                        (1+ new-pos)))
         )))))

;; --------------------------------------------

(defun next-word (delim)
  (input-reader-read-next-word *next-word-reader* delim))

(defun next-word-this-line (delim)
  (input-reader-read-next-word-this-line *next-word-reader* delim))

(defun refill-buffer ()
  (input-reader-refill-buffer *next-word-reader*))

(defun reset-buffer ()
  (input-reader-reset *next-word-reader*))

(defun skip-to-eol ()
  (input-reader-skip-to-eol *next-word-reader*))

;; -------------------------------------------------------------------

(defun do-with-clean-system (rdr-fn fn)
  (with-forth
    (setf *next-word-reader* rdr-fn)
    (init-interpreter)
    (funcall fn)))

(defmacro with-clean-system (rdr-fn &body body)
  `(do-with-clean-system ,rdr-fn (lambda () ,@body)))

#+:LISPWORKS
(editor:setup-indent "with-clean-system" 1)

;; -------------------------------------------------------------------

(defun make-interactive-word-reader ()
  (flet ((next-buffer ()
           (let ((new-buffer (progn
                               (format t (if (compiling?) "~&] " "~&OK "))
                               (finish-output)
                               (read-line *standard-input* nil :eof))))
             (if (or (eq new-buffer :eof)
                     (and (= 3 (length new-buffer))
                          (string-equal "bye" new-buffer)))
                 :eof
               new-buffer))
           ))
    (make-input-reader #'next-buffer)))
           
(defun interactive (&optional (w *tic-outer*))
  (princ "Type BYE to exit...")
  (with-clean-system (make-interactive-word-reader)
    (run-interactive-interpreter w)))

;; -------------------------------------------------------------------

(defun make-strings-reader (&rest bufs)
  (flet ((next-buffer ()
           (or (pop bufs)
               :eof)))
    (make-input-reader #'next-buffer)))

(defun interpret (verbs-string &optional (w *tic-outer*))
  (with-clean-system (make-strings-reader verbs-string)
    (run-interpreter w)))

;; ---------------------------------------------------------------------

(defun forth-compile-in (word)
  (cond ((compiling?)
         (arena-append %cur-icode% word))
        ;; else
        (t
         (let ((w (last-def)))
           (if (and w
                    (has-data? w)
                    (vectorp (data-of w)))
               (setf (data-of w) (concatenate 'vector (data-of w) (vector word)))
             (report-error " Can't use \",\" here"))
           ))))

;; --------------------------------------------

(defun dbg (w)
  (with-users-base
   (let ((*print-level* 2)
         (*print-length* 10))
     (format t "------------------------~&")
     (format t "~A~&" (name-of w))
     (format t "  pstack:~&")
     (print *pstack*)
     (format t "  rstack:~&")
     (print *rstack*)
     (format t "  state = ~A~&" (compiling?))
     )))

;; -------------------------------

(defun basic-compile-literal (v)
  (forth-compile-in *tic-lit*)
  (forth-compile-in v))

(defun basic-push-literal (v)
  (sp-! v))

;; -------------------------------
;; ... in anticipation of separate treatment of rationals and floats

(defgeneric compile-literal (v)
  (:method (v)
   (basic-compile-literal v)))

(defgeneric push-literal (v)
  (:method (v)
   (basic-push-literal v)))

;; -------------------------------

(defun handle-found (word wrd-compile-fn)
  (if (and (compiling?)
           (not (is-immed word)))
      (funcall wrd-compile-fn word)
    ;; else
    (execute-word word)))

(defun not-found (str)
  (huh? str))

;; --------------------------------------------

(defun do-with-forth-standard-input (fn)
  (let ((*read-default-float-format* 'double-float)
        ;; (*read-eval* nil)
        (*read-base* @base))
    (funcall fn)))

(defmacro with-forth-standard-input (&body body)
  `(do-with-forth-standard-input (lambda ()
                                   ,@body)))

#|
 ;; interesting idioms
 (1+ x) := (- (lognot x))
 (1- x) := (lognot (- x))
|#

;; --------------------------------------------

(defun handle-not-found (str lit-compile-fn)
  ;; using the Lisp reader as much as possible
  (with-forth-standard-input
    (labels
        ((try-to-read (str)
           (with-forth-standard-input
             (ignore-errors
               (multiple-value-bind (obj pos)
                   (read-from-string str)
                 (when (>= pos (length str))
                   obj))
               )))
         (try-as-number (str)
           (let* ((nstr (concatenate 'string "#N|" str "|"))
                  (ans  (try-to-read nstr)))
             (and (numberp ans)
                  ans)
             ))
         (handle-it (val)
           (if (compiling?)
               (funcall lit-compile-fn val)
             (push-literal val))))
      
      (let ((v  (or (try-to-read str)
                    (try-as-number str))))
        (cond ((null v)
               (not-found str))
              
              ((and (consp v)
                    (eq (car v) 'quote))
               (handle-it (cadr v)))
              
              ((symbolp v)
               ;; Lisp reader sends us here with oddball encounters
               (if-let (ans (try-as-number str))
                   (handle-it ans)
                 (not-found str)))
              
              (t
               (handle-it v))
              )))))
  
(defun forth-handle-found (word)
  (handle-found word #'forth-compile-in))

(defun forth-handle-not-found (str)
  (handle-not-found str #'compile-literal))

;; -----------------------------------------------

#||#
(defun vlist ()
  (nlet iter ((voc (context-voc)))
    (when voc
      (format t "~&Vocabulary: ~A~%" (name-of voc))
      (dolinks (w (latest-in-voc voc))
        (princ (name-of w))
        (princ #\space))
      (go-iter (@fcell (data-of voc) 1))))
  (values))
#||#
#|
(defun vlist ()
  (nlet iter ((voc (context-voc)))
    (when voc
      (format t "~&Vocabulary: ~A~%" (name-of voc))
      (dolist (w (@fcell (data-of voc)))
        (princ (name-of w))
        (princ #\space))
      (go-iter (@fcell (data-of voc) 1))))
  (values))
|#
#||#
(defun forth-apropos (str)
  (nlet iter ((voc (context-voc))
              (ac  nil))
    (if voc
        (progn
          (dolinks (wp (latest-in-voc voc))
            (let ((name (string (name-of wp))))
              (when (search str name
                            :test #'char-equal)
                (pushnew name ac :test #'string-equal))))
          (go-iter (@fcell (data-of voc) 1) ac))
      ;; else
      (sort ac #'string-lessp)
      )))
#||#
#|
(defun forth-apropos (str)
  (nlet iter ((voc (context-voc))
              (ac  nil))
    (if voc
        (progn
          (dolist (wp  (@fcell (data-of voc)))
            (let ((name (string (name-of wp))))
              (when (search str name
                            :test #'char-equal)
                (pushnew name ac :test #'string-equal))))
          (go-iter (@fcell (data-of voc) 1) ac))
      ;; else
      (sort ac #'string-lessp)
      )))
|#
#||#
(defun catalog ()
  (let ((ac  (make-array 28
                         :initial-element nil))
        (pos nil)
        (tbl #."abcdefghijklmnopqrstuvwxyz"))
    (nlet iter ((voc (context-voc)))
      (if voc
          (progn
            (dolinks (wp (latest-in-voc voc))
              (let* ((name (string (name-of wp)))
                     (ch   (char name 0)))
                (cond ((digit-char-p ch)
                       (setf (aref ac 26)
                             (adjoin name (aref ac 26)
                                     :test #'string-equal)))
                      
                      ((setf pos (position ch tbl
                                           :test #'char-equal))
                       (setf (aref ac pos)
                             (adjoin name (aref ac pos)
                                     :test #'string-equal)))
                      
                      (t
                       (setf (aref ac 27)
                             (adjoin name (aref ac 27)
                                     :test #'string-equal))
                       ))
                ))
            (go-iter (@fcell (data-of voc) 1)))
        ;; else
        (progn
          (loop for ix from 0
                for lst across ac
                do
                  (setf (aref ac ix)
                        (sort lst #'string-lessp)))
          (coerce ac 'list)))
      )))
#||#
#|
(defun catalog ()
  (let ((ac  (make-array 28
                         :initial-element nil))
        (pos nil)
        (tbl #."abcdefghijklmnopqrstuvwxyz"))
  (nlet iter ((voc (context-voc)))
    (if voc
        (progn
          (dolist (wp (@fcell (data-of voc)))
            (let* ((name (string (name-of wp)))
                   (ch   (char name 0)))
              (cond ((digit-char-p ch)
                     (setf (aref ac 26)
                           (adjoin name (aref ac 26)
                                   :test #'string-equal)))
                    
                    ((setf pos (position ch tbl
                                         :test #'char-equal))
                     (setf (aref ac pos)
                           (adjoin name (aref ac pos)
                                   :test #'string-equal)))
                    
                    (t
                     (setf (aref ac 27)
                           (adjoin name (aref ac 27)
                                   :test #'string-equal))
                     ))
              ))
          (go-iter (@fcell (data-of voc) 1)))
      ;; else
      (progn
        (loop for ix from 0
              for lst across ac
              do
                (setf (aref ac ix)
                      (sort lst #'string-lessp)))
        (coerce ac 'list)))
    )))
|#
;; --------------------------------------------

(defun forgetter (wp)
  (let* ((pos  (position wp *dict*))
         (wrds (coerce (subseq *dict* pos) 'list)))
    ;;
    ;; The *DICT* is a chronologically ordered vector of verbs. We
    ;; trim from most recent verbs back to earlier entries.
    ;;
    ;; If any vocabularies are among the discarded words,
    ;; we can toss them out of the *VOCABS* list.
    ;;
    (setf *vocabs* (remove-if (um:rcurry #'find wrds) *vocabs*))

    ;; Next, for each remaining vocabulary, we check to find the first
    ;; word among the discards that is held in that vocabulary. If we
    ;; find one, then we know that all later words in that same
    ;; vocabulary are going to be discarded too.
    ;;
    ;; And that goes for later words, in the same vocabulary, among
    ;; the discards too. So we might as well trim them away from the
    ;; discards list.
    ;;
    (nlet iter ((vocs *vocabs*)
                (wrds wrds))
      (when (and vocs wrds)
        (let* ((voc   (car vocs))
               (tl    (cdr vocs))
               ;; (vwrds (defs-of-voc voc))
               ;; (test  (um:rcurry #'member vwrds))
               (vwrds  (latest-in-voc voc))
               (test   (um:curry #'find-entry-eq vwrds))
               (xwrds  (some test wrds)))
          (cond (xwrds
                 ;; (setf (defs-of-voc voc) (cdr xwrds))
                 (setf (latest-in-voc voc) (prev-of xwrds))
                 (go-iter tl (remove-if test wrds)))
                (t
                 (go-iter tl wrds))
                ))))
    ;; NIL out the deleted slots for GC
    (fill *dict* nil :start pos)
    (setf (fill-pointer *dict*) pos)
    (unless (find (current-voc) *vocabs*)
      (setf (current-voc) *tic-forth*))
    (unless (find (context-voc) *vocabs*)
      (setf (context-voc) *tic-forth*))
    ))

#|
(defun voc-of-wrd (wp)
  (dolist (voc *vocabs*)
    (when (member wp (defs-of-voc voc))
        (return-from voc-of-wrd voc))))
|#
(defun voc-of-wrd (wp)
  (dolist (voc *vocabs*)
    (when (find-entry-eq (latest-in-voc voc) wp)
      (return-from voc-of-wrd voc))))

;; -------------------------------------------------
;; Pathname from:
;;
;;    (editor:buffer-pathname (editor:current-buffer))
;;
;; Highlight sexpr and eval region.

(defvar *the-forth-kernel*
  (merge-pathnames
   #P"forth-itc-rpl-clos-forthcode.lisp" ;; should be companion file to this one...
   (translate-logical-pathname "PROJECTS:LISP;TOOLS;FORTH;")
   ;; #+:MAC #P"/Users/davidmcclain/projects/Lispworks/tools/Forth/forth-itc-rpl-clos-lispcode.lisp"
   ;; #+:WINDOWS #P"c:/Projects/Lispworks/tools/Forth/forth-itc-rpl-clos-lispcode.lisp"
   ))

(defun goforth ()
  (let ((*readtable* (copy-readtable)))
    (set-macro-character #\] nil)
    (set-macro-character #\} nil)
    (init-dict)
    (load *the-forth-kernel*)))

;; ----------------------------------------------------------

#+:LISPWORKS
(defun file-string (fname)
  (hcl:file-string fname))

#-:LISPWORKS
(defun file-string (fname)
  (with-open-file (f fname
                   :direction :input
                   :element-type 'character)
    (let ((ans (make-string (file-length f)
                            :element-type 'character)))
      (read-sequence ans f)
      (string-right-trim '(#\Null) ans)
      )))

(defun src-file (fname)
  (merge-pathnames
   fname
   (translate-logical-pathname
    "PROJECTS:LISP;TOOLS;FORTH;4TH-SRC;")))

(defun inhale (fname)
  (interpret (file-string (src-file fname))))

;; --------------------------------------------

(defun doit ()
  (goforth)
  (interactive))


(in-package :common-lisp-user)

(defpackage :forthrpl
  (:use #:common-lisp)
  (:import-from #:um
   #:if-let
   #:when-let
   #:nlet
   #:do-nothing
   #:curry
   #:dlambda)
  (:export
   #:interpret
   #:interactive
   ))

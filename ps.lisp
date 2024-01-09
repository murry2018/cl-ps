;;;; ps.lisp

(in-package #:ps)

(ql:quickload '(:named-readtables :cmu-infix :rutils :cl-interpol))

(named-readtables:defreadtable ps:syntax
  (:merge
   cmu-infix:syntax
   rutils:rutils-readtable
   :interpol-syntax))

(defun init ()
  (named-readtables:in-readtable ps:syntax))

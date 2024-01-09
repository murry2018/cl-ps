;;;; package.lisp

(defpackage #:ps
  (:use #:cl)
  (:import-from :rutils #:% #:%%)
  (:export
   #:syntax
   #:%
   #:%%
   #:scanf
   #:init))

(defpackage #:ps-user
  (:use #:common-lisp #:cl-user #:ps))

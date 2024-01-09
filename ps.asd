;;;; ps.asd

(asdf:defsystem #:ps
  :description "Describe ps here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:named-readtables #:cmu-infix #:cl-interpol #:rutils)
  :components ((:file "package")
               (:file "ps")
	       (:file "scanf")))

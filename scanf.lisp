;;; Quickstart
;;     > (ql:quickload :ps)
;;     > (setq x 0 y 0)
;;     > (setf (ps:scanf nil "~d ~d" x y) "991 125")
;;     > x
;;    991
;;     > y
;;    125
;;; What did I do?
;; 1. copying everthing from format-lisp.lisp (https://cl-pdx.com/static/format-setf.lisp)
;; 2. alternating from '(setf (format ' to '(setf (scanf '
;; 3. export as ps:scanf
(in-package #:ps)

;;; Original code from

;;;; format-setf.lisp -*- Package: FORMAT-SETF -*-
;;;;
;;;; A package to implement the (setf format) macro.
;;;; Author: Christophe Rhodes (csr21@cam.ac.uk)
;;;;
;;;; WARNING! WARNING! WARNING!
;;;;
;;;; The following is not very good code. Reading it may cause you to
;;;; fall over laughing. There are specific things I'm still not clear
;;;; on:
;;;;
;;;; 1. these eval-whens ... I can see the need for the first (around
;;;; the package stuff), but the one around #'whitespacep -- is that
;;;; necessary?
;;;;
;;;; 2. CMUCL has (if (find-package "FOO")
;;;;                  (rename-package "FOO" "FOO" 'nil)
;;;;                (make-package "FOO"))
;;;; instead of the first eval-when below. I don't get it. Is that
;;;; sensible? Does it prevent previous definitions from getting in the way?
;;;;
;;;; 3. Does the fact that I have
;;;; `(format-setf-integer ,directive ,pos ,value ,arguments ,control-string 16)
;;;; show that format-setf-integer should really be a function returning
;;;; a list?
;;;;
;;;; Yes. Since we know all the things at compile-time of (setf
;;;; format) form that we want to do the calculations of (*ugh*, that
;;;; construction is ugly) we should do the computation then and
;;;; there, and not wait for another pass of the macroexpander. I
;;;; think. Change made 2000-04-16.
;;;;
;;;; 4. Is there a nicer way than my global special variables hack to
;;;; have available the "outermost" value of the control-string
;;;; et. al. at all times?
;;;;
;;;; That's enough for now -- enjoy! Any and all feedback is welcome.

;; (package declaration is removed)

;;; from CMUCL format.lisp
;;; Thanks to William Lott, David Adam and Bill Maddox
;;;
;;; (required-argument) defaults removed.
(defstruct (format-directive
	    (:print-function %print-format-directive))
  (string "" :type simple-string)
  (start 0 :type (and unsigned-byte fixnum))
  (end #.most-positive-fixnum :type (and unsigned-byte fixnum))
  (character #\Null :type base-char)
  (colonp nil :type (member t nil))
  (atsignp nil :type (member t nil))
  (params nil :type list))

(defun %print-format-directive (struct stream depth)
  (declare (ignore depth))
  (print-unreadable-object (struct stream)
    (write-string (format-directive-string struct) stream
		  :start (format-directive-start struct)
		  :end (format-directive-end struct))))

(defun tokenize-control-string (string)
  (declare (simple-string string))
  (let ((index 0)
	(end (length string))
	(result nil))
    (loop
     (let ((next-directive (or (position #\~ string :start index) end)))
       (when (> next-directive index)
	 (push (subseq string index next-directive) result))
       (when (= next-directive end)
	 (return))
       (let ((directive (parse-directive string next-directive)))
	 (push directive result)
	 (setf index (format-directive-end directive)))))
    (nreverse result)))

(defun parse-directive (string start)
  (let ((posn (1+ start)) (params nil) (colonp nil) (atsignp nil)
	(end (length string)))
    (flet ((get-char ()
		     (if (= posn end)
			 (error 'format-error
				:complaint "String ended before directive was found."
				:control-string string
				:offset start)
		       (schar string posn))))
      (loop
       (let ((char (get-char)))
	 (cond ((or (char<= #\0 char #\9) (char= char #\+) (char= char #\-))
		(multiple-value-bind
		    (param new-posn)
		    (parse-integer string :start posn :junk-allowed t)
		  (push (cons posn param) params)
		  (setf posn new-posn)
		  (case (get-char)
		    (#\,)
		    ((#\: #\@)
		     (decf posn))
		    (t
		     (return)))))
	       ((or (char= char #\v) (char= char #\V))
		(push (cons posn :arg) params)
		(incf posn)
		(case (get-char)
		  (#\,)
		  ((#\: #\@)
		   (decf posn))
		  (t
		   (return))))
	       ((char= char #\#)
		(push (cons posn :remaining) params)
		(incf posn)
		(case (get-char)
		  (#\,)
		  ((#\: #\@)
		   (decf posn))
		  (t
		   (return))))
	       ((char= char #\')
		(incf posn)
		(push (cons posn (get-char)) params)
		(incf posn)
		(unless (char= (get-char) #\,)
		  (decf posn)))
	       ((char= char #\,)
		(push (cons posn nil) params))
	       ((char= char #\:)
		(if colonp
		    (error 'format-error
			   :complaint "Too many colons supplied."
			   :control-string string
			   :offset posn)
		  (setf colonp t)))
	       ((char= char #\@)
		(if atsignp
		    (error 'format-error
			   :complaint "Too many at-signs supplied."
			   :control-string string
			   :offset posn)
		  (setf atsignp t)))
	       (t
		(when (char= (schar string (1- posn)) #\,)
		  (push (cons (1- posn) nil) params))
		(return))))
       (incf posn))
      (let ((char (get-char)))
	(when (char= char #\/)
	  (let ((closing-slash (position #\/ string :start (1+ posn))))
	    (if closing-slash
		(setf posn closing-slash)
	      (error 'format-error
		     :complaint "No matching closing slash."
		     :control-string string
		     :offset posn))))
	(make-format-directive
	 :string string :start start :end (1+ posn)
	 :character (char-upcase char)
	 :colonp colonp :atsignp atsignp
	 :params (nreverse params))))))

;;; why is this in an eval-when? I don't get it...
(eval-when (compile load eval)
  (unless (fboundp 'whitespacep)
    (defparameter *whitespace-chars* 
      '(#\space #\tab #\newline #\return #\linefeed #\page))
    (defun whitespacep (char)
      (find char *whitespace-chars*))))

;;; From CMU repository, by Mark Kantrowitz.
;;;
;;; Change made to fix the index returned when junk-allowed is true
;;; (we effectively want to unread a character). Also, we now allow
;;; <num>{d,e,f,l,s}<exponent> format.
;;;
;;; FIXME: Change doc string.
(defun parse-float (string &key (start 0) end (radix 10) junk-allowed)
  "Converts a substring of STRING, as delimited by START and END, to a 
   floating point number, if possible. START and END default to the 
   beginning and end of the string. RADIX must be between 2 and 36. 
   A floating point number will be returned if the string consists of an
   optional string of spaces and an optional sign, followed by a string
   of digits optionally containing a decimal point, and an optional e or
   E followed by an optionally signed integer. The use of e/E to indicate
   an exponent only works for RADIX = 10. Returns the floating point
   number, if any, and the index for the first character after the number."
  ;; END defaults to the end of the string
  ;; We don't accomplish this by sticking (end (length string)) in the 
  ;; lambda list because I've encountered too many implementations that 
  ;; don't handle such properly. Also, this will work ok if somebody calls
  ;; the function with :end nil.
  (setq end (or end (length string))) 
  ;; Skip over whitespace. If there's nothing but whitespace, signal an error.
  (let ((index (or (position-if-not #'whitespacep string :start start :end end)
                   (if junk-allowed
                       (return-from parse-float (values nil end))
                     (error "No non-whitespace characters in number."))))
        (minusp nil) (decimalp nil) (found-digit nil) 
        (before-decimal 0) (after-decimal 0) (decimal-counter 0)
        (exponent 0)
	(exponent-char nil)
        (result 0))
    (declare (fixnum index))
    ;; Take care of optional sign.
    (let ((char (char string index)))
      (cond ((char= char #\-)
             (setq minusp t)
             (incf index))
            ((char= char #\+)
             (incf index))))
    (loop
     (when (= index end) (return nil))
     (let* ((char (char string index))
            (weight (digit-char-p char radix)))
       (cond ((and weight (not decimalp))
              ;; A digit before the decimal point
              (setq before-decimal (+ weight (* before-decimal radix))
                    found-digit t))
             ((and weight decimalp)
              ;; A digit after the decimal point
              (setq after-decimal (+ weight (* after-decimal radix))
                    found-digit t)
              (incf decimal-counter))
             ((and (char= char #\.) (not decimalp))
              ;; The decimal point
              (setq decimalp t))
             ((and (or (char-equal char #\d)
		       (char-equal char #\e)
		       (char-equal char #\f)
		       (char-equal char #\l)
		       (char-equal char #\s))
		   (= radix 10))
              (multiple-value-bind (num idx) 
                  (parse-integer string :start (1+ index) :end end
                                 :radix radix :junk-allowed junk-allowed)
		(unless num
		  ;; There wasn't an exponent after all -- the
		  ;; character detected is likely to be a false
		  ;; positive
		  (return nil))
                (setq exponent (or num 0)
		      exponent-char char
                      index idx)
		;; escape after this also when junk is allowed.
                (when (or junk-allowed (= index end)) (return nil))))
             (junk-allowed (return nil))
             ((whitespacep char)
              (when (position-if-not #'whitespacep string
                                     :start (1+ index) :end end)
                (error "There's junk in this string: ~S." string))
              (return nil))
             (t
              (error "There's junk in this string: ~S." string))))
     (incf index))
    ;; Cobble up the resulting number
    (setq result (coerce (* (+ before-decimal
			       (* after-decimal 
				  (expt radix (- decimal-counter))))
			    (expt radix exponent))
			 (case exponent-char
			   ((#\d) 'double-float)
			   ((#\e) 'float)
			   ((#\f) 'single-float)
			   ((#\l) 'long-float)
			   ((#\s) 'short-float)
			   (t 'float))))
    ;; Return the result
    (values
     (if found-digit
         (if minusp (- result) result)
       (if junk-allowed
           nil
         (error "There are no digits in this string: ~S" string)))
     index)))

;;; The rest of this is my code. All mine. Not yours. Mine. Ha.
;;;
;;; Only kidding. This code is available under some nebulous license
;;; that will be moved to some flavour of open-source license if
;;; anyone else actually uses this. Just do remember that I wrote
;;; it...  :-)
;;;
;;; But, I suppose, for the moment,
;;; © Christophe Rhodes, 2000.

;;; these will contain the "outermost" values of the control-string,
;;; value and arguments list.
(defvar *control-string* nil)
(defvar *value* nil)
(defvar *arguments* nil)

(defvar *maximal-depth* 20
  "The maximal depth assumed for format iterations, used to deal
   with the \"~{ ... ~}\" and \"~@{ ...~}\" case.")
  
(defun format-directives (control-string)
  "Returns (as multiple values) the directives in control-string."
  (values-list
   (loop for x across control-string
	 for i from 0
	 when (char= x #\~)
	 collect (parse-directive control-string i))))

(defmacro format-setf (stream control-string &rest args)
  ;; *control-string* will contain the original control string, so
  ;; that we can always know where we are. Similarly *value* and *arguments*.
  `(let ((*control-string* (or *control-string* ,control-string))
	 (*value* (or *value* ,(first (last args))))
	 (*arguments* (or *arguments* ',(butlast args))))
     (when ,stream
       (error "Non-nil stream: ~a" ,stream))
     ;; we ignore the possibility of ~:* for now. Also, we will probably
     ;; never get ~? working...
     ,(let ((value (first (last args)))
	    (arguments (butlast args)))
	;; We'll first need to find the first directive. If the strings
	;; don't agree before then, we'll give up.
	(let ((directives (multiple-value-list (format-directives control-string))))
	  (cond
	   ((some #'(lambda (x) (member (format-directive-character x) '(#\[))) directives)
	    (pretreat-conditional control-string directives arguments value))
	   (t (let ((pos (position #\~ control-string)))
		(if pos
		    `(progn
		       (unless (equal ,(subseq control-string 0 pos)
				      (subseq ,value 0 ,pos))
			 (error "Mismatch in string literals: ~s, ~s" ,control-string ,value))
		       ,(let ((directive (parse-directive control-string pos)))
			  (ccase (format-directive-character directive)
				 ((#\A #\a)
				  (format-setf-aesthetic directive pos value arguments control-string))
				 ((#\B #\b)
				  (format-setf-integer directive pos value arguments control-string 2))
				 ((#\C #\c)
				  (format-setf-character directive pos value arguments control-string))
				 ((#\D #\d)
				  (format-setf-integer directive pos value arguments control-string))
				 ((#\F #\f)
				  (format-setf-float directive pos value arguments control-string))
				 ((#\O #\o)
				  (format-setf-integer directive pos value arguments control-string 8))
				 ((#\R #\r)
				  (let ((radix (cdar (last (format-directive-params directive)))))
				    (if radix
					(format-setf-integer directive pos value arguments control-string radix)      
				      `(error "Roman numerals, cardinals and ordinals currently unsupported"))))
				 ((#\S #\s)
				  ;; see below for implementation decision
				  (format-setf-standard directive pos value arguments control-string))
				 ((#\T #\t)
				  (format-setf-tabulate directive pos value arguments control-string))
				 ((#\X #\x)
				  (format-setf-integer directive pos value arguments control-string 16))
				 ((#\%)
				  (format-setf-literal directive pos value arguments control-string #\Newline))
				 ((#\~)
				  (format-setf-literal directive pos value arguments control-string #\~))
				 ((#\{)
				  (format-setf-iteration directive pos value arguments control-string))
				 ((#\Newline)
				  `(setf (scanf nil ,(subseq control-string (format-directive-end directive)) ,@arguments) 
					 (subseq ,value ,pos)))
				 )))
		  `(progn
		     (unless (equal ,control-string ,value)
		       (error "Mismatch in string literals"))
		     ,value)))))))))


(defun pretreat-conditional (control-string directives arguments value)
  ;; here we go into pretreatment of the control-string. We convert
  ;; (setf (scanf nil "~a://~a~@[:~d]/~a" protocol host port path)
  ;;       "http://www-jcsu/~csr21/")
  ;;
  ;; =>
  ;;
  ;; (let (#:protocol #:host #:port #:path)
  ;;   (let ((#:true (ignore-errors (setf (scanf "~a://~a:~d/~a" #:protocol #:host #:port #:path) "http://www-jcsu/~csr21/")))
  ;;         (#:false (ignore-errors (setf (scanf "~a://~a/~a" #:protocol #:host #:path) "http://www-jcsu/~csr21/"))))
  ;;     (cond ...
  ;;
  ;; This really breaks in the presence of ~:*. We'll have to
  ;; preprocess ~* and ~^ before anything else.
  (let ((opening (find #\[ directives :key #'format-directive-character))
	;; for the moment, we'll assume that people don't nest conditionals.
	;; the logic here (and in the iteration case) is broken, because we
	;; can't simply cater for both "~{~a~}~{~a~}" and "~{~{~a~}~}".
	;; (although both of those are fairly broken).
	(closing (find #\] directives :key #'format-directive-character)))
    (let ((pre-string (subseq control-string 0 (format-directive-start opening)))
	  (conditional-string (subseq control-string (format-directive-end opening) (format-directive-start closing)))
	  (post-string (subseq control-string (format-directive-end closing)))
	  (atsignp (format-directive-atsignp opening))
	  (colonp (format-directive-colonp opening))
	  (nargs (length arguments)))
      (cond
       ((and atsignp colonp) `(error "Invalid format control string: ~~@:["))
       (atsignp
	(let ((gensyms (loop for x from 1 to nargs collect (gensym)))
	      (false-case (gensym))
	      (true-case (gensym)))
	  `(let ,gensyms
	     (let ((,true-case (ignore-errors (setf (scanf nil
							    ,(concatenate 'string pre-string conditional-string post-string)
							    ,@gensyms)
						    ,value)))
		   (,false-case (ignore-errors (setf (scanf nil
							     ,(concatenate 'string pre-string post-string)
							     ,@(cdr gensyms))
						     ,value))))
	       (cond
		;; hmm. I think that it's sensible to assume that we do the true case, no?
		;; maybe we should have a special variable (*favoured-conditional-branch*?)
		((and ,true-case ,false-case) (setf (scanf nil ,(concatenate 'string pre-string conditional-string post-string) ,@arguments) ,value))
		(,true-case (setf (scanf nil ,(concatenate 'string pre-string conditional-string post-string) ,@arguments) ,value))
		(,false-case
		 ,(let ((argument-position (format-setf-arguments-consumed pre-string)))
		    `(progn
		       (setf ,(elt arguments argument-position) nil) 
		       (setf (scanf nil ,(concatenate 'string pre-string post-string) ,@(subseq arguments 0 argument-position) ,@(subseq arguments (+ 1 argument-position)))
			     ,value))))
		(t (error "Error expanding conditional -- both false")))))))
       (colonp
	(let ((separator (find #\; directives :key #'format-directive-character)))
	  (let ((false-string (subseq control-string (format-directive-end opening) (format-directive-start separator)))
		(true-string (subseq control-string (format-directive-end separator) (format-directive-start closing)))
		(gensyms (loop for x from 1 to (1- nargs) collect (gensym)))
		(false-case (gensym))
		(true-case (gensym)))
	    `(let ,gensyms
	       (let ((,true-case (ignore-errors (setf (scanf nil
							      ,(concatenate 'string pre-string true-string post-string)
							      ,@(cdr gensyms))
						      ,value)))
		     (,false-case (ignore-errors (setf (scanf nil
							       ,(concatenate 'string pre-string false-string post-string)
							       ,@(cdr gensyms))
						       ,value))))
		 (cond
		  ((and ,true-case ,false-case) (error "Error expanding conditional -- both true"))
		  (,true-case
		   ,(let ((argument-position (format-setf-arguments-consumed pre-string)))
		      `(progn
			 (warn "Assuming truth value for ~a is T" ',(elt arguments argument-position))
			 (setf ,(elt arguments argument-position) t)
			 (setf (scanf nil ,(concatenate 'string pre-string true-string post-string) ,@(subseq arguments 0 argument-position) ,@(subseq arguments (+ 1 argument-position)))
			       ,value))))
		  (,false-case
		   ,(let ((argument-position (format-setf-arguments-consumed pre-string)))
		      `(progn
			 (setf ,(elt arguments argument-position) nil) 
			 (setf (scanf nil ,(concatenate 'string pre-string false-string post-string) ,@(subseq arguments 0 argument-position) ,@(subseq arguments (+ 1 argument-position)))
			       ,value))))
		  (t (error "Error expanding conditional -- both false"))))))))
       (t
	(let* ((conditional-directives (multiple-value-list (format-directives conditional-string)))
	       (nclauses (+ 1 (count #\; conditional-directives :key #'format-directive-character)))
	       (last-separator (find #\; conditional-directives :key #'format-directive-character :from-end t))
	       (argument-position (format-setf-arguments-consumed pre-string)))
	  (let ((result-gensyms (loop for x from 1 to nclauses collect (gensym)))
		(arg-gensyms (loop for x from 1 to (1- nargs) collect (gensym))))
	    `(let ,arg-gensyms
	       (let (,@(loop for start = 0 then (format-directive-end d)
			     with n = 0
			     for d in conditional-directives
			     if (char= (format-directive-character d) #\;)
			     collect `(,(nth n result-gensyms) (ignore-errors (setf (scanf nil
											    ,(concatenate 'string pre-string (subseq conditional-string start (format-directive-start d)) post-string)
											    ,@(cdr arg-gensyms))
										    ,value)))
			     do (incf n))
		       (,(nth (1- nclauses) result-gensyms) (ignore-errors (setf (scanf nil
											 ,(concatenate 'string pre-string (subseq conditional-string (format-directive-end last-separator)))
											 ,@(cdr arg-gensyms))
										 ,value))))
		 (unless (= (count-if #'identity (list ,@result-gensyms)) 1)
		   (error "Set of possible clauses for conditional does not have one element."))
		 (cond
		  ,@(loop for start = 0 then (format-directive-end d)
			  with n = 0
			  for d in conditional-directives
			  if (char= (format-directive-character d) #\;)
			  collect `(,(nth n result-gensyms)
				    (setf ,(elt arguments argument-position) ,n)
				    (setf (scanf nil
						  ,(concatenate 'string pre-string (subseq conditional-string start (format-directive-start d)) post-string)
						  ,@(subseq arguments 0 argument-position)
						  ,@(subseq arguments (+ 1 argument-position)))
					  ,value))
			  do (incf n))
		  (,(nth (1- nclauses) result-gensyms)
		   (setf ,(elt arguments argument-position) ,(if (format-directive-colonp last-separator)
								 `(progn (warn "Assuming -1 for default conditional clause") -1)
							       (1- nclauses)))
		   (setf (scanf nil ,(concatenate 'string pre-string (subseq conditional-string (format-directive-end last-separator)) post-string)
				 ,@(subseq arguments 0 argument-position)
				 ,@(subseq arguments (+ 1 argument-position)))
			 ,value)))
		    )))))))))


;;; Design decision: ~a is assumed to be fed a string. Yes, really. Tough.
(defun format-setf-aesthetic (directive pos value arguments control-string)
  ;; this isn't right yet -- we don't take account of ~~, ~%, ~t which produce useful literals.
  (let ((literal (subseq control-string
			 (format-directive-end directive)
			 (or (position #\~ control-string :start (format-directive-end directive)) (length control-string)))))
    (when (and (equal literal "")
	       (not (= (format-directive-end directive) (length control-string))))
      (warn "No literal following ~~a directive in (setf format): ~s" control-string))
    `(progn
       (setf ,(first arguments) (subseq ,value ,pos ,@(unless (= (format-directive-end directive)
							       (length control-string))
							`((search ,literal ,value :start2 ,pos)))))
       ,@(when (or (rest arguments) (not (equal (subseq control-string (format-directive-end directive)) "")))
	   `((setf (scanf nil ,(subseq control-string (format-directive-end directive)) ,@(rest arguments)) 
		   (subseq ,value (search ,literal ,value :start2 ,pos)))))
       ,value)))

(defun format-setf-tabulate (directive pos value arguments control-string)
  ;; atsign can be dealt with, colon invokes prettyprinter stuff.
  (let ((colonp (format-directive-colonp directive))
        (atsignp (format-directive-atsignp directive))
        ;; disregard ~vt and ~#t for now...
        (colnum (or (cdr (first (format-directive-params directive))) 1))
        (colinc (or (cdr (second (format-directive-params directive))) 1)))
    `(let* ((position-in-value-string (search ,value *value* :from-end t))
	    ;; this relatively hairy thing first looks up where the
	    ;; current value string is in terms of the global value, then
	    ;; finds the previous newline in the global value (since this
	    ;; is the only way we can guarantee finding the previous
	    ;; newline...
	    ;;
	    ;; the (+ 1 ...) is because we want
	    ;; position-*since*-last-newline, and the -1 is to make
	    ;; the base case come out right.
	    (position-since-last-newline (- (+ ,pos position-in-value-string)
					    (+ 1 (or (position #\Newline *value* :end position-in-value-string :from-end t) -1)))))
       ,(cond
	 (colonp nil)
	 ;; must .. implement .. error .. checking (i.e. something
	 ;; like in format-setf-literal)
	 (atsignp
	  ;; this seems to work, though I'm not sure where the 1-
	  ;; comes from.  There are probably off-by-lots errors at the
	  ;; boundaries.
	  (let ((nspaces (gensym)))
	    `(let ((,nspaces (1- (+ ,colnum (- ,colinc (mod (+ position-since-last-newline ,pos) ,colinc))))))
	       (progn
		 (setf (scanf nil ,(subseq control-string (format-directive-end directive))
			       ,@arguments)
		       (subseq ,value (+ ,pos ,nspaces)))
		 ,value))))
	 (t
	  ;; this apparently does work
	  (let ((nspaces (gensym)))
	    `(let ((,nspaces (if (< ,colnum (+ position-since-last-newline ,pos))
				 (mod (- ,colnum (+ position-since-last-newline ,pos)) ,colinc)
			       (- ,colnum (+ position-since-last-newline ,pos)))))
	       (progn
		 (setf (scanf nil ,(subseq control-string (format-directive-end directive))
			       ,@arguments)
		       (subseq ,value (+ ,pos ,nspaces)))
		 ,value))))))))
			 
;; (setf (scanf nil "~fe2" x) "1.4e2") doesn't really work at the
;; moment.
(defun format-setf-float (directive pos value arguments control-string)
  (let ((val (gensym))
	(posn (gensym)))
    `(multiple-value-bind (,val ,posn) (parse-float (subseq ,value ,pos) :junk-allowed t)
       ,@(unless (rest arguments) `((declare (ignorable ,posn))))
       (if (typep ,val 'float)
	   (progn (setf ,(first arguments) ,val)
		  ;; ,@nil is discarded
		  ;;
		  ;; although, now, I'm not sure that this hack is
		  ;; strictly necessary, since we've fixed the "no
		  ;; directives" case in format-setf.
		  ,@(when (rest arguments)
		      `((setf (scanf nil ,(subseq control-string (format-directive-end directive)) ,@(rest arguments)) 
			      (subseq ,value (+ ,pos ,posn)))))
		  ,value)))))

(defun format-setf-arguments-consumed (control-string)
  (let ((answer 0)
	(increment 0))
    (loop for x from 0 to (1- (length control-string))
	  if (equal (char control-string x) #\~)
	    do (let ((directive (parse-directive control-string x)))
		 (case (format-directive-character directive)
		   ((#\% #\~ #\Newline) (setf increment 0))
		   ((#\A #\a #\B #\b #\C #\c #\D #\d #\O #\o #\R #\r #\X #\x #\S #\s) (setf increment 1))))
	    and do (incf answer increment))
    answer))

(defun format-setf-iteration (directive pos value arguments control-string)
  (let* ((colonp (format-directive-colonp directive))
	 (atsignp (format-directive-atsignp directive))
	 ;; this used to read (close-pos (max (or (search "~}" control-string :from-end t) 0)
	 ;;                                   (or (search "~:}" control-string :from-end t) 0)))
	 ;; what was I thinking?
	 ;; Well, in fact neither is right.
	 (close-pos (max (or (search "~}" control-string :start2 pos) 0)
			 (or (search "~:}" control-string :start2 pos) 0)))
	 (following (+ 1 (position #\} control-string :start close-pos))))
    (cond
     ((and colonp atsignp)
      ;; hmm ... nargs has a slightly different usage here than below,
      ;; even if it is the same calculation.
      (let ((nargs (format-setf-arguments-consumed (subseq control-string (format-directive-end directive) close-pos))))
	`(progn
	   ,@(loop for a in arguments
		   collect `(setf ,a (make-list ,nargs)))
	   (setf (scanf nil
			 ,(apply #'concatenate 'string (append (make-list (length arguments) :initial-element (subseq control-string (format-directive-end directive) close-pos))
							       (list (subseq control-string following))))
			 ,@(loop for y in arguments
				 append (loop for x from 0 to (1- nargs)
					      collect `(nth ,x ,y))))
		 (subseq ,value ,pos))
	   ,value)))
     (colonp
      nil)
     ;; ignore ~^ and 0-length strings for now.
     (atsignp 
      (let ((nargs (format-setf-arguments-consumed (subseq control-string (format-directive-end directive) close-pos))))
	`(progn (setf (scanf nil 
			      ,(apply #'concatenate 'string (append (make-list (/ (length arguments) nargs) :initial-element (subseq control-string (format-directive-end directive) close-pos))
								    (list (subseq control-string following))))
			      ,@arguments)
		      (subseq ,value ,pos))
		,value)))
     (t
      ;; The thing we need now is something to count the number of things inside the iteration
      ;; then collect (or probably append) that number of gensyms for values below.
      (let ((len (gensym))
	    (vals (gensym))
	    (length-block (gensym)))
	`(multiple-value-bind (,len ,vals)
	     (block ,length-block
	       ,@(loop for x from *maximal-depth* downto 1
		       collect (let ((values (loop for y from 1 to x collect (gensym))))
				 `(let ,values
				    (if (ignore-errors (setf (scanf nil ,(expand-iteration control-string (format-directive-end directive) close-pos x) ,@values) ,value))
					(return-from ,length-block (values ,x (funcall #'list ,@values)))
				      nil)))))
	   (unless ,len
	     (error "No matching iteration case"))
	   (setf ,(first arguments) (make-list ,len))
	   (loop for foo from 0 to (1- ,len)
		 do (setf (nth foo ,(first arguments))
			  (nth foo ,vals)))
	   ,value)))
     )))

(defun expand-iteration (control-string start end n)
  (let ((s (make-string (* n (- end start)))))
    (loop for x from 0 to (1- n)
	  do (setf (subseq s (* x (- end start)) (* (+ x 1) end)) (subseq control-string start end)))
    s))

(defun format-setf-integer (directive pos value arguments control-string &optional (radix 10))
  (let ((val (gensym))
	(posn (gensym)))
    `(multiple-value-bind (,val ,posn) (parse-integer (subseq ,value ,pos) :radix ,radix :junk-allowed t)
       ,@(unless (rest arguments) `((declare (ignorable ,posn))))
       (if (typep ,val 'integer)
	   (progn (setf ,(first arguments) ,val)
		  ;; ,@nil is discarded
		  ,@(when (or (rest arguments) (not (equal (subseq control-string (format-directive-end directive)) "")))
		      `((setf (scanf nil ,(subseq control-string (format-directive-end directive)) ,@(rest arguments)) 
			      (subseq ,value (+ ,pos ,posn)))))
		  ,value)
	 (error "Error in format-setf-integer: failed to parse integer")))))

(defun format-setf-standard (directive pos value arguments control-string)
  (let ((val (gensym))
	(posn (gensym)))
    ;; erm, this needs an end, really.
    ;; (setf (scanf nil "~s://~s/~s" x y z) "http://www-jcsu.jesus.cam.ac.uk/~csr21/format-setf.lisp")
    ;; should work rather than complain about package errors...
    ;; something like :end (search ,(subseq control-string (format-directive-end directive) (start-of-new-directive)) value).
    ;;
    ;; Actually, on thinking about it, I'm no longer sure that this is
    ;; true. I think that ~s should leave the responsibility to the
    ;; user for making sure that whitespace is there (we could warn
    ;; when we statically detect ~s without following whitespace) and
    ;; that ~a should be the one to look for the following delimiter.
    `(multiple-value-bind (,val ,posn) (read-from-string ,value t nil :preserve-whitespace t :start ,pos)
       ,@(unless (rest arguments) `((declare (ignorable ,posn))))
       (progn (setf ,(first arguments) ,val)
	      ,@(when (rest arguments)
		  `((setf (scanf nil ,(subseq control-string (format-directive-end directive)) ,@(rest arguments)) 
			  (subseq ,value ,posn))))
	      ,value))))

(defun format-setf-literal (directive pos value arguments control-string character)
  (let ((n (or (cdar (format-directive-params directive)) 1)))
    `(if (every (lambda (x) (equal x ,character))
		(subseq ,value ,pos ,(+ pos n)))
	 (setf (scanf nil ,(subseq control-string (format-directive-end directive)) ,@arguments)
	       (subseq ,value ,(+ pos n)))
       (error "Mismatching number of ~as: ~s ~s" ,(or (char-name character) character) ,control-string ,value))))

(defun format-setf-character (directive pos value arguments control-string)
  ;; this one could do with rewriting or tidying...
  (let ((colonp (format-directive-colonp directive))
	(atsignp (format-directive-atsignp directive)))
    (cond
     ((and colonp atsignp) nil)
     (colonp nil)
     (atsignp (if (= (length (subseq control-string (format-directive-end directive))) 0)
		  `(progn (setf ,(first arguments) (read-from-string ,value t nil :start ,pos))
			  ,@(when (rest arguments)
			      `((error "Too many format arguments: ~a" ,arguments)))
			  ,value)
		(if (not (equal (char control-string (format-directive-end directive)) #\~))
		    ;; if the next thing in the control-string is not a
		    ;; directive, ignoring for the moment ~~ and ~%...
		    ;; first, is it compatible to take just the next
		    ;; character?
		    
		    ;; this is actually conceivably not the right behaviour; consider
		    ;; (setf (scanf nil "~@ce" x) "#\\Newlinee")
		    
		    ;; we probably want to match more than just the next character.
		    `(if (equal (char ,value ,(+ pos 3)) (char ,control-string ,(format-directive-end directive)))
			 (progn (setf ,(first arguments) (read-from-string ,value t nil :start ,pos :end ,(+ pos 3)))
				,@(when (rest arguments)
				    `((setf (scanf nil ,(subseq control-string (format-directive-end directive)) ,@(rest arguments))
					    (subseq ,value ,(+ pos 3)))))
				,value)
		       `(progn (setf ,(first arguments) (loop for x from ,pos to (length ,value)
							      if (and (equal (char ,control-string ,(format-directive-end directive)) (char ,value x))
								      (name-char (subseq ,value ,(+ pos 2) x)))
							      return it
							      finally (error "No matching character found")))
			      ,@(when (rest arguments)
				  `((setf (scanf nil ,(subseq control-string (format-directive-end directive)) ,@(rest arguments))
					  (subseq ,value ,(+ pos 3)))))
			      ,value)))))
     (t `(progn (setf ,(first arguments) (char ,value ,pos))
		,@(when (rest arguments)
		    `((setf (scanf nil ,(subseq control-string (format-directive-end directive)) ,@(rest arguments)) 
			    (subseq ,value ,(+ pos 1)))))
		,value)))))

(defsetf scanf format-setf)

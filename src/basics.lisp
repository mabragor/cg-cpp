
(in-package #:cg-c++)

(cl-interpol:enable-interpol-syntax)

(defun comment (str &key (rank :normal))
  (let ((comment-designator (cond ((eq :low rank) "//")
				  ((eq :high rank) "////")
				  ((eq :moderate rank) "///")
				  (t "//"))))
    (joinl "~%" (mapcar (lambda (x)
			  #?"$(comment-designator) $(x)")
			(split "\\n" str)))))
  
(defun simple-include (what &key (style :angular))
  (let ((what (if (or (not style) (eq style :angular))
		  #?"<$((unscore-if-not-string what))>"
		  #?'"$((unscore-if-not-string what))"')))
    #?"#include $(what)"))

(defun parse-out-keywords (kwd-lst lambda-list)
  (let ((kwds (make-array (length kwd-lst) :initial-element nil)))
    (iter (generate elt in lambda-list)
	  (if (keywordp (next elt))
	      (setf (elt kwds (position elt kwd-lst :test #'eq)) (next elt))
	      (collect elt into res))
	  (finally (return (nconc (iter (for kwd in-vector kwds)
					(collect kwd))
				  res))))))
	    

(defun include (&rest things)
  (destructuring-bind (style . things) (parse-out-keywords '(:style) things)
    (joinl "~%" (mapcar (lambda (x)
			  (simple-include x :style style))
			things))))

(defun make-using-name (name-spec)
  (joinl "::" (append (iter (for spec in (butlast name-spec))
			    (collect (if (not spec)
					 ""
					 (let ((*symbol-stringification-style* :camcase))
					   (stringify-if-symbol spec)))))
		      (list (stringify-if-symbol (car (last name-spec)))))))

(defun using (&rest name-spec)
  #?"using $((make-using-name name-spec));")
(defun using-typename (&rest name-spec)
  #?"using typename $((make-using-name name-spec));")
(defun using-namespace (&rest name-spec)
  #?"using namespace $((make-using-name name-spec));")

;; Consider the line:
;;   cout << setiosflags(ios::left);
;; what should be the syntax for it?
;;   (<< 'cout (setiosflags (:: '-ios 'left)))

;; OK, this is close to ideal, in real life it is much easier to
;; write something like
;;   (<< 'cout (call 'setiosflags (:: '-ios 'left)))
;; and then make the codewalker transform all the calls to the unknown functions in lisp
;; to calls to CALL

(defun << (&rest things)
  (joinl " << " (mapcar #'stringify-if-symbol things)))

(defun call (fname &rest args)
  (frnl "~a(~{~a~^, ~})"
	(stringify-if-symbol fname)
	(mapcar #'stringify-if-symbol args)))

;; OK, this is ugly, a better solution is needed here
(defun |::| (&rest names)
  (make-using-name names))

(defun c++return (what)
  #?"return $(what)")


(defun escaped-string-reader (stream char arg)
  "The actual reader function for the 'sub-character' #\?."
  (declare (ignore arg char))
  (let ((str (iter (for char in-string (read stream t nil t))
		   (cond ((char= char #\\) (progn (collect #\\ into res)
						  (collect #\\ into res)))
			 ((char= char #\") (progn (collect #\\ into res)
						  (collect #\" into res)))
			 (t (collect char into res)))
		   (finally (return (coerce res 'string))))))
    #?'"$(str)"'))

(defvar *previous-readtables* nil
  "A stack which holds the previous readtables that have been pushed
here by ENABLE-CG-C++-SYNTAX.")

(defun %enable-cg-c++-syntax ()
  "Internal function used to enable reader syntax and store current
readtable on stack."
  (push *readtable*
        *previous-readtables*)
  (setq *readtable* (copy-readtable))
  (set-dispatch-macro-character #\# #\s #'escaped-string-reader)
  (values))

(defun %disable-cg-c++-syntax ()
  "Internal function used to restore previous readtable." 
  (if *previous-readtables*
    (setq *readtable* (pop *previous-readtables*))
    (setq *readtable* (copy-readtable nil)))
  (values))

(defmacro enable-cg-c++-syntax ()
  "Enable CG-C++ reader syntax."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
    (%enable-cg-c++-syntax)))

(defmacro disable-cg-c++-syntax ()
  "Restore readtable which was active before last call to
ENABLE-CG-C++-SYNTAX. If there was no such call, the standard
readtable is used."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
    (%disable-cg-c++-syntax)))

(defmacro define-c++-comparator (name symbol)
  `(defun ,name (&rest args)
     (iter (for y in args)
	   (for x previous y)
	   (if-first-time nil
			  (collect #?"$((stringify-if-symbol x)) $(',symbol) $((stringify-if-symbol y))" into res))
	   (finally (return (joinl " && " res))))))
(define-c++-comparator c++< <)
(define-c++-comparator c++> >)
(define-c++-comparator c++>= >=)
(define-c++-comparator c++<= <=)
(define-c++-comparator c++== ==)

;; What remains to be done to completely generate hello.cpp
;; * for-loop
;; * if-block
;; * function-definitions
;; * (kinda done) literal-strings
;; * definitions of variables
;; * (done, but what about precedence?) inequalities
;; * ; at the end of some lines
;; * extra newlines for better human-readability


(defclass foo-class ()
  ((msg :accessor foo-msg :initarg :msg)))

(defun make-foo (msg)
  (make-instance 'foo-class :msg msg))

(defmethod print-object ((object foo-class) stream)
  (princ (foo-msg object) stream))

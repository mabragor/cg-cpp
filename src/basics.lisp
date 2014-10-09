
(in-package #:cg-c++)

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


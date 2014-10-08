
(in-package #:cg-c++)

(defun comment (str &key (rank :normal))
  (let ((comment-designator (cond ((eq :low rank) "//")
				  ((eq :high rank) "////")
				  ((eq :moderate rank) "///")
				  (t "//"))))
    (joinl "~%" (mapcar (lambda (x)
			  #?"$(comment-designator) $(x)")
			(split "\\n" str)))))
  

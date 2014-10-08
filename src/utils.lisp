
(in-package #:cg-c++)

(defun joinl (joinee lst)
  (format nil (concatenate 'string "~{~a~^" joinee "~}") lst))
(defun join (joinee &rest lst)
  (joinl joinee lst))

(defun frnl (format-str &rest args)
  (apply #'format `(nil ,format-str ,@args)))
(defun fart (format-str &rest args)
  (apply #'format `(t ,format-str ,@args)))

(defun camelcaseize (name)
  (format nil "~{~a~}" (mapcar #'string-capitalize (cl-ppcre:split "-" (string-downcase name)))))
(defun underscorize (name)
  (format nil "~{~a~^_~}" (cl-ppcre:split "-" (string-downcase name))))

(defun camcase-if-not-string (name)
  (if (stringp name)
      name
      (camelcaseize name)))

(defun unscore-if-not-string (name)
  (if (stringp name)
      name
      (underscorize name)))

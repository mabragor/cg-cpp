
(in-package #:cl-user)

(defpackage #:cg-c++-tests
  (:use #:cl #:cg-c++ #:fiveam #:iterate)
  (:export #:run-tests))


(in-package :cg-c++-tests)

(cl-interpol:enable-interpol-syntax)

(def-suite cg-c++)
(in-suite cg-c++)

(defun run-tests ()
  (let ((results (run 'cg-c++)))
    (explain! results)
    (unless (results-status results)
      (error "Tests failed."))))


(test comment
      (is (equal #?"// asdf" (cg-c++::comment "asdf")))
      (is (equal #?"// asdf" (cg-c++::comment "asdf" :rank :low)))
      (is (equal #?"/// asdf" (cg-c++::comment "asdf" :rank :moderate)))
      (is (equal #?"//// asdf" (cg-c++::comment "asdf" :rank :high)))
      (is (equal #?"//// asdf\n//// wer" (cg-c++::comment #?"asdf\nwer" :rank :high))))
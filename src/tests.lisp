
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

(test include
  (is (equal #?"#include <asdf>" (cg-c++::include "asdf")))
  (is (equal #?"#include <stdio.h>" (cg-c++::include 'stdio.h)))
  (is (equal #?"#include <stdio.h>\n#include <stdlib.h>" (cg-c++::include 'stdio.h 'stdlib.h)))
  (is (equal #?'#include "stdio.h"\n#include "stdlib.h"' (cg-c++::include 'stdio.h 'stdlib.h
									  :style t))))

(test using
  (is (equal "using Asdf::Asdf::asdf;" (cg-c++::using 'asdf 'asdf 'asdf)))
  (is (equal "using ::AsdfQwe::QWE::asdf;" (cg-c++::using nil 'asdf-qwe '*qwe 'asdf)))
  (is (equal "using typename Asdf::Asdf::asdf;" (cg-c++::using-typename 'asdf 'asdf 'asdf)))
  (is (equal "using typename ::AsdfQwe::QWE::asdf;" (cg-c++::using-typename nil 'asdf-qwe '*qwe 'asdf)))
  (is (equal "using namespace Asdf::Asdf::asdf;" (cg-c++::using-namespace 'asdf 'asdf 'asdf)))
  (is (equal "using namespace ::AsdfQwe::QWE::asdf;" (cg-c++::using-namespace nil 'asdf-qwe '*qwe 'asdf))))


(test if-node
  (is (equal #?"if (asdf)\n    asdf" (princ-to-string (cg-c++::c++if "asdf" (cg-c++::sn "asdf")))))
  (is (equal #?"if (asdf)\n    if (asdf)\n        asdf"
	     (princ-to-string (cg-c++::c++if "asdf"
					     (cg-c++::c++if "asdf"
							    (cg-c++::sn "asdf"))))))
  (is (equal #?"if (asdf) {\n    asdf;\n} else {\n    qwerty;\n}"
	     (princ-to-string (cg-c++::c++if "asdf"
					     "asdf"
					     "qwerty")))))
  

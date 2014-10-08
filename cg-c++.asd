;;;; cg-c++.asd

(asdf:defsystem #:cg-c++
  :serial t
  :description "Generate C++ code (and conveniently use it)"
  :author "Alexandr Popolitov <popolit@gmail.com>"
  :license "GPL"
  :depends-on (#:cl-trivial-templates #:cl-ppcre #:cl-interpol #:iterate)
  :pathname "src/"
  :components ((:file "package")
	       (:file "utils")
	       (:file "basics")))


(defsystem :cg-c++-tests
  :description "Tests for CG-C++."
  :licence "GPL"
  :serial t
  :depends-on (:cg-c++ :fiveam :iterate)
  :pathname "src/"
  :components ((:file "tests")))

(defmethod perform ((op test-op) (sys (eql (find-system :cg-c++))))
  (load-system :cg-c++-tests)
  (funcall (intern "RUN-TESTS" :cg-c++-tests)))



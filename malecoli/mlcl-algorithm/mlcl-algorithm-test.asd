;;;; 2008-08-05 15:59:48

(defpackage #:mlcl-dataset-test-asd
  (:use :asdf))

(in-package :mlcl-dataset-test-asd)

(defsystem mlcl-algorithm-test
  :name "mlcl-algorithm-test"
  :version "0.1"
  :components ((:module test
               	:components
	        	((:file "test01" :depends-on ()))))
  :depends-on ("mlcl-algorithm"))

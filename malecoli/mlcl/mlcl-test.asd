;;;; 2008-08-05 15:59:48

(defpackage #:mlcl-dataset-test-asd
  (:use :asdf))

(in-package :mlcl-test-asd)

(defsystem mlcl-test
  :name "mlcl-test"
  :version "0.1"
  :components (
               ;(:module test
               ;	:components
	       ; 	((:file "test01" :depends-on ())))
               )
  :depends-on ("mlcl"))

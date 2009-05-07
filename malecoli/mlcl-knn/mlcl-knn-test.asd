;;;; 2008-08-05 15:59:48

(defpackage #:mlcl-knn-test-asd
  (:use :asdf))

(in-package :mlcl-knn-test-asd)

(defsystem mlcl-knn-test
  :name "mlcl-knn-test"
  :version "0.1"
  :components (
               (:module test
               	:components
	        	((:file "test-arff" :depends-on ())))
               )
  :depends-on ("mlcl-knn"))

;;;; 2008-08-21 11:50:25

(defpackage #:mlcl-knn-asd
  (:use :cl :asdf))

(in-package :mlcl-knn-asd)

(defsystem mlcl-knn
  :name "mlcl-knn"
  :version "0.1"
  :components ((:module package
               	:components
	        	((:file "defpackage" :depends-on ())))
               (:module resources
               	:components
	        	((:file "resource" :depends-on ()))
                :depends-on ("package"))	
               (:module kb
               	:components
	        	((:file "knn-kb" :depends-on ()))
                 :depends-on ("package" "resources"))
               (:module knn
               	:components
	        	((:file "knn" :depends-on ()))
                 :depends-on ("package" "kb")))
  :depends-on ("mlcl-algorithm"))


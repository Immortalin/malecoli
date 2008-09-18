;;;; 2008-08-21 09:30:59

(defpackage #:mlcl-algorithm-asd
  (:use :cl :asdf))

(in-package :mlcl-algorithm-asd)

(defsystem mlcl-algorithm
  :name "mlcl-algorithm"
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
	        	((:file "algorithm-kb" :depends-on ()))
                 :depends-on ("package" "resources"))
               (:module algorithm
               	:components
	        	((:file "algorithm" :depends-on ())
	                 (:file "trivial-algorithm" :depends-on ()))		
                :depends-on ("package"))
               (:module make
               	:components
	        	((:file "makefile" :depends-on ()))	
                :depends-on ("package" "algorithm" "kb"))
               )
  :depends-on ("mlcl-dataset"))

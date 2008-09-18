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
               ;(:module algorithm
               ;	:components
	       ; 	((:file "algorithm" :depends-on ()))
               ; :depends-on ("package"))
               )
  :depends-on ("mlcl-dataset"))

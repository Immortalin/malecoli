;;;; 2008-08-21 12:54:28

(defpackage #:clone-cbr-asd
  (:use :cl :asdf))

(in-package :clone-cbr-asd)

(defsystem clone-cbr
  :name "clone-cbr"
  :version "0.1"
  :components ((:module package
               	:components
	        	((:file "defpackage" :depends-on ())))
               (:module api
               	:components
	        	((:file "cbr-api" :depends-on ()))
                 :depends-on ("log" "package"))
               (:module log
               	:components
	                ((:file "who" :depends-on ())	
	        	 (:file "log" :depends-on ("who")))
                 :depends-on ("package"))
               )
  
  :depends-on ("cl-kb" "clone-kb" "cl-who" "clone-ml"))

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
               ;(:module resources
               ;	:components
	       ; 	((:file "resource" :depends-on ()))
               ; :depends-on ("package"))
               ;(:module cbr
               ;	:components
	       ; 	((:file "onecbr" :depends-on ()))
               ;  :depends-on ("package" "resources"))
               (:module api
               	:components
	        	((:file "cbr-api" :depends-on ()))
                 :depends-on ("package"))
               )
  :depends-on ("cl-kb" "clone-kb"))

;;;; 2008-08-21 12:54:28

(defpackage #:clone-kb-asd
  (:use :cl :asdf))

(in-package :clone-kb-asd)

(defsystem clone-kb
  :name "clone-kb"
  :version "0.1"
  :components ((:module package
               	:components
	        	((:file "defpackage" :depends-on ())))
               (:module resources
               	:components
	        	((:file "resource" :depends-on ()))
                :depends-on ("package"))
               (:module model
               	:components
                	((:file "model" :depends-on ())
	        	 (:file "model-kb" :depends-on ("model"))
	        	 (:file "xml" :depends-on ("model" "model-kb"))
                	 (:file "model2kb" :depends-on ("model" "model-kb")))
                 :depends-on ("package" "resources"))
               (:module message
               	:components
                	((:file "message" :depends-on ())
	        	 (:file "xml" :depends-on ("message"))
                	 (:file "message2kb" :depends-on ("message")))
                 :depends-on ("package" "resources" "model"))
               )
  :depends-on ("cl-kb"))

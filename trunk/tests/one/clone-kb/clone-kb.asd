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
               (:module negotiation
               	:components
                	((:file "model" :depends-on ())
                	 (:file "message" :depends-on ())
                	 (:file "negotiation" :depends-on ("model" "message")))
                 :depends-on ("package"))
               (:module xml
               	:components
                	((:file "xml-model" :depends-on ())
	        	 (:file "xml-message" :depends-on ())
                	 (:file "xml-negotiation" :depends-on ("xml-model" "xml-message")))
                 :depends-on ("package" "resources" "negotiation"))
               (:module kb
               	:components
                	((:file "model-kb" :depends-on ())
                	 (:file "model2kb" :depends-on ("model-kb"))
                	 (:file "message2kb" :depends-on ())
                	 (:file "negotiation2kb" :depends-on ()))
                 :depends-on ("package" "resources" "negotiation"))
               )
  :depends-on ("cl-kb"))

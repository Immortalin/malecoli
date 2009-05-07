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
               (:module xml
               	:components
                	((:file "xml-model" :depends-on ("model"))
	        	 (:file "xml-message" :depends-on ("message"))
                	 (:file "xml-negotiation" :depends-on ("xml-model" "xml-message" "xml-state" "negotiation"))
                	 (:file "xml-state" :depends-on ("negotiation"))
                	 (:file "model" :depends-on ())
                	 (:file "message" :depends-on ())
                	 (:file "negotiation" :depends-on ("model" "message")))
                 :depends-on ("package" "resources"))
               (:module kb
               	:components
                	((:file "model-kb" :depends-on ()))
                 :depends-on ("package" "resources"))
               (:module html
               	:components
                	((:file "kb2html" :depends-on ()))
                 :depends-on ("package" "resources" "kb"))
               (:module xml2kb
               	:components
                	((:file "model2kb" :depends-on ())
                	 (:file "state2kb" :depends-on ("model2kb"))
                	 (:file "message2kb" :depends-on ("model2kb"))
                	 (:file "negotiation2kb" :depends-on ("message2kb" "state2kb" "model2kb")))
                 :depends-on ("package" "resources" "xml" "kb" "html"))
               )
  :depends-on ("cl-kb" "cl-who"))

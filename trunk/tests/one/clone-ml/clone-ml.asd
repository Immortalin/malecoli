;;;; 2008-08-21 12:54:28

(defpackage #:clone-ml-asd
  (:use :cl :asdf))

(in-package :clone-ml-asd)

(defsystem clone-ml
  :name "clone-ml"
  :version "0.1"
  :components ((:module package
               	:components
	        	((:file "defpackage" :depends-on ())))
               (:module kb
               	:components
	        	((:file "negotiation-kb" :depends-on ())
	        	 (:file "onenegotiation-kb" :depends-on ("negotiation-kb"))
	        	 (:file "gare-kb" :depends-on ("negotiation-kb"))	
  	        	 (:file "gare-instances-kb" :depends-on ("gare-kb" "negotiation-kb")))
                 :depends-on ("package"))
               (:module gare
               	:components
	        	((:file "gare2protege" :depends-on ()))
                 :depends-on ("package" "kb")))
  :depends-on ("mlcl-kb" "mlcl-dataset" "mlcl-cbr"))

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
               (:module resources
               	:components
	        	((:file "resource" :depends-on ()))
                :depends-on ("package"))
               (:module gare
               	:components
	        	((:file "gare2protege" :depends-on ()))
                 :depends-on ("package" "resources"))
               (:module onemodel
               	:components
                	((:file "model" :depends-on ())
	        	 (:file "model-kb" :depends-on ("model"))
	        	 (:file "xml" :depends-on ("model" "model-kb"))
                	 (:file "model2kb" :depends-on ("model" "model-kb"))
	        	 (:file "model2protege" :depends-on ("model" "model-kb")))
                 :depends-on ("package" "resources"))
               )
  :depends-on ("cl-kb" "mlcl" "mlcl-knn"))

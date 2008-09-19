;;;; 2008-08-21 09:30:59

(defpackage #:mlcl-asd
  (:use :cl :asdf))

(in-package :mlcl-asd)

(defsystem mlcl
  :name "mlcl"
  :version "0.1"
  :components ((:module package
               	:components
	        	((:file "defpackage" :depends-on ())))
               (:module resources
               	:components
	        	((:file "resource" :depends-on ()))
                :depends-on ("package"))
               (:module core
               	:components
                 	(
	        	 (:file "schema" :depends-on ("schema-compiler" "case"))
                 	 (:file "case" :depends-on ())
                 	 (:file "schema-compiler" :depends-on ())
                 	 )
                 :depends-on ("package" "kb"))
               (:module dataset
               	:components
                 	(
	        	 (:file "workspace" :depends-on ("dataset" "storage"))
	        	 (:file "dataset" :depends-on ("storage"))
	   	    	 (:file "storage" :depends-on ())
                 	 )
                 :depends-on ("package" "kb" "core"))
               (:module kb
                :components
	        	((:file "dataset-kb" :depends-on ()))
                 :depends-on ("package" "resources"))
               (:module arff
               	:components
	        	((:file "arff" :depends-on ()))
                 :depends-on ("kb"))
               )
  :depends-on ("mlcl-kb" "cl-ppcre" "cl-store"))

;;;; 2008-08-21 09:30:59

(defpackage #:mlcl-asd
  (:use :cl :asdf))

(in-package :mlcl-asd)

(defsystem mlcl
  :name "mlcl"
  :version "0.1"
  :components (
               (:module package
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
                 :depends-on ("package" "resources"))
               (:module dataset
               	:components
                 	(
	                 (:file "case-importer" :depends-on ())
	        	 (:file "dataset" :depends-on ("storage" "case-importer"))
	   	    	 (:file "storage" :depends-on ())
                 	)
                :depends-on ("package" "resources" "core"))
               (:module algorithm
               	:components
                 	(
	                 (:file "algorithm" :depends-on ())
	        	 (:file "trivial-algorithm" :depends-on ("algorithm"))
                 	)
                :depends-on ("package" "resources" "core"))
               (:module workspace
               	:components
                 	(
	                 (:file "workspace" :depends-on ("makefile"))
                 	 (:file "makefile" :depends-on ())
                 	)
                :depends-on ("package" "resources" "core" "dataset" "algorithm"))
               (:module dataset-arff
               	:components
	        	((:file "import-arff" :depends-on ()))
                :depends-on ("resources"))
               )
  :depends-on ("cl-kb" "cl-ppcre" "cl-store"))

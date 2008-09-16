;;;; 2008-08-21 09:30:59

(defpackage #:mlcl-dataset-asd
  (:use :cl :asdf))

(in-package :mlcl-dataset-asd)

(defsystem mlcl-dataset
  :name "mlcl-dataset"
  :version "0.1"
  :components ((:module package
               	:components
	        	((:file "defpackage" :depends-on ())))
               (:module resources
               	:components
	        	((:file "resource" :depends-on ()))
                :depends-on ("package"))
               (:module dataset
               	:components
                 	(
	        	 (:file "workspace" :depends-on ("schema" "dataset" "storage"))
	        	 (:file "dataset" :depends-on ("schema" "storage"))
	   	    	 (:file "storage" :depends-on ("case"))
	        	 (:file "schema" :depends-on ("schema-compiler"))
	        	 (:file "schema-compiler" :depends-on ())
	 		 (:file "case-importer" :depends-on ("schema-compiler"))		
	        	 (:file "case" :depends-on ())
                 	 )
                 :depends-on ("package" "kb"))
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

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
               (:module dataset
               	:components
	        	((:file "dataset" :depends-on ("dataset-schema"))
	        	 (:file "dataset-schema" :depends-on ())
	        ;	 (:file "generate-dataset" :depends-on ("dataset-schema" "kb-loader"))	
	        	 (:file "dataset-case" :depends-on ())
	        	 (:file "kb-loader" :depends-on ("dataset-case" "dataset-schema"))
	        	 )
                 :depends-on ("package" "kb"))
               (:module kb
                :components
	        	((:file "dataset-kb" :depends-on ()))
                 :depends-on ("package"))
               (:module arff
               	:components
	        	((:file "arff" :depends-on ()))
                 :depends-on ("kb"))
               )
  :depends-on ("mlcl-kb" "cl-ppcre" "cl-store"))

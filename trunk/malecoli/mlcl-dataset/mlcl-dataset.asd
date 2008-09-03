;;;; 2008-08-21 09:30:59

(defpackage #:mlcl-dataset-asd
  (:use :cl :asdf))

(in-package :mlcl-dataset-asd)

(defmethod asdf:perform :after ((o asdf:load-op)
	(c (eql (asdf:find-system 'clsql))))
	(funcall (find-symbol (symbol-name '#:push-library-path)
	(find-package 'clsql))
	#p"/hardmnt/tharpe0/sra/software/opt/sqlite/lib/"))

(defsystem mlcl-dataset
  :name "mlcl-dataset"
  :version "0.1"
  :components ((:module package
               	:components
	        	((:file "defpackage" :depends-on ())))
               (:module dataset
               	:components
	        	((:file "dataset" :depends-on ())
	        	 (:file "dataset-case" :depends-on ()))
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
  :depends-on ("mlcl-kb" "cl-ppcre" "clsql-sqlite3"))

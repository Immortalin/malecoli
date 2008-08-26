;;;; 2008-08-05 15:59:48

(defpackage #:mlcl-kb-asd
  (:use :cl :asdf))

(in-package :mlcl-kb-asd)

(defsystem mlcl-kb
  :name "mlcl-kb"
  :version "0.1"
  :components ((:module package
               	:components
	        	((:file "defpackage" :depends-on ())))
               (:module kb
               	:components
	        	((:file "frame" :depends-on ("kb-core"))
	        	 (:file "kb-core" :depends-on ())
	        	 (:file "kb" :depends-on ("kb-core" "frame")))
                 :depends-on ("package"))
               (:module xml
               	:components
	        	((:file "xml" :depends-on ("pprj"))
	        	 (:file "pprj" :depends-on ()))
                 :depends-on ("package" "kb"))
               (:module def
               	:components
	        	((:file "kb-def" :depends-on ()))
                 :depends-on ("package" "kb" "xml"))
               (:module protege
               	:components
	        	((:file "protege-kb" :depends-on ())
	                 (:file "frame" :depends-on ("protege-kb")))
                 :depends-on ("package" "kb" "def")))
  :depends-on ("s-xml" "cl-ppcre"))

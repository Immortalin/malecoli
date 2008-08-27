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
               (:module core
               	:components
	        	((:file "frame" :depends-on ("kb-core"))
	        	 (:file "kb-core" :depends-on ())
	        	 (:file "kb" :depends-on ("kb-core" "frame")))
                 :depends-on ("package"))
               (:module xml
               	:components
	        	((:file "xml" :depends-on ("pprj"))
	        	 (:file "pprj" :depends-on ()))
                 :depends-on ("package" "core"))
               (:module def
               	:components
	        	((:file "kb-def" :depends-on ()))
                 :depends-on ("package" "core" "xml"))
               (:module kb
               	:components
	        	((:file "protege-kb" :depends-on ())
	        	 (:file "kb" :depends-on ("protege-kb"))
	                 (:file "frame" :depends-on ("kb")))
                 :depends-on ("package" "core" "def")))
  :depends-on ("s-xml" "cl-ppcre"))

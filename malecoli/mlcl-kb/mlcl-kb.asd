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
	        	((:file "frame" :depends-on ("kb"))
	        	 (:file "kb" :depends-on ())
	        	 (:file "kb-utility" :depends-on ("kb" "frame")))
                 :depends-on ("package"))
               (:module xml
               	:components
	        	((:file "xml" :depends-on ("pprj"))
	        	 (:file "pprj" :depends-on ()))
                 :depends-on ("package" "core"))
               (:module def
               	:components
		       	((:file "kb-def" :depends-on ()))
                 :depends-on ("package" "core" "kbs"))
               (:module kbs
               	:components
	        	((:file "protege-kb" :depends-on ())
	        	 (:file "kb-kb" :depends-on ("protege-kb"))
	        	 )
                 :depends-on ("package" "core")))
  :depends-on ("s-xml" "cl-ppcre"))

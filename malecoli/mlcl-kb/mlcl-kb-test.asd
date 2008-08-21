;;;; 2008-08-05 15:59:48

(defpackage #:mlcl-kb-test-asd
  (:use :mlcl-kb :asdf))

(in-package :mlcl-kb-test-asd)

(defsystem mlcl-kb-test
  :name "mlcl-kb-test"
  :version "0.1"
  :components ((:module test
               	:components
	        	((:file "dump" :depends-on ())
	        	 (:file "test-protege" :depends-on ("dump")))))
  :depends-on ("mlcl-kb"))

;;;; 2008-08-05 15:59:48

(defpackage #:clone-kb-test-asd
  (:use :asdf))

(in-package :clone-kb-test-asd)

(defsystem clone-kb-test
  :name "clone-kb-test"
  :version "0.1"
  :components (
               (:module test
               	:components
	        	((:file "test-one" :depends-on ())))
               )
  :depends-on ("clone-kb"))

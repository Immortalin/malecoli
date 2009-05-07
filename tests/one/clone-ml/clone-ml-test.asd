;;;; 2008-08-05 15:59:48

(defpackage #:clone-ml-test-asd
  (:use :asdf))

(in-package :clone-ml-test-asd)

(defsystem clone-ml-test
  :name "clone-ml-test"
  :version "0.1"
  :components (
               (:module test
               	:components
	        	((:file "test-one" :depends-on ())))
               )
  :depends-on ("clone-ml"))

;;;; 2008-08-05 15:59:48

(defpackage #:clone-cbr-test-asd
  (:use :asdf))

(in-package :clone-cbr-test-asd)

(defsystem clone-cbr-test
  :name "clone-cbr-test"
  :version "0.1"
  :components (
               (:module test
               	:components
	        	((:file "test-one" :depends-on ())))
               )
  :depends-on ("clone-cbr"))

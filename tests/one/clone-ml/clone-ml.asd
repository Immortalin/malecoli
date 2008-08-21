;;;; 2008-08-21 12:54:28

(defpackage #:clone-ml-asd
  (:use :cl :asdf))

(in-package :clone-ml-asd)

(defsystem clone-ml
  :name "clone-ml"
  :version "0.1"
  :components ((:module package
               	:components
	        	((:file "defpackage" :depends-on ())))
               (:module kb
               	:components
	        	((:file "neg-kb" :depends-on ()))
                 :depends-on ("package")))
  :depends-on ("mlcl-kb" "mlcl-dataset" "mlcl-cbr"))

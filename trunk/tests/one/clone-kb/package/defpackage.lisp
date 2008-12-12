;;;; 2008-08-21 12:54:28


(in-package :common-lisp-user)

(defpackage :clone-kb
  (:nicknames :clone-kb)
  (:use :cl)
  (:export
    xml-negotiation-import
    xml-model-import
    xml-messages-import
    
    negotiation->kb
    model->kb
    model-instance->kb
    messages->kb
    *default-one-model-kb-pathname*
   ))


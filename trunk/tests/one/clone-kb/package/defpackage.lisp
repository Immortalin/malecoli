;;;; 2008-08-21 12:54:28

(in-package :common-lisp-user)

(defpackage :clone-kb
  (:nicknames :clone-kb)
  (:use :cl)
  (:export
    xml-negotiation-import
    xml-model-import
    negotiation-model->kb
    negotiation->kb
    
    kb->html
    kb->html-as-string
    agreement-issues->html-as-string
    
    date->string
   ))


;;;; 2008-08-21 12:54:28


(in-package :common-lisp-user)

(defpackage :clone-ml
  (:nicknames :clone-ml)
  (:use :cl)
  (:export
    cbr-add-negotiation-instance
    cbr-process-negotiation-instance
    cbr-save-workspaces
    cbr-clear-cache
    
    caseinfo
    caseinfo-model-id 
    caseinfo-instance-id
    caseinfo-name
    caseinfo-date
    caseinfo-html
    caseinfo-agreement-html
    caseinfo-current-state
    caseinfo-step-number
    caseinfo-participants
    
    find-caseinfo
   ))


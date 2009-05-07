;;;; 2008-08-21 12:54:28

(in-package :common-lisp-user)

(defpackage :clone-cbr
  (:nicknames :clone-cbr)
  (:use :cl)
  (:export
    *negotiation-instance*
    *negotiation-model*
    *messages*
    *endstate*
    update-case
    learn-case
    process-case
    update-model
    get-case-kb-project
    get-case-kb
    get-case-log
    
    
   ))


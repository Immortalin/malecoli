;;;; 2008-08-21 12:54:28


(in-package :common-lisp-user)

(defpackage :clone-cbr
  (:nicknames :clone-cbr)
  (:use :cl)
  (:export
    *negotiation-instance*
    *messages*
    *endstate*
    update-case
    learn-case
    process-case
    get-case-kb-project
    get-case-kb
    get-case-log
   ))


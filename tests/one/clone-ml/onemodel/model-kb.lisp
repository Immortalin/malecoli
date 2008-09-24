;;;; Created on 2008-08-25 13:26:16

(in-package :clone-ml)

(progn
  (defvar *default-one-model-kb-pathname*)
  (eval-when (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
    (if (null (boundp '*default-one-model-kb-pathname*))
        (setq *default-one-model-kb-pathname*            
              #p"/hardmnt/tharpe0/sra/serra/Software/Developing/MaLeCoLi/workspace/extra/one/kb/"))))


(defun model-fullname (name version)
  (format nil "~A~A" name version))

(defun find-model-kb (name version)
  (cl-kb:find-kb (model-fullname name version) nil))

(defun make-model-kb (name version &key (pathname (merge-pathnames
                                                   (make-pathname
                                                    :name (model-fullname name version)
                                                    :type "pprj" :case :local)
                                                   *default-one-model-kb-pathname*)))
  (let ((kb (cl-kb:make-kb pathname 
                     :use '(cl-kbs::|onenegotiation|))))
    (cl-kb:kb-create kb)
    (let ((this (cl-kb:mk-simple-instance (format nil "model @ ~A" (model-fullname name version)) 
                                          '|onenegotiation|::|one_model| 
                                          :kb kb)))
      (setf (cl-kb:frame-own-slot-value this '|negotiation|::|neg_model_id|) name)
      (setf (cl-kb:frame-own-slot-value this '|negotiation|::|neg_model_version|) version))
    (cl-kb:kb-save kb)
    kb))
  

(defun get-model-case (kb name version)
  (let ((id (format nil "case @ ~A" (model-fullname name version))))
    (cl-kb:get-cls id :kb kb)))

(defun get-model-context (kb name version)
  (let ((id (format nil "context @ ~A" (model-fullname name version))))
    (cl-kb:get-cls id :kb kb)))

(defun get-model-conclusion (kb name version)
  (let ((id (format nil "conclusion @ ~A" (model-fullname name version))))
    (cl-kb:get-cls id :kb kb)))

(defun get-model-protocol (kb name version)
  (let ((id (format nil "protocol @ ~A" (model-fullname name version))))
    (cl-kb:get-cls id :kb kb)))

(defun get-model-process (kb name version)
  (let ((id (format nil "process @ ~A" (model-fullname name version))))
    (cl-kb:get-cls id :kb kb)))

(defun get-model-item (kb name version)
  (let ((id (format nil "item @ ~A" (model-fullname name version))))
    (cl-kb:get-cls id :kb kb)))

(defun get-model-proposal (kb name version)
  (let ((id (format nil "proposal @ ~A" (model-fullname name version))))
    (cl-kb:get-cls id :kb kb)))

(defun get-model-base-issue (kb name version)
  (let ((id (format nil "issue @ ~A" (model-fullname name version))))
    (cl-kb:get-cls id :kb kb)))

(defun get-model-issue (kb name version issuename)
  (let ((id (format nil "~A @ ~A" issuename (model-fullname name version))))
    (cl-kb:get-cls id :kb kb)))


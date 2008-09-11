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
  (mlcl-kb:find-kb (model-fullname name version) nil))

(defun make-model-kb (name version &key (pathname (merge-pathnames
                                                   (make-pathname
                                                    :name (model-fullname name version)
                                                    :type "xml" :case :local)
                                                   *default-one-model-kb-pathname*)))
  (let ((kb (make-kb (model-fullname name version) 
                     :use-list (list 
                           'mlcl-kbs::PROTEGE-KB
                           'mlcl-kbs::dataset-kb
                           'mlcl-kbs::negotiation-kb
                           'mlcl-kbs::onenegotiation-kb)
                     :protege-file pathname)))
    (kb-create kb)
    (let ((this (make-simple-instance (format nil "model @ ~A" (model-fullname name version)) :kb kb)))
      (instance-add-direct-type this 'onenegotiation-kb::|one_model|)
      (setf (frame-own-slot-value this 'negotiation-kb::|neg_model_id|) name)
      (setf (frame-own-slot-value this 'negotiation-kb::|neg_model_version|) version))   
    kb))
  

(defun get-model-case (kb name version)
  (let ((id (format nil "case @ ~A" (model-fullname name version))))
    (get-cls id :kb kb)))

(defun get-model-context (kb name version)
  (let ((id (format nil "context @ ~A" (model-fullname name version))))
    (get-cls id :kb kb)))

(defun get-model-conclusion (kb name version)
  (let ((id (format nil "conclusion @ ~A" (model-fullname name version))))
    (get-cls id :kb kb)))

(defun get-model-protocol (kb name version)
  (let ((id (format nil "protocol @ ~A" (model-fullname name version))))
    (get-cls id :kb kb)))

(defun get-model-process (kb name version)
  (let ((id (format nil "process @ ~A" (model-fullname name version))))
    (get-cls id :kb kb)))

(defun get-model-item (kb name version)
  (let ((id (format nil "item @ ~A" (model-fullname name version))))
    (get-cls id :kb kb)))

(defun get-model-proposal (kb name version)
  (let ((id (format nil "proposal @ ~A" (model-fullname name version))))
    (get-cls id :kb kb)))

(defun get-model-base-issue (kb name version)
  (let ((id (format nil "issue @ ~A" (model-fullname name version))))
    (get-cls id :kb kb)))

(defun get-model-issue (kb name version issuename)
  (let ((id (format nil "~A @ ~A" issuename (model-fullname name version))))
    (get-cls id :kb kb)))


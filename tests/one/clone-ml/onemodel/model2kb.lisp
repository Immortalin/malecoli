;;;; Created on 2008-09-29 18:10:28

(in-package :clone-ml)

(defun import-model (model)
  (let ((kb (or (find-model-kb model)
                (make-model-kb model))))
    (cl-kb:kb-clear kb)
    (model->kb-schema model kb)
    kb))

(defun import-model-instance (model)
  (let ((kb (or (find-model-kb model)
                (import-model model))))
    (model->kb-instance model kb)))

(defun model->kb-schema (model kb)
  (cl-kb:with-kb kb t
                 (cl-kb:mk-cls (model-case-id model) :supercls '|onenegotiation|::|one_case|)
                 (let ((con (cl-kb:mk-cls (model-context-id model) :supercls '|onenegotiation|::|one_context|)))
                   nil)
                 ;  (dolist (attr (neginfo-attributes (model-neginfo model)))
                 ;    (add-attribute model kb attr con)))
                 (cl-kb:mk-cls (model-conclusion-id model) :supercls '|onenegotiation|::|one_conclusion|)
                 (let ((pro (cl-kb:mk-cls (model-protocol-id model) :supercls '|onenegotiation|::|one_protocol| )))
                   nil)
                 ;  (dolist (attr (protoinfo-attributes (model-protoinfo model)))
                 ;    (add-attribute model kb attr pro)))
                 (cl-kb:mk-cls (model-process-id model) :supercls '|onenegotiation|::|one_process|)
                 (let ((pro (cl-kb:mk-cls (model-item-id model) :supercls '|onenegotiation|::|one_item|)))
                   nil)
                 ;    (dolist (attr (item-attributes (infomodel-item (model-infomodel model))))
                 ;      (add-attribute model kb attr pro)))
                 (cl-kb:mk-cls (model-proposal-id model) :supercls '|onenegotiation|::|one_proposal|)
                 (cl-kb:mk-cls (model-issue-id model) :supercls '|onenegotiation|::|one_issue|)
                 ;    (dolist (is (item-issues (infomodel-item (model-infomodel model))))
                 ;      (add-issue model kb is issue-id proposal-id)))))
                 )
  kb)

(defun model->kb-instance (model kb)
  (cl-kb:with-kb kb t
                 (cl-kb:mk-simple-instance (instance-case-id model) (cl-kb:get-simple-instance (model-case-id model)))
                 (cl-kb:mk-simple-instance (instance-context-id model) (cl-kb:get-simple-instance (model-context-id model)))
                 (cl-kb:mk-simple-instance (instance-protocol-id model) (cl-kb:get-simple-instance (model-protocol-id model)))
                 (cl-kb:mk-simple-instance (instance-conclusion-id model) (cl-kb:get-simple-instance (model-conclusion-id model)))
                 (cl-kb:mk-simple-instance (instance-process-id model) (cl-kb:get-simple-instance (model-process-id model))))
  kb)

;
;
;

  
(defun model-full-name (model)
  (format nil "~A~A" (model-name model) (model-version model)))

(defun instance-full-name (model) 
  (format nil "~A~A~A" (model-name model) (model-version model) (model-id model)))  

(defun model-case-id (model)
  (format nil "case @ ~A" (model-full-name model)))

(defun model-context-id (model)
  (format nil "context @ ~A" (model-full-name model)))

(defun model-conclusion-id (model)
  (format nil "conclusion @ ~A" (model-full-name model)))

(defun model-protocol-id (model)
  (format nil "protocol @ ~A" (model-full-name model)))

(defun model-process-id (model)
  (format nil "process @ ~A" (model-full-name model)))

(defun model-proposal-id (model)
  (format nil "proposal @ ~A" (model-full-name model)))

(defun model-issue-id (model)
  (format nil "issue @ ~A" (model-full-name model)))

(defun model-item-id (model)
  (format nil "item @ ~A" (model-full-name model)))

(defun instance-case-id (model)
  (format nil "case @ ~A" (instance-full-name model)))

(defun instance-context-id (model)
  (format nil "context @ ~A" (instance-full-name model)))

(defun instance-conclusion-id (model)
  (format nil "conclusion @ ~A" (instance-full-name model)))

(defun instance-protocol-id (model)
  (format nil "protocol @ ~A" (instance-full-name model)))

(defun instance-process-id (model)
  (format nil "process @ ~A" (instance-full-name model)))

(defun instance-proposal-id (model)
  (format nil "proposal @ ~A" (instance-full-name model)))

(defun instance-issue-id (model)
  (format nil "issue @ ~A" (instance-full-name model)))


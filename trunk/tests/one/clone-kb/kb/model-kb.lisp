
;;;; Created on 2008-08-25 13:26:16

(in-package :clone-kb)

(progn
  (defvar *default-one-model-kb-pathname*)
  (eval-when (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
    (if (null (boundp '*default-one-model-kb-pathname*))
        (setq *default-one-model-kb-pathname*            
              cl-kb:*kb-default-path*))))


;
; kb names
;

(defun model-kb-name (model)
  (format nil "model-~A" (model-id model)))

(defun model-instance-kb-name (model) 
  (format nil "instance-~A" 
          (attribute-value 
           (find-if #'(lambda (x) (string-equal (attribute-name x) "id")) 
                    (neginfo-attributes (model-neginfo model))))))


;
; find kbs
;

(defun find-model-kb (model)
  (cl-kb:find-kb (model-kb-name model) nil t))

(defun find-model-instance-kb (model)
  (cl-kb:find-kb (model-instance-kb-name model) nil t))


;
; make kbs
;

(defun make-model-kb (model)
  (let ((pathname (merge-pathnames
                   (make-pathname
                    :name (model-kb-name model)
                    :type "pprj" :case :local)
                   *default-one-model-kb-pathname*)))
    (let ((kb (cl-kb:make-kb pathname :use '(cl-kbs::|onenegotiation|))))
      (cl-kb:kb-create kb)
      (init-model-kb model kb)
      (cl-kb:kb-save kb)
      kb)))

(defun make-model-instance-kb (model)
  (let ((pathname (merge-pathnames
                   (make-pathname
                    :name (model-instance-kb-name model)
                    :type "pprj" :case :local)
                   *default-one-model-kb-pathname*)))
    (let ((kb (cl-kb:make-kb pathname :use (list (find-model-kb model)))))
      (cl-kb:kb-create kb)
      (init-model-instance-kb model kb)
      (cl-kb:kb-save kb)
      kb)))

;
; init model kb
;

(defun init-model-kb (model kb)
  (let ((this (cl-kb:mk-simple-instance (format nil "model @ ~A" (model-kb-name model))
                                        '|onenegotiation|::|one_model| 
                                        :kb kb)))
    (setf (cl-kb:frame-own-slot-value this '|negotiation|::|neg_model_id|) (model-name model))
    (setf (cl-kb:frame-own-slot-value this '|negotiation|::|neg_model_version|) (model-version model))))
  
(defun init-model-instance-kb (model kb)
  (declare (ignore model)
           (ignore kb))
  nil)
  
;
; get kbs
;

(defun get-model-kb (model import-fn &optional (overwrite nil))
  (let ((kb (find-model-kb model)))
    (if (not kb)
        (progn
          (setf kb (make-model-kb model))
          (cl-kb:with-kb kb t
                         (funcall import-fn model kb)))
        (if overwrite
            (progn
              (cl-kb:kb-clear kb)
              (cl-kb:with-kb kb t
                             (init-model-kb model kb)
                             (funcall import-fn model kb)))))
    kb))
  
(defun get-model-instance-kb (model import-fn &optional (overwrite nil))
  (let ((kb (find-model-instance-kb model)))
    (if (not kb)
        (progn
          (setf kb (make-model-instance-kb model))
          (cl-kb:with-kb kb t
                         (funcall import-fn model kb)))
        (if overwrite
            (progn
              (cl-kb:kb-clear kb)
              (cl-kb:with-kb kb t
                             (init-model-instance-kb model kb)
                             (funcall import-fn model kb)))))
    kb))

;
; element's ids
;

(defun model-case-id (model)
  (declare (ignore model))
  (format nil "one_case"))

(defun model-context-id (model)
  (declare (ignore model))
  (format nil "one_context"))

(defun model-protocol-id (model)
  (declare (ignore model))
  (format nil "one_protocol"))

(defun model-conclusion-id (model)
  (declare (ignore model))
  (format nil "one_conclusion"))

(defun model-process-id (model)
  (declare (ignore model))
  (format nil "one_process"))


(defun instance-case-id (model)
  (format nil "case @ ~A" (model-instance-kb-name model)))

(defun instance-context-id (model)
  (format nil "context @ ~A" (model-instance-kb-name model)))

(defun instance-protocol-id (model)
  (format nil "protocol @ ~A" (model-instance-kb-name model)))

(defun instance-conclusion-id (model)
  (format nil "conclusion @ ~A" (model-instance-kb-name model)))

(defun instance-process-id (model)
  (format nil "process @ ~A" (model-instance-kb-name model)))





(defun model-model-id (model)
  (format nil "model @ ~A" (model-kb-name model)))
        



(defun model-proposal-id (model)
  (format nil "proposal @ ~A" (model-kb-name model)))

(defun model-issue-id (model)
  (format nil "issue @ ~A" (model-kb-name model)))

(defun model-item-id (model)
  (format nil "item @ ~A" (model-kb-name model)))


(defun instance-proposal-id (model)
  (format nil "proposal @ ~A" (model-instance-kb-name model)))

(defun instance-issue-id (model)
  (format nil "issue @ ~A" (model-instance-kb-name model)))
  
(defun instance-item-id (model)
  (format nil "item @ ~A" (model-instance-kb-name model)))

(defun model-msg-offer-id (model)
  (declare (ignore model))
  (format nil "neg_offer"))

(defun model-msg-adm-req-id (model)
  (declare (ignore model))
  (format nil "neg_adm_request"))

(defun model-msg-adm-res-id (model)
  (declare (ignore model))
  (format nil "neg_adm_response"))

(defun model-msg-offer-res-id (model)
  (declare (ignore model))
  (format nil "neg_offer_response"))

(defun model-msg-agreement-id (model)
  (declare (ignore model))
  (format nil "neg_agreement"))

(defun instance-msg-id (message model)
  (declare (ignore model))
  (format nil "msg ~A" (message-id message)))

(defun instance-party-id (id model)
  (declare (ignore model))
  (format nil "party ~A" id))




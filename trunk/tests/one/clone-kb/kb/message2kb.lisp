;;;; Created on 2008-11-17 17:15:12

(in-package :clone-kb)

(defun messages->kb (messages model)
  (let ((kb (get-model-instance-kb model #'model->kb-instance nil)))
    (cl-kb:with-kb kb t
                   (dolist (message messages)
                            (message->kb-instance message model kb)))))

(defun message->kb-instance (message model kb)
  (declare (ignore kb))
  (format t "MSG ID: ~A ~A~%" (message-id message) (message-type message))
  (cond
   ((eq (message-type message) 'OFFER)
    (cl-kb:mk-simple-instance (instance-msg-id message model) (cl-kb:get-cls (model-msg-offer-id model))))
   ((eq (message-type message) 'ADMISSION-REQUEST)
    (cl-kb:mk-simple-instance (instance-msg-id message model) (cl-kb:get-cls (model-msg-adm-req-id model))))
   ((eq (message-type message) 'ADMISSION-RESPONSE)
    (cl-kb:mk-simple-instance (instance-msg-id message model) (cl-kb:get-cls (model-msg-adm-res-id model))))
   ((eq (message-type message) 'OFFER-RESPONSE)
    (cl-kb:mk-simple-instance (instance-msg-id message model) (cl-kb:get-cls (model-msg-offer-res-id model))))
   ((eq (message-type message) 'AGREEMENT)
    (cl-kb:mk-simple-instance (instance-msg-id message model) (cl-kb:get-cls (model-msg-agreement-id model))))))
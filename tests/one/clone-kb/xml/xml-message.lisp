;;;; Created on 2008-11-17 17:15:29

(in-package :clone-kb)

;
; model import 
;

(defun xml-messages-import (pathname)
  (let ((messages nil))
    (with-open-file (strm pathname :direction :input)
                    (do (message seed)
                        ((eq 
                           (peek-char t strm nil :eof)
                           :eof))
                      (setf seed (make-message-seed))
                      (s-xml:start-parse-xml strm
                                             (make-instance 's-xml:xml-parser-state
                                                            :seed seed
                                                            :new-element-hook #'message-import-new-element-hook
                                                            :finish-element-hook #'message-import-finish-element-hook
                                                            :text-hook #'message-import-text-hook))
                      (setf message (message-seed-message seed))
                      (if (and 
                           (not (null (message-type message)))
                           (not (find-if #'(lambda (m) (string-equal (message-id m) (message-id message))) messages)))
                          (push message messages))))
    (nreverse messages)))


;
; model seed
;

(defstruct message-seed
  (message (make-message))
  
  (msg-attr nil)
  (attributes)
  
  (items nil)
  (issues nil)
  
  (offer nil)
  (responseto nil)
    
  (text nil)
  (id nil)
  (name nil)
  (value nil)
  (negotiation-role)
  (actors))


;
; xml hooks
;
  
(defun message-import-new-element-hook (name attributes seed)
  (declare (ignore attributes))
  (let ((new-seed (make-message-seed 
                   :message (message-seed-message seed)
                   :msg-attr (message-seed-msg-attr seed)
                   :items (message-seed-items seed))))
    (cond 
      ((eq name ':|org.one__project.metaclasses.impl.AttributeImpl|)
       (setf (message-seed-msg-attr new-seed) (make-msg-attr)))
      ((eq name ':|org.one__project.metaclasses.information.impl.IssueImpl|)
       (setf (message-seed-msg-attr new-seed) (make-msg-attr))) 
      ((eq name ':|org.one__project.metaclasses.information.impl.ItemImpl|)
       (setf (message-seed-msg-attr new-seed) (make-msg-attr))) 
      ((eq name ':|attribute|)
       (setf (message-seed-attributes new-seed) nil))   
      ((eq name ':|item|)
       (setf (message-seed-items new-seed) nil))
      ((eq name ':|offer|)
       (setf (message-seed-message new-seed) (make-message))
       (setf (message-seed-msg-attr new-seed) nil)
       (setf (message-seed-items new-seed) nil))
      ((eq name ':|offerReponse|)
       (setf (message-seed-message new-seed) (make-message))
       (setf (message-seed-msg-attr new-seed) nil)
       (setf (message-seed-items new-seed) nil)))
    new-seed))


(defun message-import-finish-element-hook (name attributes parent-seed seed)
  (cond 
   ;top level
   ((eq name ':|org.one__project.metaclasses.protocol.message.impl.AdmissionRequestImpl|)
    (make-msg 'admission-request seed))
   ((eq name ':|org.one__project.metaclasses.protocol.message.impl.AdmissionResponseImpl|)
    (make-msg 'admission-response seed))
   ((eq name ':|org.one__project.metaclasses.protocol.message.impl.OfferImpl|)
    (make-msg 'offer seed))
   ((eq name ':|org.one__project.metaclasses.protocol.message.impl.NotificationImpl|)
    (make-msg nil seed))
   ((eq name ':|org.one__project.metaclasses.protocol.message.impl.OfferResponseImpl|)
    (setf (message-seed-responseto seed) (message-seed-offer seed))
    (make-msg 'offer-response seed))
   ((eq name ':|org.one__project.metaclasses.protocol.message.impl.AgreementImpl|)
    (make-msg 'agreement seed))
   ; sender & receiver
   ((eq name ':|receiver|)
    (setf (message-receiver (message-seed-message seed)) 
          (message-seed-actors seed)))
   ((eq name ':|sender|)
    (setf (message-sender (message-seed-message seed)) 
          (make-actor :id (message-seed-id seed) 
                      :role (message-seed-negotiation-role seed))))
   ((eq name ':|negotiationRole|)
    (setf (message-seed-negotiation-role parent-seed) 
          (if (string-equal "org.one_project.metaclasses.protocol.role.impl.OwnerImpl"
                            (cdr (assoc ':|class| attributes)))
              ':owner ':participant)))
    ((eq name ':|org.one__project.metaclasses.impl.ActorImpl|)
    (push (make-actor :id (message-seed-id seed) 
                      :role (message-seed-negotiation-role seed))
          (message-seed-actors parent-seed)))
   ; negotiation id
   ((eq name ':|negotiationID|)
    (setf (message-negotiation-id (message-seed-message parent-seed)) 
          (message-seed-text seed)))
   ; id (for all ids)
    ((eq name ':|id|)
     (setf (message-seed-id parent-seed) (message-seed-text seed)))
    ((eq name ':|name|)
     (setf (message-seed-name parent-seed) (message-seed-text seed)))
   ; value (only for  top)
   ((and 
     (eq name ':|value|)
     (null (message-seed-msg-attr seed)))
    (setf (message-seed-value parent-seed) (message-seed-text seed)))
   ;
   ; attributes (for all)
   ;
   ((eq name ':|attribute|)
    (setf (message-seed-attributes parent-seed) (message-seed-attributes seed))) 
   ((eq name ':|org.one__project.metaclasses.impl.AttributeImpl|)
    (let ((attr (message-seed-msg-attr seed)))
      (setf (msg-attr-name attr) (message-seed-name seed))
      (push attr (message-seed-attributes parent-seed))))
   ((or 
     (eq name ':|name|)
     (eq name ':|value|)
     (eq name ':|type|)
     (eq name ':|isMandatory|)
     (eq name ':|isStatic|)
     (eq name ':|fileName|)
     (eq name ':|mimeType|))
    (let ((attr (message-seed-msg-attr seed))
          (val (message-seed-text seed)))
      ;(format t "DDDDD: ~A ~A~%" name attr)
      ;(format t "DDDDD: ~A ~A ~A~%" attributes parent-seed seed)
      (cond
      ((eq name ':|name|)
       (setf (msg-attr-name attr) val))
       ((eq name ':|value|)
        (setf (msg-attr-value attr) val))
       ((eq name ':|type|)
        (setf (msg-attr-type attr) val))
       ((eq name ':|isMandatory|)
        (setf (msg-attr-is-mandatory attr) val))
       ((eq name ':|isStatic|)
       (setf (msg-attr-is-static attr) val))
       ((eq name ':|fileName|)
        (setf (msg-attr-file-name attr) val))
       ((eq name ':|mimeType|)
        (setf (msg-attr-mine-type attr) val)))))
   ;
   ; items
   ;
   ((eq name ':|item|)
    (setf (message-seed-items parent-seed) (message-seed-items seed))) 
   ((eq name ':|org.one__project.metaclasses.information.impl.ItemImpl|)
    (let ((item (make-item :id (message-seed-id seed)
                           :attributes (message-seed-attributes seed)
                           :issues (message-seed-issues seed)
                           :name (message-seed-name seed))))
      (push item (message-seed-items parent-seed))))
   ;
   ; issues
   ;
   ((eq name ':|org.one__project.metaclasses.information.impl.IssueImpl|)
    (let ((issue (make-issue 
                  :id (message-seed-id seed)
                  :name (message-seed-name seed)
                  :attributes (message-seed-attributes seed))))
      (push issue (message-seed-issues parent-seed))))
   ((eq name ':|issue|)
    (setf (message-seed-issues parent-seed) (message-seed-issues seed)))
   ;
   ; responseto
   ;
   ((eq name ':|offer|)
    (setf (message-seed-offer parent-seed) (message-seed-message seed)))
   ((eq name ':|offerReponse|)
    (let ((msg (message-seed-offer seed)))
      (setf (message-seed-responseto parent-seed) msg)))
   (t 
    (format t "Not implemented: ~A ~A~%" name attributes)))
  parent-seed)

(defun message-import-text-hook (string seed)
  (setf (message-seed-text seed) string)
  seed)

(defun make-msg (type seed)
  (let ((msg (message-seed-message seed)))
    (setf (message-type msg) type)
    (setf (message-id msg)
          (message-seed-id seed))
    (setf (message-value msg)
          (message-seed-value seed))  
    (setf (message-attributes msg)
          (message-seed-attributes seed))
    (setf (message-items msg)
          (message-seed-items seed))
    (if (not (null (message-seed-responseto seed)))
        (setf (message-responseto msg) (message-id (message-seed-responseto seed))))
    msg))
  
;;;; Created on 2008-11-17 17:15:12

(in-package :clone-kb)

(defun messages->kb (messages model)
  (let ((kb (get-model-instance-kb model #'model->kb-instance nil)))
    (cl-kb:with-kb kb t
                   (dolist (message messages)
                            (message->kb-instance message model kb)))))

(defun message->kb-instance (message model kb)
  (cond
   ((eq (message-type message) 'OFFER)
    (let ((prsi (cl-kb:mk-simple-instance (instance-msg-id message model) (cl-kb:get-cls (model-proposal-id model))))
          (parties (get-party-simple-instances message model)))
      (setf (cl-kb:frame-own-slot-value prsi '|negotiation|::|neg_msg_sender|) (car parties))
      (dolist (p (cdr parties))
        (push p (cl-kb:frame-own-slot-values prsi '|negotiation|::|neg_msg_receiver|)))
      (setf (cl-kb:frame-own-slot-value prsi '|negotiation|::|neg_msg_timestamp|)
            (get-time-created message))
      (setf (cl-kb:frame-own-slot-value (cl-kb:find-simple-instance (instance-process-id model)) 
                                        '|negotiation|::|neg_case_proposal|) prsi)
      (dolist (item (message-items message))
        (let ((issueset-si (cl-kb:mk-simple-instance (format nil "issueset of ~A @ ~A" (item-name item) (message-id message)) 
                                                     (cl-kb:find-cls (model-issueset-id model item)))))
          (setf (cl-kb:frame-own-slot-value prsi (cl-kb:find-slot (format nil "has ~A" (model-issueset-id model item)))) issueset-si)
          (dolist (issue (item-issues item))
            (let ((issue-si (cl-kb:mk-simple-instance (format nil "issue ~A of ~A @ ~A" (issue-name issue) (item-name item) (message-id message)) 
                                                      (cl-kb:find-cls (model-issue-id model item issue)))))
              (setf (cl-kb:frame-own-slot-value issueset-si (cl-kb:find-slot (format nil "has ~A" (model-issue-id model item issue)))) issue-si)
              (dolist (attr (issue-attributes issue))
                (msg-attribute-value->slot-value model kb attr (cl-kb:find-cls (model-issue-id model item issue)) issue-si))))))
                                             
      ;(dolist (iscls (cl-kb:cls-direct-subclses (cl-kb:find-cls (model-issue-id model))))
      ;  (let ((issi (cl-kb:mk-simple-instance (format nil "~A ~A" (cl-kb:frame-name iscls) (message-id message)) iscls)))
      ;    (setf (cl-kb:frame-own-slot-value prsi (cl-kb:find-slot (format nil "has ~A " (cl-kb:frame-name iscls)))) issi)
      ;    (dolist (slt (cl-kb:cls-direct-template-slots iscls))
            ;(format t "IS=~A~%" (cl-kb:frame-name slt))
      ;      (let ((val (find-if #'(lambda (x) (string-equal (cl-kb:frame-name slt) (format nil "~A @ ~A~A " (msg-attr-name x) (model-name model) (model-version model))))
       ;                         (issue-attributes (car (item-issues (car (message-items message))))))))
              ;(format t "VAL=~A ~A ~%" (msg-attr-value val) (msg-attr-type val))
              ;(format t "REAL VAL=~A ~%" (value->kb-value (msg-attr-value val) (msg-attr-type val))
      ;        (setf (cl-kb:frame-own-slot-value issi slt) (value->kb-value (msg-attr-value val) (msg-attr-type val)))))))))
      #|
      (dolist (attr (issue-attributes (car (item-issues (car (message-items message))))))
        (let* ((issuename (format nil "~A @ ~A~A " (msg-attr-name attr) (model-name model) (model-version model) ))
               (issue (cl-kb:find-cls issuename nil)))
          (if issue
              (format t "~A~%" (msg-attr-name attr))
              (format t "NO: ~A~%" (msg-attr-name attr)))))))
   |#
  ))
   ((eq (message-type message) 'ADMISSION-REQUEST)
    )
   ((eq (message-type message) 'ADMISSION-RESPONSE)
    (if (string-equal (message-value message) "ACCEPT")
        (let ((party (car (cdr (get-party-simple-instances message model)))))
          (push party  
                (cl-kb:frame-own-slot-values (cl-kb:find-simple-instance (instance-context-id model)) 
                                             '|negotiation|::|neg_case_participant|)))))
   ((eq (message-type message) 'OFFER-RESPONSE)
    (let ((prsi (cl-kb:find-simple-instance (format nil "msg ~A" (message-responseto message)))))
      (setf (cl-kb:frame-own-slot-value prsi '|onenegotiation|::|neg_proposal_response|)
            (if (string-equal (message-value message) "ACCEPT") "accept" "reject"))))
   ((eq (message-type message) 'AGREEMENT)
    (let ((con (cl-kb:find-simple-instance (instance-conclusion-id model)))
          (win (car (cdr (get-party-simple-instances message model))))
          (prop (cl-kb:find-simple-instance (format nil "msg ~A" (message-responseto message)))))
      (setf (cl-kb:frame-own-slot-value con '|negotiation|::|neg_case_winner|) win)
      (setf (cl-kb:frame-own-slot-value con '|negotiation|::|neg_case_agreement|) prop)
      (setf (cl-kb:frame-own-slot-value con '|negotiation|::|neg_case_conclusion_date|) (get-time-created message))))))



;
;
;

(defun get-party-simple-instances (message model)
  (let ((res (cons nil nil)))
    (dolist (rec (message-receiver message))
        (push (get-party-simple-instance rec model) (cdr res)))
    (setf (car res) (get-party-simple-instance (message-sender message) model))
    res))

(defun get-time-created (message)
  (let ((tc (find-if #'(lambda (x) (string-equal (msg-attr-name x) "Time Created")) 
                     (message-attributes message))))
    (if (not (null tc))
        (let ((val (cl-ppcre:register-groups-bind (nil vm vd h m s nil vy) 
                                                  ("(\\w+)\\s+(\\w+)\\s+(\\w+)\\s+([\\w]+):([\\w]+):([\\w]+)\\s+(\\w+)\\s+(\\w+)" (msg-attr-value tc)) 
                                                  (date-instance vd (mount-name->number vm) vy h m s))))
          val))))

(defun party-id->party (id model)
  (if (string-equal id "Owner")
      (cl-kb:frame-own-slot-value 
       (cl-kb:find-simple-instance (instance-context-id model))
       '|negotiation|::|neg_case_owner|)
      (let ((party (cl-kb:find-simple-instance (instance-party-id id model) nil)))
        (if (null party)
            (progn
              (setf party (cl-kb:mk-simple-instance (instance-party-id id model) 
                                                    (cl-kb:find-cls "one_party")))
              (setf (cl-kb:frame-own-slot-value party '|onenegotiation|::|one_id|) id)))
        party)))
      

(defun get-party-simple-instance (act model)
  (let ((party (party-id->party (actor-id act) model)))
    party))
          
(defun mount-name->number (name)
  (cond
   ((string-equal name "Jan")
                 "1")
   ((string-equal name "Feb")
                 "2")
   ((string-equal name "Mar")
                 "3")
   ((string-equal name "Apr")
                 "4")
   ((string-equal name "May")
                 "5")
   ((string-equal name "Jun")
                 "6")
   ((string-equal name "Jul")
                 "7")
   ((string-equal name "Aug")
                 "8")
   ((string-equal name "Sep")
                 "9")
   ((string-equal name "Oct")
                 "10")   
   ((string-equal name "Nov")
                 "11")
   ((string-equal name "Dec")
                 "12")))
   
(defun value->kb-value (val type)
  (cond 
   ((string-equal type "Amount")
    (parse-integer val))
   ((string-equal type "Currency")
    val)
   (t 
    val)))


(defun msg-attribute-value->slot-value (model kb attr cls item)
  ;(format t "##$$##$$##   ~A   ~A~%" (attribute->slot-id attr model cls) (attribute-value attr))
  (let ((attrname (format nil "~A of ~A" (msg-attr-name attr) (cl-kb:frame-name cls))))
    (let ((at (cl-kb:get-slot attrname :kb kb))
          (typ (msg-attr-type attr))
          (val (msg-attr-value attr))
          (converted-value nil))      
      (if (and val (not (string-equal val "")))
          (progn 
            (setf converted-value 
                  (cond
                    ((string= typ "Date")
                     (cl-ppcre:register-groups-bind (vy vm vd) 
                                                    ("\\s*(\\w+)-(\\w+)-(\\w+)T" val) 
                                                    (date-instance vd vm vy)))
                    ((string= typ "String")
                     val)
                    ((or (string= typ "Integer") (string= typ "Amount"))
                     (let ((v (parse-integer val)))
                       (if (> v 2000000000)
                           0
                           v)))
                    ((string= typ "Boolean")
                     (if (string= val "true") t nil))
                    ((string= typ "negmod:OneImage")
                     nil)
                    ((string= typ "negmod:OneBinaryDocument")
                     nil)
                    ((string= typ "Currency")
                     val)
                    (t
                     (format t "Not implemented, yet. \"~A\" ~A~%" typ typ))
                  ))
            ;(format t "##$$##$$## ~A ~A  ~%" at converted-value)
            (setf (cl-kb:frame-own-slot-value item at) converted-value))))))
;(format t "##$$##$$## NIL~%")))))

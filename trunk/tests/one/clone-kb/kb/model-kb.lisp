
;;;; Created on 2008-08-25 13:26:16

(in-package :clone-kb)

;
; global variables
;

(defvar *model-id* nil)
(defvar *model-instance-id* nil)

;
; kb names
;

(defun model-kb-name (&optional (model-id *model-id*))
  (format nil "model-~A" model-id))

(defun model-instance-kb-name (&optional (model-instance-id *model-instance-id*)) 
  (format nil "instance-~A" model-instance-id))

;
; find kbs
;

(defun find-model-kb (&optional (model-id *model-id*))
  (cl-kb:find-kb (model-kb-name model-id) nil t))

(defun find-model-instance-kb (&optional (model-id *model-id*) (model-instance-id *model-instance-id*))
  (cl-kb:find-kb (model-instance-kb-name model-instance-id) nil t))


;
; make kbs
;

(defun make-model-kb (&optional (model-id *model-id*))
  (let ((kb (cl-kb:make-kb (format nil "~A.pprj" (model-kb-name model-id))
            :use '(cl-kbs::|onenegotiation|))))
    (cl-kb:kb-create kb)
    (cl-kb:with-kb kb t
                   (init-model-kb model-id kb))
    kb))

(defun make-model-instance-kb (model-id model-instance-id)
  (let ((kb (cl-kb:make-kb (format nil "~A.pprj" (model-instance-kb-name model-instance-id))
                           :use (list (find-model-kb model-id)))))
                           ;:use '(cl-kbs::|onenegotiation|))))
    (cl-kb:kb-create kb)
    (cl-kb:with-kb kb t
                   (init-model-instance-kb model-id model-instance-id kb))
    kb))


;
; init model kb
;

(defun init-model-kb (model-id kb)
  (let ((this (cl-kb:mk-simple-instance (g-model-id)
                                        '|onenegotiation|::|one_model| 
                                        :kb kb)))
    (setf (cl-kb:frame-own-slot-value this '|negotiation|::|neg_model_id|) model-id)))
  
(defun init-model-instance-kb (model-id model-instance-id kb)
  (declare (ignore model-instance-id))
  ;(init-model-kb model-id kb))
  )
  


(defun g-model-id ()
  (format nil "this-model"))

(defun g-model-proposal-id ()
  (format nil "this-proposal"))

(defun g-model-itemset-id ()
  (format nil "this-itemset"))

(defun g-model-item-id (item-name)
  (format nil "item@~A" item-name))

(defun g-model-m-item-id (item-name)
  (format nil "m-item@~A" item-name))

(defun g-model-m-item-attr-id (item-name attr-name)
  (format nil "m-attr@~A ~A" item-name attr-name))

(defun g-model-item-slot-id (item-name)
  (format nil "has ~A" (g-model-item-id item-name)))

(defun g-model-issueset-id (item-name)
  (format nil "issueset of ~A" (g-model-item-id item-name)))

(defun g-model-issueset-slot-id (item-name)
  (format nil "has ~A" (g-model-issueset-id item-name)))

(defun g-model-issue-id (item-name issue-name)
  (format nil "issue@~A of ~A" issue-name (g-model-item-id item-name)))

(defun g-model-m-issue-id (item-name issue-name)
  (format nil "m-issue@~A of ~A" issue-name (g-model-item-id item-name)))

(defun g-model-m-issue-attr-id (item-name issue-name attr-name)
  (format nil "m-attr@~A ~A ~A" issue-name (g-model-item-id item-name) attr-name))

(defun g-model-issue-slot-id (item-name issue-name)
  (format nil "has ~A" (g-model-issue-id item-name issue-name)))


(defun g-model-cls-slot-id (cls-name attr-name)
    (cond
     ((string-equal attr-name "allowNested")
      "one_allow_nested")
     ((string-equal attr-name "name")
      "one_name")
     ((string-equal attr-name "id")
      "one_id")
     ((string-equal attr-name "visibility")
      "one_visibility")
     ((string-equal attr-name "parentId")
      "one_parent_id")
     ((string-equal attr-name "creationDate")
      "one_creation_date")
     ((string-equal attr-name "endDate")
      "neg_case_ending_date")
     ((string-equal attr-name "startDate")
      "neg_case_starting_date")
     (t
      (format nil "has ~A of ~A" attr-name cls-name))))

(defun g-model-case-id ()
  (format nil "one_case"))

(defun g-model-context-id ()
  (format nil "one_context"))

(defun g-model-protocol-id ()
  (format nil "one_protocol"))

(defun g-model-conclusion-id ()
  (format nil "one_conclusion"))

(defun g-model-process-id ()
  (format nil "one_process"))

(defun g-instance-case-id ()
  (format nil "the case"))

(defun g-instance-context-id ()
  (format nil "the context"))

(defun g-instance-protocol-id ()
  (format nil "the protocol"))

(defun g-instance-conclusion-id ()
  (format nil "the conclusion"))

(defun g-instance-process-id ()
  (format nil "the process"))

(defun g-instance-itemset-id ()
  (format nil "the itemset"))

(defun g-instance-item-id (item-name)
  (format nil "the ~A" (g-model-item-id item-name)))

(defun g-instance-issueset-id (item-name msg-id)
  (format nil "the ~A || ~A" (g-model-issueset-id item-name) msg-id))

(defun g-instance-issue-id (item-name issue-name msg-id)
  (format nil "the ~A || ~A" (g-model-issue-id item-name issue-name) msg-id))

(defun g-instance-party-id (id)
  (format nil "party ~A" id))


(defun g-model-msg-offer-id ()
  (format nil "neg_offer"))

(defun g-model-msg-adm-req-id ()
  (format nil "neg_adm_request"))

(defun g-model-msg-adm-res-id ()
  (format nil "neg_adm_response"))

(defun g-model-msg-offer-res-id ()
  (format nil "neg_offer_response"))

(defun g-model-msg-agreement-id ()
  (format nil "neg_agreement"))

(defun g-instance-msg-id (message-id)
  (format nil "msg ~A" message-id))


(defun date->string (di) 
  (let ((y (cl-kb:frame-own-slot-value di '|dataset|::|time_year|))
        (m (cl-kb:frame-own-slot-value di '|dataset|::|time_month|))
        (d (cl-kb:frame-own-slot-value di '|dataset|::|time_day|))
        (h (cl-kb:frame-own-slot-value di '|dataset|::|time_hour|))
        (mi (cl-kb:frame-own-slot-value di '|dataset|::|time_minute|))
        (sec (cl-kb:frame-own-slot-value di '|dataset|::|time_sec|))
        (usec (cl-kb:frame-own-slot-value di '|dataset|::|time_usec|)))
    (format nil "~A-~A-~AT~A:~A:~A.~A" y m d h mi sec usec)))
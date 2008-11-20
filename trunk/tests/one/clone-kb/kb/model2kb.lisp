
;;;; Created on 2008-09-29 18:10:28

(in-package :clone-kb)

;
; translare models and model instances into kbs
;

(defun model->kb (model &optional (overwritep nil))
  (get-model-kb model #'model->kb-schema overwritep))

(defun model-instance->kb (model &optional (overwritep nil))
  (get-model-instance-kb model #'model->kb-instance overwritep))

;
; conversion
;

(defun model->kb-schema (model kb)
  (let ((it (cl-kb:mk-cls (model-item-id model) :supercls '|onenegotiation|::|one_item|)))
    (dolist (attr (item-attributes (infomodel-item (model-infomodel model))))
      (attribute->slot model kb attr it)))
  (cl-kb:mk-cls (model-proposal-id model) :supercls '|onenegotiation|::|one_proposal|)
  (cl-kb:mk-cls (model-issue-id model) :supercls '|onenegotiation|::|one_issue|)
  (dolist (is (item-issues (infomodel-item (model-infomodel model))))
    (issue-> model kb is (model-issue-id model) (model-proposal-id model)))
  kb)

(defun model->kb-instance (model kb)
  (let ((ca (cl-kb:mk-simple-instance (instance-case-id model) (cl-kb:get-cls (model-case-id model))))
        (co (cl-kb:mk-simple-instance (instance-context-id model) (cl-kb:get-cls (model-context-id model))))
        (pr (cl-kb:mk-simple-instance (instance-protocol-id model) (cl-kb:get-cls (model-protocol-id model))))
        (cn (cl-kb:mk-simple-instance (instance-conclusion-id model) (cl-kb:get-cls (model-conclusion-id model))))
        (po (cl-kb:mk-simple-instance (instance-process-id model) (cl-kb:get-cls (model-process-id model))))
        (it (cl-kb:mk-simple-instance (instance-item-id model) (cl-kb:get-cls (model-item-id model)))))
    (setf (cl-kb:frame-own-slot-value ca '|negotiation|::|neg_case_context|) co)
    (setf (cl-kb:frame-own-slot-value ca '|negotiation|::|neg_case_protocol|) pr)
    (setf (cl-kb:frame-own-slot-value ca '|negotiation|::|neg_case_conclusion|) cn)
    (setf (cl-kb:frame-own-slot-value ca '|negotiation|::|neg_case_process|) po)
    (setf (cl-kb:frame-own-slot-value co '|negotiation|::|neg_case_item|) it)
    (setf (cl-kb:frame-own-slot-value pr '|negotiation|::|neg_case_model|) (cl-kb:find-simple-instance (model-model-id model)))
    (dolist (attr (neginfo-attributes (model-neginfo model)))
      (attribute-value->slot-value model kb attr co))
    (dolist (p (neginfo-parties (model-neginfo model)))
      (let ((party-id (party-id p))
            (party nil))
        (setf party (cl-kb:mk-simple-instance (instance-party-id party-id model) 
                                              (cl-kb:find-cls "one_party")))
        (setf (cl-kb:frame-own-slot-value party '|onenegotiation|::|one_id|) party-id)
        (if (string-equal (party-role p) "negmod:Owner")
            (setf (cl-kb:frame-own-slot-value (cl-kb:find-simple-instance (instance-context-id model))
                                             '|negotiation|::|neg_case_owner|) party))))
    (dolist (attr (protoinfo-attributes (model-protoinfo model)))
      (attribute-value->slot-value model kb attr pr))
    (dolist (attr (item-attributes (infomodel-item (model-infomodel model))))
      (attribute-value->slot-value model kb attr it)))
  kb)

;
;
;

(defun attribute->slot-id (attr model)
  (let ((attrname (attribute-name attr)))
    (cond
     ((string-equal attrname "allowNested")
      "one_allow_nested")
     ((string-equal attrname "name")
      "one_name")
     ((string-equal attrname "id")
      "one_id")
     ((string-equal attrname "visibility")
      "one_visibility")
     ((string-equal attrname "parentId")
      "one_parent_id")
     ((string-equal attrname "creationDate")
      "one_creation_date")
     ((string-equal attrname "endDate")
      "neg_case_ending_date")
     ((string-equal attrname "startDate")
      "neg_case_starting_date")
     (t
      (format nil "~A @ ~A~A " (attribute-name attr) (model-name model) (model-version model))))))

(defun attribute->slot (model kb attr item)
  (let ((attrname (attribute->slot-id attr model)))
    (if (null (cl-kb:find-slot attrname nil))
        (progn
          (let ((at (cl-kb:mk-slot attrname :kb kb)))
            (cl-kb:cls-add-direct-template-slot item at)
            (setf (cl-kb:slot-maximum-cardinality at) 1)
            (let ((typ (attribute-onetype attr)))
              (if (not (onetype-globalp typ))
                  (let ((typename (onetype-name typ))
                        (kind (onetype-kind typ)))
                    (cond
                     ((eq kind 'enum)
                      (let ((v
                             (mapcan #'(lambda (x) (list x)) (onetype-vals typ))))
                        (setf (cl-kb:slot-allowed-values at) v)))
                     ((string= typename "negmod:OneDate")
                      (setf (cl-kb:slot-allowed-clses at) '(|dataset|::|time|)))
                     (t
                      (format t "Not implemented, yet. \"~A\" ~A~%" typename typ))))
                  (let ((typename (onetype-name typ))
                        (kind (onetype-kind typ)))
                    (cond 
                     ((string= typename "negmod:OneString")
                      (setf (cl-kb:frame-own-slot-value at '|protege|::|:SLOT-VALUE-TYPE|) "String"))
                     ((string= typename "negmod:OneInteger")
                      (setf (cl-kb:frame-own-slot-value at '|protege|::|:SLOT-VALUE-TYPE|) "Integer"))
                     ((string= typename "negmod:OneBoolean")
                      (setf (cl-kb:frame-own-slot-value at '|protege|::|:SLOT-VALUE-TYPE|) "Boolean"))
                     ((string= typename "negmod:OneDate")
                      (setf (cl-kb:frame-own-slot-values at '|protege|::|:SLOT-VALUE-TYPE|) (list "Instance" '|dataset|::|time|)))
                     ((string= typename "negmod:OneImage")
                      (setf (cl-kb:frame-own-slot-values at '|protege|::|:SLOT-VALUE-TYPE|) (list "Instance" '|onenegotiation|::|one_image|)))
                     ((string= typename "negmod:OneBinaryDocument")
                      (setf (cl-kb:frame-own-slot-values at '|protege|::|:SLOT-VALUE-TYPE|) (list "Instance" '|onenegotiation|::|one_binary_document|)))
                     ((string= typename "negmod:OneCurrency")
                      (setf (cl-kb:frame-own-slot-value at '|protege|::|:SLOT-VALUE-TYPE|) "String"))
                     ((string= typename "negmod:OneAmount")
                      (setf (cl-kb:frame-own-slot-value at '|protege|::|:SLOT-VALUE-TYPE|) "Integer"))
                     ((eq kind 'enum)
                      (let ((v
                             (mapcan #'(lambda (x) (list x)) (onetype-vals typ))))
                        (setf (cl-kb:slot-allowed-values at) v)))
                     (t
                      (format t "Not implemented, yet. \"~A\" ~A~%" typename typ))
                     ))))
            )))))

(defun attribute-value->slot-value (model kb attr item)
  ;(format t "##$$##$$##   ~A   ~A~%" (attribute-name attr) (attribute-value attr))
  (let ((attrname (attribute->slot-id attr model)))
    (let ((at (cl-kb:get-slot attrname :kb kb))
          (typ (attribute-onetype attr))
          (val (attribute-value attr))
          (converted-value nil))      
      (if (and val (not (string-equal val "")))
          (progn 
            (setf converted-value 
                  (let ((typename (onetype-name typ))
                        (kind (onetype-kind typ)))
                    (cond
                     ((eq kind 'enum)
                      (nth (position val (onetype-val-ids typ) :test #'string-equal) (onetype-vals typ)))
                     ((string= typename "negmod:OneDate")
                      (cl-ppcre:register-groups-bind (vy vm vd) 
                                                ("\\s*(\\w+)-(\\w+)-(\\w+)T" val) 
                                                (date-instance vd vm vy)))
                     ((string= typename "negmod:OneString")
                      val)
                     ((or (string= typename "negmod:OneInteger") (string= typename "negmod:OneAmount"))
                      (let ((v (parse-integer val)))
                        (if (> v 2000000000)
                            0
                            v)))
                     ((string= typename "negmod:OneBoolean")
                      (if (string= val "true") t nil))
                     ((string= typename "negmod:OneImage")
                      nil)
                     ((string= typename "negmod:OneBinaryDocument")
                      nil)
                     ((string= typename "negmod:OneCurrency")
                      val)
                     (t
                      (format t "Not implemented, yet. \"~A\" ~A~%" typename typ))
                     )))
            ;(format t "##$$##$$## ~A  ~%" converted-value)
            (setf (cl-kb:frame-own-slot-value item at) converted-value))))))
          ;(format t "##$$##$$## NIL~%")))))
      


(defun issue-> (model kb is issue-id proposal-id)
  (let ((issuename (format nil "~A @ ~A~A " (issue-name is) (model-name model) (model-version model) )))
    (let ((issue (cl-kb:mk-cls issuename :kb kb :supercls (cl-kb:get-cls issue-id :kb kb))))
      
      ;(format t "##@@ ~A ~%" issue)
      (dolist (attr (issue-attributes is))
        ;(format t "##@@ ~A ~%" attr)
        (attribute->slot model kb attr issue))
        ;(attribute-value->slot-value model kb attr issue))
      (let ((slotname (format nil "has ~A " issuename )))
        (let ((sl (cl-kb:mk-slot slotname :kb kb)))
          (cl-kb:cls-add-direct-template-slot (cl-kb:get-slot proposal-id :kb kb) sl )
          (setf (cl-kb:slot-maximum-cardinality sl) 1)
          (setf (cl-kb:slot-allowed-clses sl) (list issue)))))))

      
(defun date-instance (d m y &optional (h "0") (mm "0") (s "0"))
  (let ((date-id (format nil "date ~A ~A ~A ~A ~A ~A" d m y h mm s)))
    (multiple-value-bind (di new) (cl-kb:get-simple-instance date-id)
      (if new 
          (progn
            (cl-kb:instance-add-direct-type di '|dataset|::|time|)
            (setf (cl-kb:frame-own-slot-value di '|dataset|::|time_year|) (parse-integer y))
            (setf (cl-kb:frame-own-slot-value di '|dataset|::|time_month|) (parse-integer m))
            (setf (cl-kb:frame-own-slot-value di '|dataset|::|time_day|) (parse-integer d))
            (setf (cl-kb:frame-own-slot-value di '|dataset|::|time_hour|) (parse-integer h))
            (setf (cl-kb:frame-own-slot-value di '|dataset|::|time_minute|) (parse-integer mm))
            (setf (cl-kb:frame-own-slot-value di '|dataset|::|time_sec|) (parse-integer s))
            (setf (cl-kb:frame-own-slot-value di '|dataset|::|time_usec|) 0)))
      di)))


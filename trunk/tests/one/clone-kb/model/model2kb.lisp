
;;;; Created on 2008-09-29 18:10:28

(in-package :clone-kb)

(defun import-model (model)
  (let ((kb (find-model-kb model)))
    (if (not kb)
        (setf kb (make-model-kb model))
        (progn
          (cl-kb:kb-clear kb)
          (let ((this (cl-kb:mk-simple-instance (model-model-id model)
                                                '|onenegotiation|::|one_model| 
                                                :kb kb)))
            (setf (cl-kb:frame-own-slot-value this '|negotiation|::|neg_model_id|) (model-name model))
            (setf (cl-kb:frame-own-slot-value this '|negotiation|::|neg_model_version|) (model-version model)))))
    (model->kb-schema model kb)
    kb))

(defun import-model-instance (model)
  (if (not (find-model-kb model))
      (import-model model))
  (let ((kb (or (find-instance-model-kb model)
                (make-instance-model-kb model))))
    (model->kb-instance model kb)))

(defun model->kb-schema (model kb)
  (cl-kb:with-kb kb t
                 (cl-kb:mk-cls (model-case-id model) :supercls '|onenegotiation|::|one_case|)
                 (let ((con (cl-kb:mk-cls (model-context-id model) :supercls '|onenegotiation|::|one_context|)))
                   (dolist (attr (neginfo-attributes (model-neginfo model)))
                     (attribute->slot model kb attr con)))
                 (cl-kb:mk-cls (model-conclusion-id model) :supercls '|onenegotiation|::|one_conclusion|)
                 (let ((pro (cl-kb:mk-cls (model-protocol-id model) :supercls '|onenegotiation|::|one_protocol| )))
                   (dolist (attr (protoinfo-attributes (model-protoinfo model)))
                     (attribute->slot model kb attr pro)))
                 (cl-kb:mk-cls (model-process-id model) :supercls '|onenegotiation|::|one_process|)
                 (let ((it (cl-kb:mk-cls (model-item-id model) :supercls '|onenegotiation|::|one_item|)))
                   (dolist (attr (item-attributes (infomodel-item (model-infomodel model))))
                     (attribute->slot model kb attr it)))
                 (cl-kb:mk-cls (model-proposal-id model) :supercls '|onenegotiation|::|one_proposal|)
                 (cl-kb:mk-cls (model-issue-id model) :supercls '|onenegotiation|::|one_issue|)
                 (dolist (is (item-issues (infomodel-item (model-infomodel model))))
                   ;(format t "### ~A~%" is)
                   (issue-> model kb is (model-issue-id model) (model-proposal-id model))))
  kb)

(defun model->kb-instance (model kb)
  (cl-kb:with-kb kb t
                 (let ((ca (cl-kb:mk-simple-instance (instance-case-id model) (cl-kb:get-simple-instance (model-case-id model))))
                       (co (cl-kb:mk-simple-instance (instance-context-id model) (cl-kb:get-simple-instance (model-context-id model))))
                       (pr (cl-kb:mk-simple-instance (instance-protocol-id model) (cl-kb:get-simple-instance (model-protocol-id model))))
                       (cn (cl-kb:mk-simple-instance (instance-conclusion-id model) (cl-kb:get-simple-instance (model-conclusion-id model))))
                       (po (cl-kb:mk-simple-instance (instance-process-id model) (cl-kb:get-simple-instance (model-process-id model))))
                       (it (cl-kb:mk-simple-instance (instance-item-id model) (cl-kb:get-simple-instance (model-item-id model)))))
                   (setf (cl-kb:frame-own-slot-value ca '|negotiation|::|neg_case_context|) co)
                   (setf (cl-kb:frame-own-slot-value ca '|negotiation|::|neg_case_protocol|) pr)
                   (setf (cl-kb:frame-own-slot-value ca '|negotiation|::|neg_case_conclusion|) cn)
                   (setf (cl-kb:frame-own-slot-value ca '|negotiation|::|neg_case_process|) po)
                   (setf (cl-kb:frame-own-slot-value co '|negotiation|::|neg_case_item|) it)
                   (setf (cl-kb:frame-own-slot-value pr '|negotiation|::|neg_case_model|) (cl-kb:find-simple-instance (model-model-id model)))
                   (dolist (attr (neginfo-attributes (model-neginfo model)))
                     (attribute-value->slot-value model kb attr co))
                   (dolist (attr (protoinfo-attributes (model-protoinfo model)))
                     (attribute-value->slot-value model kb attr pr))
                   (dolist (attr (item-attributes (infomodel-item (model-infomodel model))))
                     (attribute-value->slot-value model kb attr it))))
  kb)

;
;
;

  
(defun model-model-id (model)
  (format nil "model @ ~A" (model-full-name model)))
        
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
  
(defun instance-item-id (model)
  (format nil "item @ ~A" (instance-full-name model)))

;
;
;

(defun attribute->slot (model kb attr item)
  (let ((attrname (format nil "~A @ ~A~A " (attribute-name attr) (model-name model) (model-version model) )))
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
                (setf (cl-kb:slot-allowed-clses at) '(|dataset|::|date|)))
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
                (setf (cl-kb:frame-own-slot-values at '|protege|::|:SLOT-VALUE-TYPE|) (list "Instance" '|dataset|::|date|)))
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
      )))

(defun attribute-value->slot-value (model kb attr item)
  ;(format t "##$$##$$##   ~A   ~A~%" (attribute-name attr) (attribute-value attr))
  (let ((attrname (format nil "~A @ ~A~A " (attribute-name attr) (model-name model) (model-version model) )))
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

      
(defun date-instance (d m y)
  (let ((date-id (format nil "date ~A ~A ~A" d m y)))
    (multiple-value-bind (di new) (cl-kb:get-simple-instance date-id)
      (if new 
          (progn
            (cl-kb:instance-add-direct-type di '|dataset|::|date|)
            (setf (cl-kb:frame-own-slot-value di '|dataset|::|time_year|) (parse-integer y))
            (setf (cl-kb:frame-own-slot-value di '|dataset|::|time_month|) (parse-integer m))
            (setf (cl-kb:frame-own-slot-value di '|dataset|::|time_day|) (parse-integer d))))
      di)))

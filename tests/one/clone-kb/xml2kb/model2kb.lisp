
;;;; Created on 2008-09-29 18:10:28

(in-package :clone-kb)

(defvar *model* nil)

;
; translare models and model instances into kbs
;

(defun model->kb (model &optional (overwritep nil))
  (let ((*model* model)
        (*model-id* (model-id model)))
    (get-model-kb *model-id* #'model->kb-schema overwritep)))

(defun model-instance->kb (model &optional (overwritep nil))
  (let ((*model* model)
        (*model-id* (model-id model))
        (*model-instance-id* (model-instance-name model)))
    (get-model-instance-kb *model-id* *model-instance-id* #'model->kb-instance overwritep)))


;
; get kbs
;

(defun get-model-kb (model-id import-fn &optional (overwrite nil))
  (let ((kb (find-model-kb model-id)))
    (if (not kb)
        (progn
          (setf kb (make-model-kb model-id))
          (cl-kb:with-kb kb t
                         (funcall import-fn)
                         ))               
        (if overwrite
              (cl-kb:with-kb kb t
                             (cl-kb:kb-clear kb)
                             (init-model-kb model-id kb)
                             (funcall import-fn)
                             )))
    kb))

(defun get-model-instance-kb (model-id model-instance-id import-fn &optional (overwrite nil))
  (let ((kb (find-model-instance-kb model-id model-instance-id)))
    (if (not kb)
        (progn
          (setf kb (make-model-instance-kb model-id model-instance-id))
          (cl-kb:with-kb kb t
                         (funcall import-fn)))
        (if overwrite
            (cl-kb:with-kb kb t
                           (cl-kb:kb-clear kb)
                           (init-model-instance-kb model-id model-instance-id kb)
                           (funcall import-fn))))
    kb))



(defun model->kb-schema ()
  (let ((mod-ins (cl-kb:find-simple-instance (g-model-id))))
    (setf (cl-kb:frame-own-slot-value mod-ins '|negotiation|::|neg_model_name|) (model-name *model*))
    (setf (cl-kb:frame-own-slot-value mod-ins '|negotiation|::|neg_model_version|) (model-version *model*)))
  (let ((procls (cl-kb:mk-cls (g-model-proposal-id) :supercls '|onenegotiation|::|one_proposal|))
        (itset (cl-kb:mk-cls (g-model-itemset-id) :supercls '|onenegotiation|::|one_itemset|)))
    (dolist (item (infomodel-items (model-infomodel *model*)))
      (let ((it (cl-kb:mk-cls (g-model-item-id (item-name item)) :supercls '|onenegotiation|::|one_item|))
            (its (cl-kb:mk-slot (g-model-item-slot-id (item-name item))))
            (m_it (cl-kb:mk-simple-instance (g-model-m-item-id (item-name item)) '|negotiation|::|m_item|)))
        (setf (cl-kb:frame-own-slot-value m_it '|negotiation|::|m_has_name|) (item-name item))
        (setf (cl-kb:frame-own-slot-value m_it '|negotiation|::|m_cls|) it)
        (setf (cl-kb:frame-own-slot-value m_it '|negotiation|::|m_slot|) its)
        (cl-kb:cls-add-direct-template-slot itset its)
        (setf (cl-kb:slot-maximum-cardinality its) 1)
        (setf (cl-kb:slot-allowed-clses its) (list it))
        (dolist (attr (item-attributes item))
          (let ((m_attr (cl-kb:mk-simple-instance (g-model-m-item-attr-id (item-name item) (attribute-name attr)) '|negotiation|::|m_attr|)))
            (setf (cl-kb:frame-own-slot-value m_attr '|negotiation|::|m_has_name|) (attribute-name attr))
            (push m_attr (cl-kb:frame-own-slot-values m_it '|negotiation|::|m_has_attr|))
            (let ((sat (attribute->slot attr it)))
                    (if sat 
                        (setf (cl-kb:frame-own-slot-value m_attr '|negotiation|::|m_slot|) sat)))))
        (let ((isset (cl-kb:mk-cls (g-model-issueset-id (item-name item)) :supercls '|onenegotiation|::|one_issueset|))
              (props (cl-kb:mk-slot (g-model-issueset-slot-id (item-name item)))))
          (setf (cl-kb:frame-own-slot-value m_it '|negotiation|::|m_issueset_slot|) props)
          (cl-kb:cls-add-direct-template-slot procls props)
          (setf (cl-kb:slot-maximum-cardinality props) 1)
          (setf (cl-kb:slot-allowed-clses props) (list isset))
          (dolist (is (item-issues item))
            (let ((iscls (cl-kb:mk-cls (g-model-issue-id (item-name item) (issue-name is)) :supercls '|onenegotiation|::|one_issue|))
                  (sis (cl-kb:mk-slot (g-model-issue-slot-id (item-name item) (issue-name is))))
                  (m_is (cl-kb:mk-simple-instance (g-model-m-issue-id (item-name item) (issue-name is)) '|negotiation|::|m_issue|)))
              (setf (cl-kb:frame-own-slot-value m_is '|negotiation|::|m_has_name|) (issue-name is))
              (setf (cl-kb:frame-own-slot-value m_it '|negotiation|::|m_has_issue|) m_is)
              (setf (cl-kb:frame-own-slot-value m_is '|negotiation|::|m_cls|) iscls)
              (setf (cl-kb:frame-own-slot-value m_is '|negotiation|::|m_slot|) sis)
              (cl-kb:cls-add-direct-template-slot isset sis)
              (setf (cl-kb:slot-maximum-cardinality sis) 1)
              (setf (cl-kb:slot-allowed-clses sis) (list iscls))
              (dolist (attr (issue-attributes is))
                (let ((m_attr (cl-kb:mk-simple-instance 
                               (g-model-m-issue-attr-id (item-name item) (issue-name is) (attribute-name attr)) 
                               '|negotiation|::|m_attr|)))
                  (setf (cl-kb:frame-own-slot-value m_attr '|negotiation|::|m_has_name|) (attribute-name attr))
                  
                  (push m_attr (cl-kb:frame-own-slot-values m_is '|negotiation|::|m_has_attr|))
                  (let ((sat (attribute->slot attr iscls)))
                    (if sat 
                        (setf (cl-kb:frame-own-slot-value m_attr '|negotiation|::|m_slot|) sat))))))))))))


  
(defun model->kb-instance ()
  ;(model->kb-schema)
  (let ((ca (cl-kb:mk-simple-instance (g-instance-case-id) (cl-kb:get-cls (g-model-case-id))))
        (co (cl-kb:mk-simple-instance (g-instance-context-id) (cl-kb:get-cls (g-model-context-id))))
        (pr (cl-kb:mk-simple-instance (g-instance-protocol-id) (cl-kb:get-cls (g-model-protocol-id))))
        (cn (cl-kb:mk-simple-instance (g-instance-conclusion-id) (cl-kb:get-cls (g-model-conclusion-id))))
        (po (cl-kb:mk-simple-instance (g-instance-process-id) (cl-kb:get-cls (g-model-process-id))))
        (its (cl-kb:mk-simple-instance (g-instance-itemset-id) (cl-kb:get-cls (g-model-itemset-id)))))
    (setf (cl-kb:frame-own-slot-value ca '|negotiation|::|neg_case_context|) co)
    (setf (cl-kb:frame-own-slot-value ca '|negotiation|::|neg_case_protocol|) pr)
    (setf (cl-kb:frame-own-slot-value ca '|negotiation|::|neg_case_conclusion|) cn)
    (setf (cl-kb:frame-own-slot-value ca '|negotiation|::|neg_case_process|) po)
    (setf (cl-kb:frame-own-slot-value pr '|negotiation|::|neg_case_model|) 
          (cl-kb:find-simple-instance (g-model-id)))
    (setf (cl-kb:frame-own-slot-value co '|negotiation|::|neg_case_itemset|) its)
    (dolist (item (infomodel-items (model-infomodel *model*)))
      (let ((it (cl-kb:mk-simple-instance (g-instance-item-id (item-name item)) (cl-kb:get-cls (g-model-item-id (item-name item))))))
        (setf (cl-kb:frame-own-slot-value its (cl-kb:get-slot (g-model-item-slot-id (item-name item)))) it)
        (dolist (attr (item-attributes item))
          (attribute-value->slot-value attr (cl-kb:get-cls (g-model-item-id (item-name item))) it))))
    (dolist (attr (neginfo-attributes (model-neginfo *model*)))
      (attribute-value->slot-value attr (cl-kb:get-cls (g-model-context-id)) co))
    (dolist (p (neginfo-parties (model-neginfo *model*)))
      (let ((party-id (party-id p))
            (party nil))
        (setf party (cl-kb:mk-simple-instance (g-instance-party-id party-id) 
                                              (cl-kb:find-cls "one_party")))
        (setf (cl-kb:frame-own-slot-value party '|onenegotiation|::|one_id|) party-id)
        (if (string-equal (party-role p) "negmod:Owner")
            (setf (cl-kb:frame-own-slot-value (cl-kb:find-simple-instance (g-instance-context-id))
                                              '|negotiation|::|neg_case_owner|) party))))
    (dolist (attr (protoinfo-attributes (model-protoinfo *model*)))
      (attribute-value->slot-value attr (cl-kb:get-cls (g-model-protocol-id)) pr))))
    

(defun attribute->slot (attr item)
  (let ((attrname (g-model-cls-slot-id (cl-kb:frame-name item) (attribute-name attr))))
    (if (null (cl-kb:find-slot attrname nil))
        (progn
          (let ((at (cl-kb:mk-slot attrname)))
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
                     ((or
                       (string= typename "negmod:OneString")
                       (string= typename "negmod:OneCurrency")
                       (string= typename "negmod:OneByte")
                       (string= typename "negmod:OneChar"))
                      (setf (cl-kb:frame-own-slot-value at '|protege|::|:SLOT-VALUE-TYPE|) "String"))
                     ((or
                       (string= typename "negmod:OneShort")
                       (string= typename "negmod:OneInteger"))
                      (setf (cl-kb:frame-own-slot-value at '|protege|::|:SLOT-VALUE-TYPE|) "Integer"))
                     ((string= typename "negmod:OneBoolean")
                      (setf (cl-kb:frame-own-slot-value at '|protege|::|:SLOT-VALUE-TYPE|) "Boolean"))
                     ((string= typename "negmod:OneDate")
                      (setf (cl-kb:frame-own-slot-values at '|protege|::|:SLOT-VALUE-TYPE|) (list "Instance" '|dataset|::|time|)))
                     ((string= typename "negmod:OneImage")
                      (setf (cl-kb:frame-own-slot-values at '|protege|::|:SLOT-VALUE-TYPE|) (list "Instance" '|onenegotiation|::|one_image|)))
                     ((string= typename "negmod:OneBinaryDocument")
                      (setf (cl-kb:frame-own-slot-values at '|protege|::|:SLOT-VALUE-TYPE|) (list "Instance" '|onenegotiation|::|one_binary_document|)))
                     ((string= typename "negmod:OneXmlDocument")
                      (setf (cl-kb:frame-own-slot-values at '|protege|::|:SLOT-VALUE-TYPE|) (list "Instance" '|onenegotiation|::|one_xml_document|)))
                     ((or 
                       (string= typename "negmod:OneLong")
                       (string= typename "negmod:OneAmount")
                       (string= typename "negmod:OneDouble")
                       (string= typename "negmod:OneFloat"))
                      (setf (cl-kb:frame-own-slot-value at '|protege|::|:SLOT-VALUE-TYPE|) "Float"))
                     ((eq kind 'enum)
                      (let ((v
                             (mapcan #'(lambda (x) (list x)) (onetype-vals typ))))
                        (setf (cl-kb:slot-allowed-values at) v)))
                     (t
                      (format t "Typename conversion not implemented, yet. \"~A\" ~A~%" typename typ))
                     )))
              at))))))

(defun %attribute-value->slot-value (val typename kind typ)
  (cond
    ((eq kind 'enum)
     (nth (position val (onetype-val-ids typ) :test #'string-equal) (onetype-vals typ)))
    ((or
      (string= typename "negmod:OneDate")
      (string= typename "Date"))
     (cl-ppcre:register-groups-bind (vy vm vd) 
                                    ("\\s*(\\w+)-(\\w+)-(\\w+)T" val) 
                                    (date-instance vd vm vy)))
    ((or
      (string= typename "negmod:OneString")
      (string= typename "negmod:OneCurrency")
      (string= typename "negmod:OneByte")
      (string= typename "negmod:OneChar")
      (string= typename "String")
      (string= typename "Currency")
      (string= typename "Byte")
      (string= typename "Char"))
     val)
    ((or 
      (string= typename "negmod:OneInteger")
      (string= typename "negmod:OneShort")
      (string= typename "Integer")
      (string= typename "Short"))
     (handler-case
      (let ((v (parse-integer val :junk-allowed t)))
        (if (> v 2000000000)
            0
            v))
      (error () 0)))
    ((or 
      (string= typename "negmod:OneLong")
      (string= typename "negmod:OneAmount")
      (string= typename "negmod:OneDouble")
      (string= typename "negmod:OneFloat")
      (string= typename "Long")
      (string= typename "Amount")
      (string= typename "Double")
      (string= typename "Float"))
     (handler-case
      (let ((v (with-input-from-string (strm val)
                                       (float (read strm)))))
        v)
      (error () 0)))
    ((or
      (string= typename "negmod:OneBoolean")
      (string= typename "Boolean"))
     (if (string= val "true") t nil))
    ((or 
      (string= typename "negmod:OneImage")
      (string= typename "negmod:OneBinaryDocument")
      (string= typename "negmod:OneXmlDocument")
      (string= typename "Image")
      (string= typename "BinaryDocument")
      (string= typename "XmlDocument"))
     nil) 
    (t
     (format t "Not implemented, yet. \"~A\" ~A~%" typename typ))))
        

(defun attribute-value->slot-value (attr cls item)
  (let ((attrname (g-model-cls-slot-id (cl-kb:frame-name cls) (attribute-name attr))))
    (let ((at (cl-kb:get-slot attrname))
          (typ (attribute-onetype attr))
          (val (attribute-value attr))
          (converted-value nil))
      (if (and val (not (string-equal val "")))
          (progn
            (setf converted-value (%attribute-value->slot-value val (onetype-name typ) (onetype-kind typ) typ))
            (setf (cl-kb:frame-own-slot-value item at) converted-value))))))
      
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

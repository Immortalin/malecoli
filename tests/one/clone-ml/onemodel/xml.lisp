;;;; Created on 2008-08-25 11:03:10

;;;; Created on 2008-08-18 11:41:45

(in-package :clone-ml)

;
; protege model import 
;

(defun onemodel-import (pathname)
  (let ((seed (make-seed))
        (model nil))
    (setf model (seed-model seed))
    (setf (seed-infomodel seed) (model-infomodel model))
    (setf (seed-item seed) (infomodel-item (model-infomodel model)))
    (setf (seed-neginfo seed) (model-neginfo model))
    (with-open-file (strm pathname :direction :input)
                    (s-xml:start-parse-xml strm
                                           (make-instance 's-xml:xml-parser-state
                                                          :seed seed
                                                          :new-element-hook #'model-import-new-element-hook
                                                          :finish-element-hook #'model-import-finish-element-hook
                                                          :text-hook #'model-import-text-hook)))
    (let ((kb (find-model-kb (model-name model) (model-version model))))
      (if (null kb)
          (setf kb (make-model-kb (model-name model) (model-version model)))
          (mlcl-kb:kb-clear kb))
      (format t "--> save ~%")
      (convert-one-model model kb)
      (format t "--> save ~%")
      (kb-save kb)
      kb)))

;
; namespaces
;

(in-package :mlcl-kb)

(defpackage :onemodel-ns
  (:export
    |primitiveType|
    |NegotiationModel|
   ))

(defpackage :xmi-ns
  (:export
    |id|
    |type|
    |name|
   ))

(s-xml:register-namespace "http://www.omg.org/XMI" "xmi" :xmi-ns)
(s-xml:register-namespace "http://NegotiationMetaModel_v1.3.1.ecore" "negmod" :onemodel-ns)

(in-package :clone-ml)

; seed

(defstruct seed
  (model (make-model))
  (infomodel nil)
  (neginfo nil)
  (item nil)
  (text nil)
  (literals nil)
  (attributes)
  (inim nil))

; hooks

(defun model-import-add-type (kind attributes seed) 
  (let ((type (model-get-type (seed-model seed) 
                              (cdr (assoc 'xmi-ns:|id| attributes))
                              (cdr (assoc 'xmi-ns:|type| attributes)))))
    (setf (onetype-kind type) kind)
    (setf (onetype-name type) (cdr (assoc 'xmi-ns:|type| attributes)))
    (setf (onetype-globalp type) (not (seed-inim seed)))
    type))
  
(defun model-import-new-element-hook (name attributes seed)
  (let ((new-seed (make-seed :model (seed-model seed)
                             :infomodel (seed-infomodel seed)
                             :neginfo (seed-neginfo seed)
                             :item (seed-item seed)
                             :inim (seed-inim seed))))
    (cond 
     ((eq name 'onemodel-ns:|NegotiationModel|)
      (setf (model-name (seed-model seed)) (cdr (assoc ':|name| attributes)))
      (setf (model-version (seed-model seed)) (cdr (assoc ':|version| attributes))))
     ((eq name ':|informationModel|)
      (setf (infomodel-name (seed-infomodel seed)) (cdr (assoc ':|name| attributes)))
      (setf (seed-inim seed) t)
      (setf (seed-inim new-seed) t)))
     new-seed))

(defun model-import-finish-element-hook (name attributes parent-seed seed)
  (cond 
   ((eq name ':|primitiveType|)
    (model-import-add-type 'primitive attributes seed))
   ((eq name ':|complexType|)
    (model-import-add-type 'complex attributes seed))
   ((eq name ':|enumeration|)
    (let ((type (model-import-add-type 'enum attributes seed)))
      (setf (onetype-vals type) (seed-literals seed))))
   ((eq name ':|literal|)
    (let ((lit (cdr (assoc ':|name| attributes))))
      (push lit (seed-literals parent-seed))))
   ((eq name ':|attribute|)
    (let ((attr (make-attribute)))
      (setf (attribute-name attr) (cdr (assoc ':|name| attributes)))
      (setf (attribute-onetype attr) (model-get-type (seed-model seed) 
                                                     (cdr (assoc ':|type| attributes))
                                                     nil))
      (push attr (seed-attributes parent-seed))))
   ((eq name ':|issue|)
    (let ((issue (make-issue)))
      (setf (issue-name issue) (cdr (assoc ':|name| attributes)))
      (setf (issue-attributes issue) (seed-attributes seed))
      (push issue (item-issues (seed-item parent-seed)))))
   ((eq name ':|item|)
    (progn
      (setf (item-name (seed-item seed)) (cdr (assoc ':|name| attributes)))
      (setf (item-attributes (seed-item seed)) (seed-attributes seed))))
   ((eq name ':|informationModel|)
    (setf (seed-inim parent-seed) nil))
   ((eq name ':|negotiation|)
    (setf (neginfo-attributes (model-neginfo (seed-model seed))) (seed-attributes seed)))
   ((eq name ':|negotiationProtocol|)
    (push (make-attribute :name "startDate"
                          :onetype (model-get-type (seed-model seed) nil "negmod:OneDate"))
          (seed-attributes seed))
    (push (make-attribute :name "endDate"
                          :onetype (model-get-type (seed-model seed) nil "negmod:OneDate"))
          (seed-attributes seed))
    (setf (protoinfo-attributes (model-protoinfo (seed-model seed))) (seed-attributes seed))
    ))
  parent-seed)

(defun model-import-text-hook (string seed)
  (setf (seed-text seed) string)
  seed)

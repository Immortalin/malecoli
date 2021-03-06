
;;;; Created on 2008-08-18 11:41:45

(in-package :clone-kb)

;
; model import 
;

(defun xml-model-import (pathname)
  (let ((seed (make-model-seed))
        (model nil))
    (setf model (model-seed-model seed))
    (setf (model-seed-infomodel seed) (model-infomodel model))
    ;(setf (model-seed-items seed) (infomodel-item (model-infomodel model)))
    (setf (model-seed-neginfo seed) (model-neginfo model))
    (with-open-file (strm pathname :direction :input)
                    (s-xml:start-parse-xml strm
                                           (make-instance 's-xml:xml-parser-state
                                                          :seed seed
                                                          :new-element-hook #'model-import-new-element-hook
                                                          :finish-element-hook #'model-import-finish-element-hook
                                                          :text-hook #'model-import-text-hook)))
    model))


;
; namespaces
;

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
(s-xml:register-namespace "http://NegotiationMetaModel_v1.3.3.ecore" "negmod" :onemodel-ns)

;
; model seed
;

(defstruct model-seed
  (model (make-model))
  (infomodel nil)
  (neginfo nil)
  (item nil)
  (text nil)
  (literals nil)
  (attributes)
  (inim nil)
  (parties)
  (roles))


;
; xml hooks
;

(defun model-import-new-element-hook (name attributes seed)
  (let ((new-seed (make-model-seed :model (model-seed-model seed)
                             :infomodel (model-seed-infomodel seed)
                             :neginfo (model-seed-neginfo seed)
                             :item (model-seed-item seed)
                             :inim (model-seed-inim seed))))
    (cond 
     ((eq name 'onemodel-ns:|NegotiationModel|)
      (let ((model (model-seed-model seed)))
        (setf (model-name model) (cdr (assoc ':|name| attributes)))
        (setf (model-version model) (cdr (assoc ':|version| attributes)))
        (setf (model-id model) (cdr (assoc ':|id| attributes)))))
     ((eq name ':|informationModel|)
      (let ((infomodel (model-seed-infomodel seed)))
        (setf (infomodel-name infomodel) (cdr (assoc ':|name| attributes)))
        (setf (infomodel-version infomodel) (cdr (assoc ':|version| attributes)))
        (setf (infomodel-id infomodel) (cdr (assoc ':|id| attributes))))
      (setf (model-seed-inim seed) t)
      (setf (model-seed-inim new-seed) t))
     ((eq name ':|item|)
      (setf (model-seed-item new-seed) (make-item))))
     new-seed))

(defun model-import-finish-element-hook (name attributes parent-seed seed)
  (cond 
   ; negotiation
   ((eq name ':|negotiation|)
    (setf (neginfo-attributes (model-neginfo (model-seed-model seed))) (model-seed-attributes seed))
    (dolist (p (model-seed-parties seed))
      (push (make-party :id (car p) :role (cdr (find-if #'(lambda (x) (string-equal (car x) (cdr p))) (model-seed-roles seed))))
            (neginfo-parties (model-neginfo (model-seed-model seed))))))
   ; information model
   ((eq name ':|informationModel|)
    (setf (model-seed-inim parent-seed) nil))
   ; negotiation protocol
   ((eq name ':|negotiationProtocol|)
    (push (make-attribute :name "startDate"
                          :onetype (model-get-type (model-seed-model seed) nil "negmod:OneDate")
                          :value (cdr (assoc ':|startDate| attributes)))
          (model-seed-attributes seed))
    (push (make-attribute :name "endDate"
                          :onetype (model-get-type (model-seed-model seed) nil "negmod:OneDate")
                          :value (cdr (assoc ':|endDate| attributes)))
          (model-seed-attributes seed))
    (setf (protoinfo-attributes (model-protoinfo (model-seed-model seed))) 
          (model-seed-attributes seed))
    (setf (model-seed-roles parent-seed) (model-seed-roles seed)))
   ; party
   ((eq name ':|party|)
    (push (cons 
           (cdr (assoc ':|id| attributes))
           (cdr (assoc ':|negotiationRole| attributes)))
          (model-seed-parties parent-seed)))
   ((eq name ':|negotiationRole|)
    (push (cons 
           (cdr (assoc 'xmi-ns:|id| attributes))
           (cdr (assoc 'xmi-ns:|type| attributes)))
          (model-seed-roles parent-seed)))
   ; items
   ((eq name ':|item|)
    (progn
      (setf (item-name (model-seed-item seed)) (cdr (assoc ':|name| attributes)))
      (setf (item-attributes (model-seed-item seed)) (model-seed-attributes seed))
      (push (model-seed-item seed) (infomodel-items (model-seed-infomodel seed)))))
   ; issues
   ((eq name ':|issue|)
    (let ((issue (make-issue)))
      (setf (issue-name issue) (cdr (assoc ':|name| attributes)))
      (setf (issue-attributes issue) (model-seed-attributes seed))
      (push issue (item-issues (model-seed-item parent-seed)))))
   ; attributes
   ((eq name ':|attribute|)
    (let ((attr (make-attribute)))
      (setf (attribute-name attr) (cdr (assoc ':|name| attributes)))
      (setf (attribute-onetype attr) (model-get-type (model-seed-model seed) 
                                                     (cdr (assoc ':|type| attributes))
                                                     nil))
      (setf (attribute-value attr) (cdr (assoc ':|value| attributes)))
      (push attr (model-seed-attributes parent-seed))))
   ; types
   ((eq name ':|primitiveType|)
    (model-import-add-type 'primitive attributes seed))
   ((eq name ':|complexType|)
    (model-import-add-type 'complex attributes seed))
   ((eq name ':|enumeration|)
    (let ((type (model-import-add-type 'enum attributes seed)))
      (setf (onetype-vals type) (mapcar #'car (model-seed-literals seed)))
      (setf (onetype-val-ids type) (mapcar #'cdr (model-seed-literals seed)))))
   ((eq name ':|literal|)
    (let ((lit (cdr (assoc ':|name| attributes)))
          (id (cdr (assoc 'xmi-ns::|id| attributes))))
      (push (cons lit id) (model-seed-literals parent-seed)))))
  parent-seed)

(defun model-import-text-hook (string seed)
  (setf (model-seed-text seed) string)
  seed)


;
; add types
;

(defun model-import-add-type (kind attributes seed) 
  (let ((type (model-get-type (model-seed-model seed) 
                              (cdr (assoc 'xmi-ns:|id| attributes))
                              (cdr (assoc 'xmi-ns:|type| attributes)))))
    (setf (onetype-kind type) kind)
    (setf (onetype-name type) (cdr (assoc 'xmi-ns:|type| attributes)))
    (setf (onetype-globalp type) (not (model-seed-inim seed)))
    type))

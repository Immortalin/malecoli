;;;; Created on 2008-04-15 13:01:59

(in-package :clone-ml)

(defstruct onetype 
  (name)
  (id)
  (kind)
  (globalp nil)
  (vals nil))

(defstruct attribute
  (name)
  (onetype)
  (value nil))

(defstruct issue
  (name)
  (attributes nil))

(defstruct item
  (name)
  (attributes nil)
  (issues nil))

(defstruct neginfo 
  (attributes))

(defstruct protoinfo 
  (attributes))

(defstruct infomodel
  (name)
  (item (make-item)))
  
(defstruct model
  (name)
  (version)
  (id)
  (neginfo (make-neginfo))
  (protoinfo (make-protoinfo))
  (infomodel (make-infomodel))
  (types nil))

(defstruct instance-model 
  (model-kb nil)
  (vals nil))


;
; functions
;

(defun model-get-type (model typeid name)
  (let ((typ (or
              (and typeid (model-get-type-by-id model typeid))
              (and name (model-get-type-by-name model name)))))
    (if (null typ)
        (progn 
          (setf typ (make-onetype :id typeid :name name))
          (push typ (model-types model))))
    typ))

(defun model-get-value (model value type)
  (declare (ignore model))
  (format t "~A ~A ~%" value type)
  value)

(defun model-get-type-by-id (model typeid)
  (let ((typ (find-if #'(lambda (x) (string= (onetype-id x) typeid)) (model-types model))))
    typ))

(defun model-get-type-by-name (model name)
  (let ((typ (find-if #'(lambda (x) (string= (onetype-name x) name)) (model-types model))))
    typ))

;;;; Created on 2008-04-15 13:01:59

(in-package :clone-ml)

(defstruct onetype 
  (name nil)
  (id nil)
  (vals nil)
  (kind))


(defstruct item-attribute
  (name)
  (onetype))

(defstruct item-issue
  (name)
  (attributes))


(defstruct item
  (name nil)
  (attributes nil)
  (issues nil))

(defstruct im
  (name nil)
  (item)
  (types))

(defstruct model
  (name)
  (version)
  (im)
  (types))


(defun model-get-type (model typeid)
  (let ((el (find-if #'(lambda (x) (string= (onetype-id x) typeid)) (im-types (model-im model)))))
    (if el
        (values el t)
        (values (find-if #'(lambda (x) (string= (onetype-id x) typeid)) (model-types model)) nil))))

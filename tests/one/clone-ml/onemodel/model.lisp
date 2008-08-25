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
  (onetype))

(defstruct issue
  (name)
  (attributes nil))

(defstruct item
  (name)
  (attributes nil)
  (issues nil))

(defstruct infomodel
  (name)
  (item (make-item)))

(defstruct model
  (name)
  (version)
  (infomodel (make-infomodel))
  (types nil))

(defstruct instance-model 
  (model-kb nil)
  (vals nil))


;
; functions
;

(defun model-get-type (model typeid)
  (let ((typ (find-if #'(lambda (x) (string= (onetype-id x) typeid)) (model-types model))))
    (if (null typ)
        (progn 
          (setf typ (make-onetype :id typeid))
          (push typ (model-types model))))
    typ))
  
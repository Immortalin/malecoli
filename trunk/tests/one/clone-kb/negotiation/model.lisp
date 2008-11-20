
;;;; Created on 2008-04-15 13:01:59

(in-package :clone-kb)

; model
(defstruct model
  (name)
  (version)
  (id)
  (neginfo (make-neginfo))
  (protoinfo (make-protoinfo))
  (infomodel (make-infomodel))
  (types nil))

; info
(defstruct neginfo 
  (parties)
  (attributes))

(defstruct protoinfo 
  (attributes))

; information model
(defstruct infomodel
  (name)
  (version)
  (id)
  (item (make-item)))

; party
(defstruct party 
  (id)
  (role))

; item 
(defstruct item
  (id)
  (name)
  (attributes nil)
  (issues nil))


; issue
(defstruct issue
  (id)
  (name)
  (attributes nil))

; attribute 
(defstruct attribute
  (name)
  (onetype)
  (value nil))

; type
(defstruct onetype 
  (name)
  (id)
  (kind)
  (globalp nil)
  (vals nil)
  (val-ids nil))

(defun model-get-type (model typeid name)
  (let ((typ (or
              (and typeid (model-get-type-by-id model typeid))
              (and name (model-get-type-by-name model name)))))
    (if (null typ)
        (progn 
          (setf typ (make-onetype :id typeid :name name))
          (push typ (model-types model))))
    typ))


(defun model-get-type-by-id (model typeid)
  (let ((typ (find-if #'(lambda (x) (string= (onetype-id x) typeid)) (model-types model))))
    typ))

(defun model-get-type-by-name (model name)
  (let ((typ (find-if #'(lambda (x) (string= (onetype-name x) name)) (model-types model))))
    typ))
  
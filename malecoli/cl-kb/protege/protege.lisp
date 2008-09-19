;;;
;;; MaLeCoLi
;;; Copyright (C) 2008 Alessandro Serra
;;; 
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;

;;;; Created on 2008-04-22 16:34:12

(in-package :cl-kb)

(cl:eval-when (:compile-toplevel :load-toplevel)
              (cl:when (cl:not (cl:boundp 'concrete-value))
                       (cl:defconstant concrete-value "Concrete")
                       (cl:defconstant abstract-value "Abstract")
                       (cl:defconstant any-type-value "Any")
                       (cl:defconstant boolean-type-value "Boolean")
                       (cl:defconstant float-type-value "Float")                       
                       (cl:defconstant integer-type-value "Integer")
                       (cl:defconstant string-type-value "String")
                       (cl:defconstant symbol-type-value "Symbol")
                       (cl:defconstant instance-type-value "Instance")
                       (cl:defconstant cls-type-value "Class")))

                       
;
; conversion functions
;
  
(defun string->type-value (str)
  (cond
   ((string-equal str "Any")
    'any-type-value)
   ((string-equal str "Boolean")
    'boolean-type-value)
   ((string-equal str "Float")
    'float-type-value)
   ((string-equal str "Integer")
    'integer-type-value)
   ((string-equal str "String")
    'string-type-value)
   ((string-equal str "Symbol")
    'symbol-type-value)
   ((string-equal str "Instance")
    'instance-type-value)
   ((string-equal str "Class")
    'cls-type-value)))

(defun string->role (str)
  (cond
   ((string-equal str "Abstract")
    'abstract-value)
   ((string-equal str "Concrete")
    'concrete-value)))


;
; cls
;
  
(defun cls-documentation (cls)
  (frame-own-slot-value cls '|protege|::|:DOCUMENTATION|))

(defun (setf cls-documentation) (v cls)
  (setf (frame-own-slot-value cls '|protege|::|:DOCUMENTATION|) v))

(defun cls-role (cls)
  (string->role (frame-own-slot-value cls '|protege|::|:ROLE|)))

(defun (setf cls-role) (v cls)
  (setf (frame-own-slot-value cls '|protege|::|:ROLE|) v))

(defun cls-abstractp (cls)
  (eq (cls-role cls) 'abstract-value))

(defun (setf cls-abstractp) (v cls)
  (setf (cls-role cls) (if v abstract-value concrete-value)))

(defun cls-concretep (cls)
  (eq (cls-role cls) 'concrete-value))

(defun (setf cls-concretep) (v cls)
  (setf (cls-role cls) (if v concrete-value abstract-value)))

(defun cls-constraints (cls)
  (frame-own-slot-values cls '|protege|::|:SLOT-CONSTRAINTS|))

(defun (setf cls-constraints) (vs cls)
  (setf (frame-own-slot-values cls '|protege|::|:SLOT-CONSTRAINTS|) vs))


;
; slot
;

(defun slot-documentation (slot)
  (frame-own-slot-value slot '|protege|::|:DOCUMENTATION|))

(defun (setf slot-documentation) (v slot)
  (setf (frame-own-slot-value slot '|protege|::|:DOCUMENTATION|) v))

(defun slot-value-type (slot)
  (string->type-value (frame-own-slot-value slot '|protege|::|:SLOT-VALUE-TYPE|)))

(defun (setf slot-value-type) (val slot)
  (if (listp val)
      (setf (frame-own-slot-values slot '|protege|::|:SLOT-VALUE-TYPE|) val)
      (setf (frame-own-slot-value slot '|protege|::|:SLOT-VALUE-TYPE|) val)))

(defun slot-minimum-cardinality (slot)
  (frame-own-slot-value slot '|protege|::|:SLOT-MINIMUM-CARDINALITY|))

(defun (setf slot-minimum-cardinality) (v slot)
  (setf (frame-own-slot-value slot '|protege|::|:SLOT-MINIMUM-CARDINALITY|) v))

(defun slot-maximum-cardinality (slot)
  (frame-own-slot-value slot '|protege|::|:SLOT-MAXIMUM-CARDINALITY|))

(defun (setf slot-maximum-cardinality) (v slot)
  (setf (frame-own-slot-value slot '|protege|::|:SLOT-MAXIMUM-CARDINALITY|) v))

(defun slot-minimum-value (slot)
  (frame-own-slot-value slot '|protege|::|:SLOT-NUMERIC-MINIMUM|))

(defun (setf slot-minimum-value) (v slot)
  (setf (frame-own-slot-value slot '|protege|::|:SLOT-NUMERIC-MINIMUM|) v))

(defun slot-maximum-value (slot)
  (frame-own-slot-value slot '|protege|::|:SLOT-NUMERIC-MAXIMUM|))

(defun (setf slot-maximum-value) (v slot)
  (setf (frame-own-slot-value slot '|protege|::|:SLOT-NUMERIC-MAXIMUM|) v))

(defun slot-defaults (slot)
  (frame-own-slot-values slot '|protege|::|:SLOT-DEFAULTS|))

(defun (setf slot-defaults) (vs slot)
  (setf (frame-own-slot-values slot '|protege|::|:SLOT-DEFAULTS|) vs))

(defun slot-values (slot)
  (frame-own-slot-values slot '|protege|::|:SLOT-VALUES|))

(defun (setf slot-values) (vs slot)
  (setf (frame-own-slot-values slot '|protege|::|:SLOT-VALUES|) vs))

(defun slot-inverse-slot (slot)
  (frame-own-slot-value slot '|protege|::|:SLOT-INVERSE|))

(defun (setf slot-inverse-slot) (v slot)
  (setf (frame-own-slot-value slot '|protege|::|:SLOT-INVERSE|) v))

(defun slot-allowed-clses (slot)
  (and (eq (slot-value-type slot) 'instance-type-value)
       (cdr (frame-own-slot-values slot '|protege|::|:SLOT-VALUE-TYPE|))))

(defun (setf slot-allowed-clses) (vs slot)
  (setf (frame-own-slot-values slot '|protege|::|:SLOT-VALUE-TYPE|)
        (cons instance-type-value vs)))

(defun slot-allowed-parents (slot)
  (and (eq (slot-value-type slot) 'cls-type-value)
       (cdr (frame-own-slot-values slot '|protege|::|:SLOT-VALUE-TYPE|))))

(defun (setf slot-allowed-parents) (vs slot)
  (setf (frame-own-slot-values slot '|protege|::|:SLOT-VALUE-TYPE|)
        (cons cls-type-value vs)))

(defun slot-allowed-values (slot)
  (and (eq (slot-value-type slot) 'symbol-type-value)
       (cdr (frame-own-slot-values slot '|protege|::|:SLOT-VALUE-TYPE|))))

(defun (setf slot-allowed-values) (vs slot)
  (setf (frame-own-slot-values slot '|protege|::|:SLOT-VALUE-TYPE|)
        (cons symbol-type-value vs)))

(defun slot-constraints (cls)
  (frame-own-slot-values cls '|protege|::|:SLOT-CONSTRAINTS|))

(defun (setf slot-constraints) (vs cls)
  (setf (frame-own-slot-values cls '|protege|::|:SLOT-CONSTRAINTS|) vs))

(defun slot-associated-facet (slot)
  (frame-own-slot-value slot '|protege|::|:ASSOCIATED-FACET|))

(defun (setf slot-associated-facet) (vs slot)
  (setf (frame-own-slot-value slot '|protege|::|:ASSOCIATED-FACET|) vs))


;
; facet
;

(defun facet-documentation (facet)
  (frame-own-slot-value facet '|protege|::|:DOCUMENTATION|))

(defun (setf facet-documentation) (v facet)
  (setf (frame-own-slot-value facet '|protege|::|:DOCUMENTATION|) v))

(defun facet-associated-slot (facet)
  (frame-own-slot-value facet '|protege|::|:ASSOCIATED-SLOT|))

(defun (setf facet-associated-slot) (v facet)
  (setf (frame-own-slot-value facet '|protege|::|:ASSOCIATED-SLOT|) v))


;
; simple instance
;


;
; mk's functions
;
                        
(defun mk-cls (name &key 
                    (kb *kb*) 
                    (type '|protege|::|:STANDARD-CLASS|) 
                    (supercls '|protege|::|:THING|)
                    (concretep t))
  (check-type name string)
  (check-type kb kb)
  (let ((cls (make-cls name :kb kb)))
    (instance-add-direct-type cls type)
    (cls-add-direct-supercls cls supercls)
    (setf (cls-concretep cls) concretep)
    cls))
  
(defun mk-slot (name &key (kb *kb*) (type '|protege|::|:STANDARD-SLOT|))
  (check-type name string)
  (check-type kb kb)  
  (let ((slot (make-slot name :kb kb)))
    (instance-add-direct-type slot type)
    slot))

(defun mk-facet (name &key (kb *kb*) (type '|protege|::|:STANDARD-FACET|))
  (check-type name string)
  (check-type kb kb)
  (let ((facet (make-facet name :kb kb)))
    (instance-add-direct-type facet type)
    facet))

(defun mk-simple-instance (name cls &key (kb *kb*))
  (check-type name string)
  (check-type kb kb)
  (let ((si (make-simple-instance name :kb kb)))
    (instance-add-direct-type si cls)
    si))


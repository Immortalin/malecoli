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

(in-package :mlcl-kb)

;
; conversion functions
;
  
(defun string->type-value (str)
          (cond
           ((string-equal str "Any")
            'protege-kb:any-type-value)
           ((string-equal str "Boolean")
            'protege-kb:boolean-type-value)
           ((string-equal str "Float")
            'protege-kb:float-type-value)
           ((string-equal str "Integer")
            'protege-kb:integer-type-value)
           ((string-equal str "String")
            'protege-kb:string-type-value)
           ((string-equal str "Symbol")
            'protege-kb:symbol-type-value)
           ((string-equal str "Instance")
            'protege-kb:instance-type-value)
           ((string-equal str "Class")
            'protege-kb:cls-type-value)))

(defun string->role (str)
          (cond
           ((string-equal str "Abstract")
            'protege-kb:abstract-value)
           ((string-equal str "Concrete")
            'protege-kb:concrete-value)))


;
; cls
;
  
(defun cls-documentation (cls)
  (frame-own-slot-value cls 'PROTEGE-KB::|:DOCUMENTATION|))

(defun (setf cls-documentation) (v cls)
  (setf (frame-own-slot-value cls 'PROTEGE-KB::|:DOCUMENTATION|) v))

(defun cls-role (cls)
  (string->role (frame-own-slot-value cls 'PROTEGE-KB::|:ROLE|)))

(defun (setf cls-role) (v cls)
  (setf (frame-own-slot-value cls 'PROTEGE-KB::|:ROLE|) v))

(defun cls-abstractp (cls)
  (eq (cls-role cls) 'protege-kb:abstract-value))

(defun (setf cls-abstractp) (v cls)
  (setf (cls-role cls) (if v protege-kb:abstract-value protege-kb:concrete-value)))

(defun cls-concretep (cls)
  (eq (cls-role cls) 'protege-kb:concrete-value))

(defun (setf cls-concretep) (v cls)
  (setf (cls-role cls) (if v protege-kb:concrete-value protege-kb:abstract-value)))

(defun cls-constraints (cls)
  (frame-own-slot-values cls 'PROTEGE-KB::|:SLOT-CONSTRAINTS|))

(defun (setf cls-constraints) (vs cls)
  (setf (frame-own-slot-values cls 'PROTEGE-KB::|:SLOT-CONSTRAINTS|) vs))


;
; slot
;

(defun slot-documentation (slot)
  (frame-own-slot-value slot 'PROTEGE-KB::|:DOCUMENTATION|))

(defun (setf slot-documentation) (v slot)
  (setf (frame-own-slot-value slot 'PROTEGE-KB::|:DOCUMENTATION|) v))

(defun slot-value-type (slot)
  (string->type-value (frame-own-slot-value slot 'PROTEGE-KB::|:SLOT-VALUE-TYPE|)))

(defun (setf slot-value-type) (val slot)
  (setf (frame-own-slot-value slot 'PROTEGE-KB::|:SLOT-VALUE-TYPE|) val))

(defun slot-minimum-cardinality (slot)
  (frame-own-slot-value slot 'PROTEGE-KB::|:SLOT-MINIMUM-CARDINALITY|))

(defun (setf slot-minimum-cardinality) (v slot)
  (setf (frame-own-slot-value slot 'PROTEGE-KB::|:SLOT-MINIMUM-CARDINALITY|) v))

(defun slot-maximum-cardinality (slot)
  (frame-own-slot-value slot 'PROTEGE-KB::|:SLOT-MAXIMUM-CARDINALITY|))

(defun (setf slot-maximum-cardinality) (v slot)
  (setf (frame-own-slot-value slot 'PROTEGE-KB::|:SLOT-MAXIMUM-CARDINALITY|) v))

(defun slot-minimum-value (slot)
  (frame-own-slot-value slot 'PROTEGE-KB::|:SLOT-NUMERIC-MINIMUM|))

(defun (setf slot-minimum-value) (v slot)
  (setf (frame-own-slot-value slot 'PROTEGE-KB::|:SLOT-NUMERIC-MINIMUM|) v))

(defun slot-maximum-value (slot)
  (frame-own-slot-value slot 'PROTEGE-KB::|:SLOT-NUMERIC-MAXIMUM|))

(defun (setf slot-maximum-value) (v slot)
  (setf (frame-own-slot-value slot 'PROTEGE-KB::|:SLOT-NUMERIC-MAXIMUM|) v))

(defun slot-defaults (slot)
  (frame-own-slot-values slot 'PROTEGE-KB::|:SLOT-DEFAULTS|))

(defun (setf slot-defaults) (vs slot)
  (setf (frame-own-slot-values slot 'PROTEGE-KB::|:SLOT-DEFAULTS|) vs))

(defun slot-values (slot)
  (frame-own-slot-values slot 'PROTEGE-KB::|:SLOT-VALUES|))

(defun (setf slot-values) (vs slot)
  (setf (frame-own-slot-values slot 'PROTEGE-KB::|:SLOT-VALUES|) vs))

(defun slot-inverse-slot (slot)
  (frame-own-slot-value slot 'PROTEGE-KB::|:SLOT-INVERSE|))

(defun (setf slot-inverse-slot) (v slot)
  (setf (frame-own-slot-value slot 'PROTEGE-KB::|:SLOT-INVERSE|) v))

(defun slot-allowed-clses (slot)
  (and (eq (slot-value-type slot) 'protege-kb:instance-type-value)
       (cdr (frame-own-slot-values slot 'PROTEGE-KB::|:SLOT-VALUE-TYPE|))))

(defun (setf slot-allowed-clses) (vs slot)
  (setf (frame-own-slot-values slot 'PROTEGE-KB::|:SLOT-VALUE-TYPE|)
        (cons protege-kb:instance-type-value vs)))

(defun slot-allowed-parents (slot)
  (and (eq (slot-value-type slot) 'protege-kb:cls-type-value)
       (cdr (frame-own-slot-values slot 'PROTEGE-KB::|:SLOT-VALUE-TYPE|))))

(defun (setf slot-allowed-parents) (vs slot)
  (setf (frame-own-slot-values slot 'PROTEGE-KB::|:SLOT-VALUE-TYPE|)
        (cons protege-kb:cls-type-value vs)))

(defun slot-allowed-values (slot)
  (and (eq (slot-value-type slot) 'protege-kb:symbol-type-value)
       (cdr (frame-own-slot-values slot 'PROTEGE-KB::|:SLOT-VALUE-TYPE|))))

(defun (setf slot-allowed-values) (vs slot)
  (setf (frame-own-slot-values slot 'PROTEGE-KB::|:SLOT-VALUE-TYPE|)
        (cons protege-kb:symbol-type-value vs)))

(defun slot-constraints (cls)
  (frame-own-slot-values cls 'PROTEGE-KB::|:SLOT-CONSTRAINTS|))

(defun (setf slot-constraints) (vs cls)
  (setf (frame-own-slot-values cls 'PROTEGE-KB::|:SLOT-CONSTRAINTS|) vs))

(defun slot-associated-facet (slot)
  (frame-own-slot-value slot 'PROTEGE-KB::|:ASSOCIATED-FACET|))

(defun (setf slot-associated-facet) (vs slot)
  (setf (frame-own-slot-value slot 'PROTEGE-KB::|:ASSOCIATED-FACET|) vs))


;
; facet
;

(defun facet-documentation (facet)
  (frame-own-slot-value facet 'PROTEGE-KB::|:DOCUMENTATION|))

(defun (setf facet-documentation) (v facet)
  (setf (frame-own-slot-value facet 'PROTEGE-KB::|:DOCUMENTATION|) v))

(defun facet-associated-slot (facet)
  (frame-own-slot-value facet 'PROTEGE-KB::|:ASSOCIATED-SLOT|))

(defun (setf facet-associated-slot) (v facet)
  (setf (frame-own-slot-value facet 'PROTEGE-KB::|:ASSOCIATED-SLOT|) v))


;
; simple instance
;


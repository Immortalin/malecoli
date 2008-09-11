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

;;;; Created on 2008-09-10 15:00:09

(in-package :mlcl-dataset)

;
; dataset kb compile
;

(defun dataset-kb-compile (package kb strm)
  (mlcl-kb:kb-open kb)
  (let ((compinfo (make-compiler-info)))
    (dataset-kb-compile-header package strm)
    (let ((cls-list))
      (dolist (el (mlcl-kb:kb-interned-elements kb))
        (if (and (typep el 'mlcl-kb:cls) 
                 (mlcl-kb:cls-has-supercls el 'dataset-kb::|DatasetThing|))
            (push el cls-list)))
      (dataset-kb-compile-clses cls-list strm compinfo))  
    (dataset-kb-compile-trailer package strm))
  (mlcl-kb:kb-close kb))
  
(defstruct compiler-info
  (enum-types nil))

;
; compile header/trailer
;

(defun dataset-kb-compile-header (package strm)
  (format strm ";;;; Created on ~A~%~%" (get-universal-time))
  (format strm "(in-package \"~A\")~%~%" (package-name package)))

(defun dataset-kb-compile-trailer (package strm)
  (declare (ignore package))
  (format strm "(defun init-dataset ()")
  (format strm "~%	(init-dataset-clses))")
  (format strm "~%~%")
  (format strm ";;;; Created on ~A~%" (get-universal-time)))


;
; compile clses
;

(defun dataset-kb-compile-clses (cls-list strm compinfo)
  (dolist (cls cls-list)
    (dataset-kb-compile-cls cls strm compinfo))
  (dolist (typ (compiler-info-enum-types compinfo))
    (dataset-kb-compile-enum typ strm))
  (format strm "(defun init-dataset-clses ()")
  (format strm ")~%")
  (format strm "~%~%"))

(defun dataset-kb-compile-cls (cls strm compinfo)
  (format strm ";;;; Generation of cls ~A~%~%" (mlcl-kb:frame-name cls))
  (format strm "(defclass |~A| (~{|~A| ~}) (" (frame->lisp-name cls) 
          (mapcar #'(lambda (s) (frame->lisp-name s))
                  (mlcl-kb:cls-direct-superclses cls)))
  (dolist (slot (mlcl-kb:cls-direct-template-slots cls))
    (dataset-kb-compile-slot slot strm compinfo))
  (format strm "))~%")
  (format strm "~%~%"))

(defun dataset-kb-compile-slot (slot strm compinfo)
  (format strm "~%	(|~A|~%	 "
          (frame->lisp-name slot))
  (dataset-kb-compile-slot-type slot strm compinfo)
  (format strm "~%	 :accessor ~A)" 
          (format nil "|~A|" (frame->lisp-name slot))))

(defun dataset-kb-compile-slot-type (slot strm compinfo)
  (if (eq (mlcl-kb:slot-maximum-cardinality slot) 1)
      (let ((typ (mlcl-kb:slot-value-type slot)))
        (cond 
         ((eq typ 'mlcl-kb:integer-type-value)
          (format strm ":type integer"))
         ((eq typ 'mlcl-kb:float-type-value)
          (format strm ":type float"))
         ((eq typ 'mlcl-kb:string-type-value)
          (format strm ":type string"))
         ((eq typ 'mlcl-kb:boolean-type-value)
          (format strm ":type boolean"))
         ((eq typ 'mlcl-kb:symbol-type-value)
          (push slot (compiler-info-enum-types compinfo))
          (format strm ":type string"))
         ((eq typ 'mlcl-kb:instance-type-value)
          (if (eq (length (mlcl-kb:slot-allowed-clses slot)) 1)
              (format strm ";; :type ~A" (frame->lisp-name (car (mlcl-kb:slot-allowed-clses slot))))))))
      (format strm ":type list")))
        

;
; enum
;

(defun dataset-kb-compile-enum (slot strm)
  (format strm ";;;; Generation of enum ~A~%~%" (mlcl-kb:frame-name slot))
  (format strm "~%"))

  
;
; conversion functions
;

(defun frame->lisp-name (fr)
  (let ((name (mlcl-kb:frame-name fr)))
    (if (eq (mlcl-kb:frame-kb fr) (symbol-value 'mlcl-kbs::dataset-kb))
        (progn
          (cond
           ((mlcl-kb:frame-equal fr 'dataset-kb::|DatasetCase|)
            (setf name "DATASET-CASE"))
           ((mlcl-kb:frame-equal fr 'dataset-kb::|DatasetThing|)
            (setf name "DATASET-THING"))))
        (progn
          (setf name (cl-ppcre:regex-replace-all " " name "_"))
          (setf name (cl-ppcre:regex-replace-all "\\?" name "p"))
          (setf name (cl-ppcre:regex-replace-all "\\." name "_"))))
    name))


;
; load
;

(defun dataset-kb-import (schema kb)
  (mlcl-kb:kb-open kb)
  (let ((importinfo (make-import-info)))
    (let ((si-list nil)
          (ds-list nil))
      (dolist (el (mlcl-kb:kb-interned-elements kb))
        (if (and (typep el 'mlcl-kb:simple-instance) 
                 (mlcl-kb:instance-has-type el 'dataset-kb::|DatasetCase|))
            (push el si-list))
        (if (and (typep el 'mlcl-kb:simple-instance) 
                 (mlcl-kb:instance-has-type el 'dataset-kb::|Dataset|))
            (push el ds-list)))
      (dataset-kb-import-cases schema si-list importinfo)
      (dataset-kb-import-datasets schema ds-list importinfo))
    (mlcl-kb:kb-close kb)
    (values (import-info-cases importinfo)
            (import-info-datasets importinfo))))

(defstruct import-info
  (cases nil)
  (datasets nil)
  (objects (MAKE-HASH-TABLE :test #'equal)))

;
; import instance
;

(defun dataset-kb-import-cases (schema si-list importinfo)
  (dolist (si si-list)
    (let ((el (dataset-kb-import-simple-instance schema si importinfo)))
      (push el (import-info-cases importinfo)))))

(defun dataset-kb-import-simple-instance (schema si importinfo)
  (let ((el (make-instance (find-symbol (frame->lisp-name (mlcl-kb:instance-direct-type si)) 
                                        (schema-package schema))
                           :name-id (frame->lisp-name si))))
    (setf (gethash (dataset-thing-name-id el) (import-info-objects importinfo)) el)
    (dolist (osv (mlcl-kb:frame-own-slot-values-list si))
      (dataset-kb-import-own-slot-value schema el si osv importinfo))
    el))

(defun dataset-kb-get-simple-instance (schema si importinfo)
  (let ((el (gethash (frame->lisp-name si) (import-info-objects importinfo))))
    (if (null el)
        (dataset-kb-import-simple-instance schema si importinfo)
        el)))
        
(defun dataset-kb-import-own-slot-value (schema el si osv importinfo)
  (declare (ignore si))
  (let ((slot (mlcl-kb:slot-values%-slot osv))
        (vals (mlcl-kb:slot-values%-vals osv)))
    (if vals
        (progn
          (eval (list 'setf 
                   (list (find-symbol 
                           (frame->lisp-name slot)
                           (schema-package schema))
                         el)
                   (list 'quote (dataset-kb-import-slot-value schema slot vals importinfo))))))))


(defun dataset-kb-import-slot-value (schema slot vals importinfo)
  (let ((typ (mlcl-kb:slot-value-type slot)))
    (let ((converter-values
           (cond 
            ((eq typ 'mlcl-kb:integer-type-value)
             vals)
            ((eq typ 'mlcl-kb:float-type-value)
             vals)
            ((eq typ 'mlcl-kb:string-type-value)
             vals)
            ((eq typ 'mlcl-kb:boolean-type-value)
             vals)
            ((eq typ 'mlcl-kb:symbol-type-value)
             vals)
            ((eq typ 'mlcl-kb:instance-type-value)
             (mapcar #'(lambda(x) (dataset-kb-get-simple-instance schema x importinfo)) vals)))))
      (if (eq (mlcl-kb:slot-maximum-cardinality slot) 1)
          (progn 
            (car converter-values))
          (progn
            converter-values)))))


;
;
;


(defun dataset-kb-import-datasets (schema ds-list importinfo)
  (declare (ignore schema))
  (dolist (ds ds-list)
    (let ((dsname (mlcl-kb:frame-name ds))
          (poses (mapcar #'(lambda (si) (gethash (frame-name si) (import-info-objects importinfo)))
                         (mlcl-kb:frame-own-slot-values ds 'dataset-kb::|dataset_case|))))
      (push (cons dsname poses) (import-info-datasets importinfo)))))


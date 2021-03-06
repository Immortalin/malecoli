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

(in-package :mlcl)

;
; import functions
;

(defun dataset-kb-import (package kb)
  (cl-kb:with-kb kb nil
                 (let ((importinfo (make-import-info)))
                   (let ((si-list nil)
                         (ds-list nil))
                     (cl-kb::cls-do-instance-list (cl-kb:find-frame '|dataset|::|DatasetCase|) inst 
                                                  (push inst si-list))
                     (cl-kb::cls-do-instance-list (cl-kb:find-frame '|dataset|::|Dataset|) d 
                                                  (push d ds-list))
                     (dataset-kb-import-cases package si-list importinfo)
                     (dataset-kb-import-datasets package ds-list importinfo))
                   (values (import-info-cases importinfo)
                           (import-info-datasets importinfo)))))

(defun dataset-kb-case-import (package cas)
  (let ((importinfo (make-import-info)))
    (dataset-kb-import-simple-instance package cas importinfo)))


;
; structure
;

(defstruct import-info
  (cases nil)
  (datasets nil)
  (objects (MAKE-HASH-TABLE :test #'equal :size 200)))


;
; import instances
;

(defun dataset-kb-import-cases (package si-list importinfo)
  (dolist (si si-list)
    (let ((el (dataset-kb-import-simple-instance package si importinfo)))
      (push el (import-info-cases importinfo)))))

(defun dataset-kb-import-simple-instance (package si &optional (importinfo (make-import-info)))
  (let ((symb (cl-kb:frame->symbol (cl-kb:instance-direct-type si) package)))
    (if (null symb)
        (error "undefined class ~A in package ~A " (cl-kb:instance-direct-type si) (package-name package)))
    (let ((el nil))
      (cond
        ((cl-kb:instance-has-type si '|dataset|::|DatasetThing|)
         (setf el (make-instance symb :name-id (cl-kb:frame-name si)))
         (setf (gethash (cl-kb:frame-name si) (import-info-objects importinfo)) el))
        ((cl-kb:instance-has-type si '|dataset|::|DatasetDataType|)
         (setf el (make-instance symb)))
        (t
         (error "the instance ~A must have DatasetThing or DatasetDataType as type. " (cl-kb:frame-name si))))
      (cl-kb:frame-do-own-slot-values-list si slot vals
                                           (dataset-kb-import-own-slot-value package el si slot vals importinfo))
      el)))

(defun dataset-kb-get-simple-instance (package si importinfo)
  (let ((el (gethash (cl-kb:frame-name si) (import-info-objects importinfo))))
    (if (null el)
        (dataset-kb-import-simple-instance package si importinfo)
        el)))

(defun dataset-kb-import-own-slot-value (package el si slot vals importinfo)
  (declare (ignore si))
  (if vals
      (let ((symb (find-symbol (cl-kb:frame-name slot) package)))
        (if (null symb)
            (error "undefined slot ~A in package ~A " (cl-kb:frame-name slot) (package-name package)))
        (setf (slot-value el symb) (dataset-kb-import-slot-value package slot vals importinfo)))))

(defun dataset-kb-import-slot-value (package slot vals importinfo)
  (let ((typ (cl-kb:slot-value-type slot)))
    (let ((converter-values
           (cond 
            ((eq typ 'cl-kb:integer-type-value)
             vals)
            ((eq typ 'cl-kb:float-type-value)
             vals)
            ((eq typ 'cl-kb:string-type-value)
             vals)
            ((eq typ 'cl-kb:boolean-type-value)
             vals)
            ((eq typ 'cl-kb:symbol-type-value)
             vals)
            ((eq typ 'cl-kb:instance-type-value)
             (mapcar #'(lambda(x) (dataset-kb-get-simple-instance package x importinfo)) vals))
            (t (error "undefined protege value type ~A for slot ~A (~A). " typ (cl-kb:frame-name slot) (type-of slot))))))
      (if (eq (cl-kb:slot-maximum-cardinality slot) 1)
          (progn 
            (car converter-values))
          (progn
            converter-values)))))


;
;
;


(defun dataset-kb-import-datasets (package ds-list importinfo)
  (declare (ignore package))
  (dolist (ds ds-list)
    (let ((dsname (cl-kb:frame-name ds))
          (poses (mapcar #'(lambda (si) (gethash (cl-kb:frame-name si) (import-info-objects importinfo)))
                         (cl-kb:frame-own-slot-values ds '|dataset|::|dataset_case|))))
      (push (cons dsname poses) (import-info-datasets importinfo)))))


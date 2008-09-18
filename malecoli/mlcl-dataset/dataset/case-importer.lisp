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
; import functions
;

(defun dataset-kb-import (package kb)
  (mlcl-kb:kb-open kb)
  (let ((importinfo (make-import-info)))
    (let ((si-list nil)
          (ds-list nil))
      (dolist (el (mlcl-kb:kb-interned-elements kb))
        (if (and (typep el 'mlcl-kb:simple-instance) 
                 (mlcl-kb:instance-has-type el '|dataset|::|DatasetCase|))
            (push el si-list))
        (if (and (typep el 'mlcl-kb:simple-instance) 
                 (mlcl-kb:instance-has-type el '|dataset|::|Dataset|))
            (push el ds-list)))
      (dataset-kb-import-cases package si-list importinfo)
      (dataset-kb-import-datasets package ds-list importinfo))
    (mlcl-kb:kb-close kb)
    (values (import-info-cases importinfo)
            (import-info-datasets importinfo))))

(defun dataset-kb-case-import (package cas)
  (let ((importinfo (make-import-info)))
    (dataset-kb-import-simple-instance package cas importinfo)))


;
; structure
;

(defstruct import-info
  (cases nil)
  (datasets nil)
  (objects (MAKE-HASH-TABLE :test #'equal)))


;
; import instances
;

(defun dataset-kb-import-cases (package si-list importinfo)
  (dolist (si si-list)
    (let ((el (dataset-kb-import-simple-instance package si importinfo)))
      (push el (import-info-cases importinfo)))))

(defun dataset-kb-import-simple-instance (package si importinfo)
  (let ((symb (find-symbol (frame->lisp-name (mlcl-kb:instance-direct-type si)) package)))
    (if (null symb)
        (error "undefined class ~A in package ~A " (frame->lisp-name (mlcl-kb:instance-direct-type si)) (package-name package)))
    (let ((el (make-instance symb :name-id (frame->lisp-name si))))
      (setf (gethash (dataset-thing-name-id el) (import-info-objects importinfo)) el)
      (mlcl-kb:frame-do-own-slot-values-list si slot vals
                                     (dataset-kb-import-own-slot-value package el si slot vals importinfo))                      
      el)))


;
;
;

(defun dataset-kb-get-simple-instance (package si &optional (importinfo (make-import-info)))
  (let ((el (gethash (frame->lisp-name si) (import-info-objects importinfo))))
    ;(format t "GET ~A ~%" (frame->lisp-name si))
    (if (null el)
        (dataset-kb-import-simple-instance package si importinfo)
        el)))

(defun dataset-kb-import-own-slot-value (package el si slot vals importinfo)
  (declare (ignore si))
  (if vals
      (progn
        (let ((symb (find-symbol (frame->lisp-name slot) package)))
          (if (null symb)
              (error "undefined slot ~A in package ~A " (mlcl-kb:frame-name slot) (package-name package)))
          (eval (list 'setf 
                      (list symb el)
                      (list 'quote (dataset-kb-import-slot-value package slot vals importinfo))))))))

(defun dataset-kb-import-slot-value (package slot vals importinfo)
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
             (mapcar #'(lambda(x) (dataset-kb-get-simple-instance package x importinfo)) vals))
            (t (error "undefined protege value type ~A for slot ~A (~A). " typ (mlcl-kb:frame-name slot) (type-of slot))))))
      (if (eq (mlcl-kb:slot-maximum-cardinality slot) 1)
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
    (let ((dsname (mlcl-kb:frame-name ds))
          (poses (mapcar #'(lambda (si) (gethash (frame-name si) (import-info-objects importinfo)))
                         (mlcl-kb:frame-own-slot-values ds '|dataset|::|dataset_case|))))
      (push (cons dsname poses) (import-info-datasets importinfo)))))


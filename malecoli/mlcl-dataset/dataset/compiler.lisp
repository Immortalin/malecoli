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

;;;; Created on 2008-09-04 16:08:58

(in-package :mlcl-dataset)

;
; compile header/trailer
;

(defun compile-header (dataset-name strm)
  (format strm ";;;; Created on ~A~%~%" (get-universal-time))
  (format strm "(defpackage :|~A-ds| (:use :cl :mlcl-kb :mlcl-dataset :clsql))~%~%" dataset-name)
  (format strm "(in-package :|~A-ds|)~%~%" dataset-name))

(defun compile-trailer (strm)
  (format strm "(defun init-dataset ()")
  (format strm "~%	(init-dataset-clses)")
  (format strm "~%	(init-dataset-simple-instances))")
  (format strm "~%~%")
  (format strm ";;;; Created on ~A~%" (get-universal-time)))


;
; compile clses
;

(defun compile-clses (cls-list strm)
  (dolist (cls cls-list)
    (compile-cls cls strm))
  (format strm "(defun init-dataset-clses ()")
  (dolist (cls cls-list)
    (format strm "~%	(clsql:create-view-from-class '|~A|)" (mlcl-kb:frame-name cls)))
  (format strm ")~%")
  (format strm "~%~%"))

(defun compile-cls (cls strm)
  (format strm ";;;; Generation of cls ~A~%~%" (mlcl-kb:frame-name cls))
  (format strm "(def-view-class |~A| (~{|~A| ~}) (" (mlcl-kb:frame-name cls) 
          (mapcar #'(lambda (s) (frame-name s))
                  (mlcl-kb:cls-direct-superclses cls)))
  (dolist (slot (mlcl-kb:cls-direct-template-slots cls))
    (compile-slot slot strm))
  (format strm ")~%	(:base-table |~A|))~%" (frame-name cls))
  (format strm "~%~%"))

(defun compile-slot (slot strm)
  (format strm "~%	(|~A|~%	 "
          (mlcl-kb:frame-name slot))
  (compile-slot-type slot strm)
  (format strm "~%	 :accessor ~A)" 
          (format nil "|~A|" (mlcl-kb:frame-name slot))))

(defun compile-slot-type (slot strm)
  (let ((typ (mlcl-kb:slot-value-type slot)))
    (cond 
     ((eq typ 'protege-kb::integer-type-value)
      (format strm ":type integer"))
     ((eq typ 'protege-kb::float-type-value)
      (format strm ":type float"))
     ((eq typ 'protege-kb::string-type-value)
      (format strm ":type string"))
     ((eq typ 'protege-kb::boolean-type-value)
      (format strm ":type boolean"))
     ((eq typ 'protege-kb::symbol-type-value)
      (format strm ":type string"))
     ((eq typ 'protege-kb::instance-type-value)
      (format strm ":db-kind :join :db-info (:join-class |~A| :home-key name-id :foreign-key name-id :set ~A)" 
              (frame-name (car (mlcl-kb:slot-allowed-clses slot))) (eq (slot-maximum-value slot) 1))))))
;
; compile simple instances
;

(defun compile-simple-instances (si-list strm)
  (format strm ";;;; Generation of instances ~%~%")
  (format strm "(defun init-dataset-simple-instances ()")
  (format strm "~%	(DECLARE (OPTIMIZE (SPEED 0) (SAFETY 3) (DEBUG 3)))")
  (format strm "~%	(let (")
  (dolist (si si-list)
    (compile-simple-instance-header si strm))
  (format strm ")~%")
  (format strm "~%	(progn ")
  (dolist (si si-list)
    (compile-simple-instance si strm))
  (dolist (si si-list)
    (compile-simple-instance-trailer si strm))
  (format strm ")))~%~%"))

(defun compile-simple-instance-header (si strm)
  (format strm "~%		(|~A| (make-instance '|~A| :name-id \"~A\"))" 
          (mlcl-kb:frame-name si)
          (mlcl-kb:frame-name (mlcl-kb:instance-direct-type si))
          (mlcl-kb:frame-name si)))

(defun compile-simple-instance-trailer (si strm)
  (format strm "~%		(clsql:update-records-from-instance |~A|)" 
          (mlcl-kb:frame-name si)))



(defun compile-simple-instance (si strm)
  (format strm "~%	;;;; Generation of instances ~A~%" (mlcl-kb:frame-name si))
  (dolist (osv (mlcl-kb:frame-own-slot-values-list si))
    (compile-own-slot-value si osv strm)))

(defun compile-own-slot-value (si osv strm)
  (let ((slot (slot-value%-slot osv))
        (vals (slot-value%-vals osv)))
    (if vals
        (progn
          (format strm "~%	(setf (|~A| |~A|) "
                  (frame-name slot)
                  (frame-name si))
          (compile-slot-value slot vals strm)
          (format strm ")")))))

(defun compile-slot-value (slot vals strm)
  (let ((typ (mlcl-kb:slot-value-type slot)))
    (let ((converter-values
           (cond 
            ((eq typ 'protege-kb::integer-type-value)
             vals)
            ((eq typ 'protege-kb::float-type-value)
             vals)
            ((eq typ 'protege-kb::string-type-value)
             (mapcar #'(lambda (x) (format nil "\"~A\"" x)) vals))
            ((eq typ 'protege-kb::boolean-type-value)
             vals)
            ((eq typ 'protege-kb::symbol-type-value)
             (mapcar #'(lambda (x) (format nil "\"~A\"" x)) vals))
            ((eq typ 'protege-kb::instance-type-value)
             (mapcar #'frame-name vals)))))
      (if (eq (mlcl-kb:slot-maximum-cardinality slot) 1)
          (format strm "~a" (car converter-values))
          (format strm "(~{ ~a ~})" converter-values)))))

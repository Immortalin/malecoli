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
; compile
;

(defun kb-compile (dataset kb strm)
  (mlcl-kb:kb-open kb)
  (let ((compinfo (make-compiler-info)))
    (kb-compile-header dataset strm)
    (let ((cls-list))
      (dolist (el (mlcl-kb:kb-interned-elements kb))
        (if (and (typep el 'mlcl-kb:cls) 
                 (mlcl-kb:cls-has-supercls el 'dataset-kb::|DatasetThing|))
            (push el cls-list)))
      (kb-compile-clses cls-list strm compinfo))  
    (kb-compile-trailer dataset strm))
  (mlcl-kb:kb-close kb))
  
(defstruct compiler-info
  (enum-types nil)
  (multi-types nil))
;
; compile header/trailer
;

(defun kb-compile-header (dataset strm)
  (format strm ";;;; Created on ~A~%~%" (get-universal-time))
  (format strm "(in-package \"~A\")~%~%" (package-name (dataset-package dataset))))

(defun kb-compile-trailer (dataset strm)
  (declare (ignore dataset))
  (format strm "(defun init-dataset ()")
  (format strm "~%	(init-dataset-clses))")
  (format strm "~%~%")
  (format strm ";;;; Created on ~A~%" (get-universal-time)))


;
; compile clses
;

(defun kb-compile-clses (cls-list strm compinfo)
  (dolist (cls cls-list)
    (kb-compile-cls cls strm compinfo))
  (dolist (typ (compiler-info-enum-types compinfo))
    (kb-compile-enum typ strm))
  (dolist (typ (compiler-info-multi-types compinfo))
    (kb-compile-multi typ strm))
  (format strm "(defun init-dataset-clses ()")
  (format strm "~%	(clsql:create-view-from-class 'mlcl-dataset:|DatasetThing|)")
  (format strm "~%	(clsql:create-view-from-class 'mlcl-dataset:|DatasetCase|)")
  (format strm "~%	(clsql:create-view-from-class 'mlcl-dataset:enumeration)")
  (dolist (cls cls-list)
    (format strm "~%	(clsql:create-view-from-class '|~A|)" (frame->sql-name cls)))
  (dolist (typ (compiler-info-enum-types compinfo))
    (format strm "~%	(clsql:create-view-from-class '|~A|)" (frame->sql-name typ)))
  (dolist (typ (compiler-info-multi-types compinfo))
    (format strm "~%	(clsql:create-view-from-class '|~A|)" (frame->sql-name typ)))
  (format strm ")~%")
  (format strm "~%~%"))

(defun kb-compile-cls (cls strm compinfo)
  (format strm ";;;; Generation of cls ~A~%~%" (mlcl-kb:frame-name cls))
  (format strm "(def-view-class |~A| (~{|~A| ~}) (" (frame->sql-name cls) 
          (mapcar #'(lambda (s) (frame->sql-name s))
                  (mlcl-kb:cls-direct-superclses cls)))
  (dolist (slot (mlcl-kb:cls-direct-template-slots cls))
    (kb-compile-slot slot strm compinfo))
  (format strm ")~%	(:base-table |~A|))~%" (frame->sql-name cls))
  (format strm "~%~%"))

(defun kb-compile-slot (slot strm compinfo)
  (format strm "~%	(|~A|~%	 "
          (frame->sql-name slot))
  (kb-compile-slot-type slot strm compinfo)
  (format strm "~%	 :accessor ~A)" 
          (format nil "|~A|" (frame->sql-name slot))))

(defun kb-compile-slot-type (slot strm compinfo)
  (if (eq (slot-maximum-cardinality slot) 1)
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
          (push slot (compiler-info-enum-types compinfo))
          (format strm ":db-kind :join :db-info (:join-class |~A| :home-key |~A| :foreign-key id :set nil)" 
                  (frame->sql-name slot) (frame->sql-name slot)))
         ((eq typ 'protege-kb::instance-type-value)
          (format strm ":db-kind :join :db-info (:join-class |~A| :home-key |~A| :foreign-key name-id :set nil)" 
                  (if (eq (length (mlcl-kb:slot-allowed-clses slot)) 1)
                      (frame->sql-name (car (mlcl-kb:slot-allowed-clses slot)))
                      '|DatasetThing|)
                  (frame->sql-name slot)))))
      (let ((typ (mlcl-kb:slot-value-type slot)))
        (cond 
         ((or (eq typ 'protege-kb::integer-type-value)
              (eq typ 'protege-kb::float-type-value)
              (eq typ 'protege-kb::string-type-value)
              (eq typ 'protege-kb::boolean-type-value))
          (let ((ty (format nil "~A-type" (frame->sql-name slot))))
            (push slot (compiler-info-multi-types compinfo))
            (format strm ":db-kind :join :db-info (:join-class |~A| :home-key |~A| :foreign-key name-id :set t)" 
                    ty (frame->sql-name slot) )))
         ((eq typ 'protege-kb::symbol-type-value)
          (push slot (compiler-info-enum-types compinfo))
          (format strm ":db-kind :join :db-info (:join-class |~A| :home-key |~A| :foreign-key id :set t)" 
                  (frame->sql-name slot) (frame->sql-name slot)))
         ((eq typ 'protege-kb::instance-type-value)
          (format strm ":db-kind :join :db-info (:join-class |~A| :home-key |~A| :foreign-key name-id :set t)" 
                  (if (eq (length (mlcl-kb:slot-allowed-clses slot)) 1)
                      (frame->sql-name (car (mlcl-kb:slot-allowed-clses slot)))
                      '|DatasetThing|)
                  (frame->sql-name slot)))))))
  

;
; enum/multi
;

(defun kb-compile-enum (slot strm)
  (format strm ";;;; Generation of enum ~A~%~%" (mlcl-kb:frame-name slot))
  (format strm "(def-view-class |~A| (enumeration) (" (frame->sql-name slot))
  (format strm ")~%	(:base-table |~A|))~%" (frame->sql-name slot))
  (format strm "~%~%"))

(defun kb-compile-multi (slot strm)
  (format strm ";;;; Generation of multi ~A~%~%" (mlcl-kb:frame-name slot))
  (format strm "(def-view-class |~A| (|DatasetThing|) (" (frame->sql-name slot))
  (format strm "~%	(value")
  (format strm "~%	 :accessor |~A|-value" (frame->sql-name slot))
  (format strm "~%	")
  (let ((typ (mlcl-kb:slot-value-type slot)))
             (cond 
              ((eq typ 'protege-kb::integer-type-value)
               (format strm ":type integer"))
              ((eq typ 'protege-kb::float-type-value)
               (format strm ":type float"))
              ((eq typ 'protege-kb::string-type-value)
               (format strm ":type string"))
              ((eq typ 'protege-kb::boolean-type-value)
               (format strm ":type boolean"))))
  (format strm "))~%	(:base-table |~A|))~%" (frame->sql-name slot))
  (format strm "~%~%"))

  
;
; conversion functions
;

(defun frame->sql-name (fr)
  (let ((name (mlcl-kb:frame-name fr)))
    (setf name (cl-ppcre:regex-replace-all " " name "_"))
    (setf name (cl-ppcre:regex-replace-all "\\?" name "p"))
    (setf name (cl-ppcre:regex-replace-all "\\." name "_"))
    name))

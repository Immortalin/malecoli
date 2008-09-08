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
  (kb-compile-header dataset strm)
  (mlcl-kb:kb-open kb)
  (let ((cls-list))
    (dolist (el (mlcl-kb:kb-interned-elements kb))
      (if (and (typep el 'mlcl-kb:cls) 
               (mlcl-kb:cls-has-supercls el 'dataset-kb::|DatasetThing|))
          (push el cls-list)))
    (kb-compile-clses cls-list strm))
  (mlcl-kb:kb-close kb)
  (kb-compile-trailer dataset strm))


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

(defun kb-compile-clses (cls-list strm)
  (dolist (cls cls-list)
    (kb-compile-cls cls strm))
  (format strm "(defun init-dataset-clses ()")
  (dolist (cls cls-list)
    (format strm "~%	(clsql:create-view-from-class '|~A|)" (frame->sql-name cls)))
  (format strm ")~%")
  (format strm "~%~%"))

(defun kb-compile-cls (cls strm)
  (format strm ";;;; Generation of cls ~A~%~%" (mlcl-kb:frame-name cls))
  (format strm "(def-view-class |~A| (~{|~A| ~}) (" (frame->sql-name cls) 
          (mapcar #'(lambda (s) (frame->sql-name s))
                  (mlcl-kb:cls-direct-superclses cls)))
  (dolist (slot (mlcl-kb:cls-direct-template-slots cls))
    (kb-compile-slot slot strm))
  (format strm ")~%	(:base-table |~A|))~%" (frame->sql-name cls))
  (format strm "~%~%"))

(defun kb-compile-slot (slot strm)
  (format strm "~%	(|~A|~%	 "
          (frame->sql-name slot))
  (kb-compile-slot-type slot strm)
  (format strm "~%	 :accessor ~A)" 
          (format nil "|~A|" (frame->sql-name slot))))

(defun kb-compile-slot-type (slot strm)
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
; conversion functions
;

(defun frame->sql-name (fr)
  (let ((name (mlcl-kb:frame-name fr)))
    (setf name (cl-ppcre:regex-replace-all " " name "_"))
    (setf name (cl-ppcre:regex-replace-all "\\?" name "p"))
    (setf name (cl-ppcre:regex-replace-all "\\." name "_"))
    name))

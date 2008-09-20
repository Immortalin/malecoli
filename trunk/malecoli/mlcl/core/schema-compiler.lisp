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
; dataset kb compile
;

(defun schema-compile (package kb strm)
  (cl-kb:kb-open kb)
  (let ((compinfo (make-compiler-info)))
    (schema-compile-header package kb strm)
    (let ((cls-list))
      (cl-kb:cls-do-subcls-list (cl-kb:find-cls '|dataset|::|DatasetThing|)
                                 el
                                 (if (eq kb (cl-kb:frame-kb el))
                                     (push el cls-list))) 
      (schema-compile-clses package cls-list strm compinfo))  
    (schema-compile-trailer package strm))
  (cl-kb:kb-close kb))
  
(defstruct compiler-info
  (enum-types nil)
  (symbols nil))


;
; compile header/trailer
;

(defun schema-compile-header (package kb strm)
  (format strm ";;;; Created on ~A~%~%" (get-universal-time))
  (format strm "(in-package \"~A\")~%~%" (package-name package))
  (dolist (ukb (cl-kb:kb-use-list kb))
    (if (member (cl-kb:find-kb 'cl-kbs::|dataset|) (cl-kb:kb-use-list ukb))
        (format strm "(use-package \"~A-ws\")~%" (package-name (cl-kb:kb-package ukb)))))
  (format strm "~%~%"))

(defun schema-compile-trailer (package strm)
  (format strm "(defun init ()")
  (format strm "~%	(init-clses))")
  (format strm "~%~%")
  (format strm "(format t \"!! loaded ~A !!~A\")" (package-name package) "~%~%")
  (format strm "~%~%")
  (format strm ";;;; Created on ~A~%" (get-universal-time)))


;
; compile clses
;

(defun schema-compile-clses (package cls-list strm compinfo)
  (dolist (cls cls-list)
    (schema-compile-cls cls strm compinfo))
  (dolist (typ (compiler-info-enum-types compinfo))
    (schema-compile-enum typ strm))
  (format strm "(defun init-clses ()")
  (format strm ")~%")
  (format strm "~%~%")
  (format strm "(export '(")
  (dolist (sym (compiler-info-symbols compinfo))
    (format strm " |~A|" sym))
  (format strm ") (find-package \"~A\"))" (package-name package))
  (format strm "~%~%"))
  
(defun schema-compile-cls (cls strm compinfo)
  (push (frame->lisp-name cls) (compiler-info-symbols compinfo))
  (format strm ";;;; Generation of cls ~A~%~%" (cl-kb:frame-name cls))
  (format strm "(defclass |~A| (~{|~A| ~}) (" (frame->lisp-name cls) 
          (mapcar #'(lambda (s) (frame->lisp-name s))
                  (cl-kb:cls-direct-superclses cls)))
  (dolist (slot (cl-kb:cls-direct-template-slots cls))
    (push (frame->lisp-name slot) (compiler-info-symbols compinfo))
    (schema-compile-slot slot strm compinfo))
  (format strm "))~%")
  (format strm "~%~%"))

(defun schema-compile-slot (slot strm compinfo)
  (format strm "~%	(|~A|~%	 "
          (frame->lisp-name slot))
  (schema-compile-slot-type slot strm compinfo)
  (format strm "~%	 :accessor ~A)" 
          (format nil "|~A|" (frame->lisp-name slot))))

(defun schema-compile-slot-type (slot strm compinfo)
  (if (eq (cl-kb:slot-maximum-cardinality slot) 1)
      (let ((typ (cl-kb:slot-value-type slot)))
        (cond 
         ((eq typ 'cl-kb:integer-type-value)
          (format strm ":type integer"))
         ((eq typ 'cl-kb:float-type-value)
          (format strm ":type float"))
         ((eq typ 'cl-kb:string-type-value)
          (format strm ":type string"))
         ((eq typ 'cl-kb:boolean-type-value)
          (format strm ":type boolean"))
         ((eq typ 'cl-kb:symbol-type-value)
          (push slot (compiler-info-enum-types compinfo))
          (format strm ":type string"))
         ((eq typ 'cl-kb:instance-type-value)
          (if (eq (length (cl-kb:slot-allowed-clses slot)) 1)
              (format strm ";; :type ~A" (frame->lisp-name (car (cl-kb:slot-allowed-clses slot))))))))
      (format strm ":type list")))
        

;
; enum
;

(defun schema-compile-enum (slot strm)
  (format strm ";;;; Generation of enum ~A~%~%" (cl-kb:frame-name slot))
  (format strm "~%"))

  
;
; conversion functions
;

(defun frame->lisp-name (fr)
  (let ((name (cl-kb:frame-name fr)))
    (if (cl-kb:frame-in-kb-p fr (symbol-value 'cl-kbs::|dataset|))
        (progn
          (cond
            ((cl-kb:frame-equal fr '|dataset|::|DatasetCase|)
             (setf name "DATASET-CASE"))
            ((cl-kb:frame-equal fr '|dataset|::|DatasetThing|)
             (setf name "DATASET-THING"))
            ((cl-kb:frame-equal fr '|dataset|::|date|)
             (setf name "DATASET-DATE")))))
    name))

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


(defun schema-compile (package kb)
  (let ((compinfo (make-compiler-info)))
    (cl-kb:with-kb kb nil
                   (append (schema-compile-header package kb)
                           (let ((cls-list))
                             (cl-kb:cls-do-subcls-list (cl-kb:find-cls '|dataset|::|DatasetThing|)
                                                       el
                                                       (if (eq kb (cl-kb:frame-kb el))
                                                           (push el cls-list))) 
                             (schema-compile-clses package cls-list compinfo))
                           (schema-compile-trailer package)))))
  
(defstruct compiler-info
  (enum-types nil)
  (symbols nil))

;
; compile header/trailer
;

(defun schema-compile-header (package kb )
  (let ((codes nil))
    (push `(in-package ,(format nil "~A" (package-name package))) codes)
    (dolist (ukb (cl-kb:kb-use-list kb))
      (if (member (cl-kb:find-kb 'cl-kbs::|dataset|) (cl-kb:kb-use-list ukb))
          (push `(use-package ,(format nil "~A-ws" (package-name (cl-kb:kb-package ukb)))) codes)))
    codes))

(defun schema-compile-trailer (package)
  (list `(format t "!! loaded ~A !!~%" ,(package-name package))))


;
; compile clses
;

(defun schema-compile-clses (package cls-list compinfo)
  (let ((codes nil))
    (dolist (cls cls-list)
      (push (schema-compile-cls package cls compinfo) codes))
    (dolist (typ (compiler-info-enum-types compinfo))
      (schema-compile-enum typ))
    (append codes
            (list
             `(export ',(mapcar #'(lambda (x) x) (compiler-info-symbols compinfo))
                      (find-package ,(package-name package)))))))
           
(defun schema-compile-cls (package cls compinfo)
  (let ((framesymb (cl-kb:frame->symbol cls package t)))  
    (push framesymb (compiler-info-symbols compinfo))
    `(defclass 
       ,framesymb
       ,(mapcar #'(lambda (s) (cl-kb:frame->symbol s package))
                                   (cl-kb:cls-direct-superclses cls))
       ,(mapcar #'(lambda (slot) 
                    (push (cl-kb:frame->symbol slot package) (compiler-info-symbols compinfo))
                    (schema-compile-slot package slot compinfo))
                (cl-kb:cls-direct-template-slots cls)))))

(defun schema-compile-slot (package slot compinfo)
  (let ((slotsymb (cl-kb:frame->symbol slot package t)))
    `(,slotsymb :accessor ,slotsymb :type ,(schema-compile-slot-type slot compinfo))))
                
(defun schema-compile-slot-type (slot compinfo)
  (if (eq (cl-kb:slot-maximum-cardinality slot) 1)
      (let ((typ (cl-kb:slot-value-type slot)))
        (cond 
         ((eq typ 'cl-kb:integer-type-value)
          'integer)
         ((eq typ 'cl-kb:float-type-value)
          'float)
         ((eq typ 'cl-kb:string-type-value)
          'string)
         ((eq typ 'cl-kb:boolean-type-value)
          'boolean)
         ((eq typ 'cl-kb:symbol-type-value)
          (push slot (compiler-info-enum-types compinfo))
          'string)
         ((eq typ 'cl-kb:instance-type-value)
          t)))
      'list))
        

;
; enum
;

(defun schema-compile-enum (slot)
  (declare (ignore slot))
  nil)

  

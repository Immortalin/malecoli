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

;;;; Created on 2008-09-10 11:08:30

(in-package :mlcl-dataset)

(defclass dataset-schema ()
  ((name 
    :READER dataset-schema-name
    :INITARG :name
    :INITFORM nil
    :TYPE string)
   (pathname 
    :READER dataset-schema-pathname
    :INITARG :pathname
    :TYPE pathname)
   (package
    :READER dataset-schema-package
    :INITARG :package
    :TYPE package)
   (kb
    :TYPE kb
    :READER dataset-schema-kb)))

(defmethod initialize-instance :after ((schema dataset-schema) &rest initargs)
  (declare (ignore initargs))
  (if (null (dataset-schema-name schema))
      (setf (slot-value schema 'name) (pathname-name (dataset-schema-pathname schema)))) 
  (setf (slot-value schema 'package) (or 
                                      (find-package (format nil "~A-ds" (dataset-schema-name schema))) 
                                      (make-package (format nil "~A-ds" (dataset-schema-name schema)) 
                                                    :use '(:cl :mlcl-kb :mlcl-dataset))))
  (setf (slot-value schema 'kb) (or (mlcl-kb:find-kb (dataset-schema-name schema) nil)
                                     (mlcl-kb:make-kb 
                                      (dataset-schema-name schema)
                                      :use-list (list 'mlcl-kbs::dataset-kb 'mlcl-kbs::protege-kb)
                                      :protege-file (merge-pathnames
                                                     (make-pathname :type "pprj")
                                                     (dataset-schema-pathname schema)))))
  (dataset-schema-load schema))

(defun dataset-schema-source-list-file (schema)
  (merge-pathnames
   (make-pathname :type "lisp")
   (dataset-schema-pathname schema)))

(defun dataset-schema-compiled-list-file (schema)
  (merge-pathnames
   (make-pathname :type nil)
   (dataset-schema-pathname schema)))

(defun dataset-schema-xml-kb-file (schema)
  (merge-pathnames
   (make-pathname :type "xml")
   (dataset-schema-pathname schema)))

(defun dataset-schema-load (schema)
  (let ((lispfile (dataset-schema-source-list-file schema)))
    (if (or (not (probe-file lispfile)) (< (file-write-date lispfile) (file-write-date (dataset-schema-xml-kb-file schema))))
        (progn
          (dataset-schema-generate-lisp-file schema)
          (compile-file lispfile)
          (load (dataset-schema-compiled-list-file schema))
          (funcall (find-symbol "INIT-DATASET" (dataset-schema-package schema))))
        (progn
          (load (dataset-schema-compiled-list-file schema))))))

(defun dataset-schema-reload (schema)
  (dataset-schema-load schema))

(defun dataset-schema-generate-lisp-file (schema)
  (mlcl-kb:kb-open (dataset-schema-kb schema))
  (with-open-file (strm (dataset-schema-source-list-file schema) :direction :output :if-exists :supersede)
                  (dataset-kb-compile (dataset-schema-package schema) (dataset-schema-kb schema) strm))
  (mlcl-kb:kb-close (dataset-schema-kb schema)))


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
  (if (eq (slot-maximum-cardinality slot) 1)
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
  (format strm "~%~%"))

  
;
; conversion functions
;

(defun frame->lisp-name (fr)
  (let ((name (mlcl-kb:frame-name fr)))
    (if (eq (mlcl-kb:frame-kb fr) (symbol-value 'mlcl-kbs::dataset-kb))
        (progn
          (cond
           ((eq fr (symbol-value 'dataset-kb::|DatasetCase|))
              (setf name "DATASET-CASE"))
           ((eq fr (symbol-value 'dataset-kb::|DatasetThing|))
              (setf name "DATASET-THING"))))
        (progn
          (setf name (cl-ppcre:regex-replace-all " " name "_"))
          (setf name (cl-ppcre:regex-replace-all "\\?" name "p"))
          (setf name (cl-ppcre:regex-replace-all "\\." name "_"))))
    name))

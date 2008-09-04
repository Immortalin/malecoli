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

;;;; Created on 2008-09-04 10:45:22

(in-package :mlcl-dataset)

(defun dataset-generate-lisp-file (name pathname kb)
  (let ((lispfile (merge-pathnames
                   (make-pathname :type "lisp")
                   pathname)))
    (with-open-file (strm lispfile :direction :output :if-exists :supersede)
                    (format strm ";;;; Created on ~A~%~%" (get-universal-time))
                    (format strm "(defpackage :|~A-ds| (:use :cl :mlcl-kb :mlcl-dataset :clsql))~%~%" name)
                    (format strm "(in-package :|~A-ds|)~%~%" name)
                    (dataset-generate-lisp-file-from-kb strm kb)
                    (format strm "~%~%")
                    (format strm ";;;; Created on ~A~%" (get-universal-time))
                    )))
        
(defun dataset-generate-lisp-file-from-kb (strm kb)
  (mlcl-kb:kb-open kb)
  (dolist (el (mlcl-kb:kb-interned-elements kb))
    (if (and (typep el 'mlcl-kb:cls) 
             (mlcl-kb:cls-has-supercls el 'dataset-kb::|DatasetThing|))
        (dataset-generate-cls strm kb el)))
  (dolist (el (mlcl-kb:kb-interned-elements kb))
    (if (and (typep el 'mlcl-kb:simple-instance) 
             (mlcl-kb:instance-has-type el 'dataset-kb::|DatasetThing|))
        (dataset-generate-instance strm kb el)))
  (mlcl-kb:kb-close kb))
                                       
(defun dataset-generate-cls (strm kb cls)
  (declare (ignore kb))
  (format strm ";;;; Generation of cls ~A~%~%" (mlcl-kb:frame-name cls))
  (format strm "(def-view-class |~A| (~{|~A| ~}) (" (mlcl-kb:frame-name cls) 
          (mapcar #'(lambda (s) (frame-name s))
                  (mlcl-kb:cls-direct-superclses cls)))
  (dolist (slot (mlcl-kb:cls-direct-template-slots cls))
    (format strm "~%	(|~A|~%	 :type ~A~%	 :accessor ~A)" (mlcl-kb:frame-name slot) 
            (let ((typ (mlcl-kb:slot-value-type slot)))
              (cond 
               ((eq typ 'protege-kb::integer-type-value)
                "integer")
               ((eq typ 'protege-kb::symbol-type-value)
                "string")))
            (format nil "|~A|" (mlcl-kb:frame-name slot))))
  (format strm ")~%	(:base-table |~A|))~%" (frame-name cls))
  (format strm "~%~%")
  (format strm "(defun init-dataset () (clsql:create-view-from-class '|~A|))~%" (mlcl-kb:frame-name cls))
  (format strm "~%~%"))

(defun dataset-generate-instance (strm kb inst)
  (declare (ignore kb))
  (format strm ";;;; Generation of instances ~A~%~%" (mlcl-kb:frame-name inst))
  (format strm "(let ((v (make-instance '|~A| :name \"~A\"))))" (mlcl-kb:frame-name (mlcl-kb:instance-direct-type inst)) (mlcl-kb:frame-name inst))
  (format strm "~%~%"))

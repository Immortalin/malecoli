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

;;;; Created on 2008-09-01 09:29:03

(in-package :mlcl-dataset)

(defclass dataset ()
  ((name 
    :READER dataset-name
    :INITARG :name
    :TYPE string)
   (pathname 
    :READER dataset-pathname
    :INITARG :pathname
    :TYPE pathname)
   (kb
    :TYPE kb
    :READER dataset-kb)
   (sqldatabase
    :TYPE clsql:database
    :READER database-sqldatabase)))


(defmethod initialize-instance :after ((dataset dataset) &rest initargs)
  (declare (ignore initargs))
  (setf (slot-value dataset 'kb) (or (mlcl-kb:find-kb (dataset-name dataset))
                                     (mlcl-kb:make-kb 
                                      (dataset-name dataset)
                                      :use-list (list 'mlcl-kbs::dataset-kb)
                                      :protege-file (merge-pathnames
                                                     (make-pathname :type "pprj")
                                                     (dataset-pathname dataset)))))
  (setf (slot-value dataset 'sqldatabase) 
        (clsql:connect (list (format nil "~A" (merge-pathnames
                                               (make-pathname :type "sqlite3")
                                               (dataset-pathname dataset))))
                       :database-type :sqlite3))
  (let ((lispfile (merge-pathnames
                   (make-pathname :type "lisp")
                   (dataset-pathname dataset))))
    (if (not (probe-file lispfile))
        (dataset-generate-lisp-file dataset)
        (dataset-generate-lisp-file dataset))
    (compile-file lispfile)
    (load lispfile)))
 
(defun make-dataset (name pathname)
  (make-instance 'dataset :name name :pathname pathname))
     
(defun dataset-generate-lisp-file (dataset)
  (let ((lispfile (merge-pathnames
                   (make-pathname :type "lisp")
                   (dataset-pathname dataset))))
    (with-open-file (strm lispfile :direction :output :if-exists :supersede)
                    (format strm ";;;; Created on ~A~%~%" (get-universal-time))
                    (format strm "(defpackage :~A-ds (:use :cl :mlcl-kb :mlcl-dataset :clsql))~%~%" (dataset-name dataset))
                    (format strm "(in-package :~A-ds)~%~%" (dataset-name dataset))
                    (dataset-generate-lisp-file-from-kb strm (dataset-kb dataset))
                    (format strm "~%~%")
                    (format strm ";;;; Created on ~A~%" (get-universal-time))
                    )))
        
           

(defun dataset-generate-lisp-file-from-kb (strm kb)
  (mlcl-kb:kb-open kb)
  (dolist (el (mlcl-kb:kb-interned-elements kb))
    (if (and (typep el 'mlcl-kb:cls) 
             (mlcl-kb:cls-has-supercls el 'dataset-kb::|DatasetThing|))
        (dataset-generate-cls strm kb el)))
  (mlcl-kb:kb-close kb))
                                       
(defun dataset-generate-cls (strm kb cls)
  (format strm ";;;; Generation of cls ~A~%~%" (frame-name cls))
  (format strm "(def-view-class |~A| (~{|~A| ~}) (~%" (frame-name cls) 
          (mapcar #'(lambda (s) (frame-name s))
                  (mlcl-kb:cls-direct-superclses cls)))
  (dolist (slot (mlcl-kb:cls-direct-template-slots cls))
    (format strm "	(|~A|~%	 :type ~A)~%" (mlcl-kb:frame-name slot) 
            (let ((typ (mlcl-kb:slot-value-type slot)))
              (cond 
               ((eq typ 'protege-kb::integer-type-value)
                "integer")
               ((eq typ 'protege-kb::symbol-type-value)
                "string")))))
  (format strm "))~%")
  (format strm "~%~%"))



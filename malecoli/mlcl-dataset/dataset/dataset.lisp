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
                                      :use-list (list 'mlcl-kbs::dataset-kb 'mlcl-kbs::protege-kb)
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
        (progn
          (dataset-generate-lisp-file (dataset-name dataset) (dataset-pathname dataset) (dataset-kb dataset))
          (compile-file lispfile)
          (load (merge-pathnames
                 (make-pathname :type nil)
                 (dataset-pathname dataset)))
          (clsql:disconnect)    
          (clsql:destroy-database (list (format nil "~A" (merge-pathnames
                                               (make-pathname :type "sqlite3")
                                               (dataset-pathname dataset))))
                       :database-type :sqlite3)
          (setf (slot-value dataset 'sqldatabase) 
                (clsql:connect (list (format nil "~A" (merge-pathnames
                                                       (make-pathname :type "sqlite3")
                                                       (dataset-pathname dataset))))
                               :database-type :sqlite3))
          (funcall (find-symbol "INIT-DATASET" (find-package (format nil "~A-ds" (dataset-name dataset))))))
        (progn
          (load (merge-pathnames
                 (make-pathname :type nil)
                 (dataset-pathname dataset)))))))
        
(defun make-dataset (name pathname)
  (make-instance 'dataset :name name :pathname pathname))

(defun dataset-import-data (dataset pathname)
  (let ((kb (or (mlcl-kb:find-kb (pathname-name pathname))
                (mlcl-kb:make-kb (pathname-name pathname) 
                             :use-list (list 'mlcl-kbs::dataset-kb 'mlcl-kbs::protege-kb (dataset-kb dataset))
                             :protege-file pathname))))
    (mlcl-kb:kb-open kb)
    (dataset-generate-lisp-file (dataset-name dataset) pathname kb)))

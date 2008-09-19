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

;;;; Created on 2008-09-11 10:44:00

(in-package :mlcl)

(defclass workspace ()
  ((file 
    :READER workspace-file
    :INITARG :file
    :TYPE pathname)
   (storage
    :type storage
    :initarg :storage
    :initform nil
    :reader workspace-storage)
   (schema
    :READER workspace-schema
    :TYPE schema
    :INITARG :schema
    :INITFORM nil)
   (datasets 
    :READER workspace-datasets
    :TYPE list
    :INITFORM nil)))
  
(defmethod initialize-instance :after ((workspace workspace) &rest initargs)
  (declare (ignore initargs))
  (if (null (workspace-schema workspace))
      (setf (slot-value workspace 'schema) (make-instance 'schema :file (merge-pathnames
                                                                         (make-pathname :type "pprj")
                                                                         (workspace-file workspace)))))
  (if (null (workspace-storage workspace))
      (setf (slot-value workspace 'storage) (make-instance 'storage :file 
                                                             (merge-pathnames
                                                              (make-pathname :type "stor")
                                                              (workspace-file workspace)))))
  (workspace-load workspace))

(defun workspace-load (workspace)
  (let ((storefile (workspace-file workspace)))
    (if (probe-file storefile)
        (let ((datasetes-list (cl-store:restore storefile)))
          (dolist (cases-list datasetes-list)
            (let ((dataset (workspace-make-dataset workspace (car cases-list))))
              (dolist (c (cdr cases-list))
                (push  (aref (storage-cases (workspace-storage workspace)) c)
                       (slot-value dataset 'cases)))))))))

(defun workspace-save (workspace)
  (let ((storefile (workspace-file workspace))
        (datasetes-list nil))
    (dolist (ds (workspace-datasets workspace))
      (if (not (dataset-temporaryp ds))
          (push (cons (dataset-name ds)
                      (mapcan #'(lambda (x) (if x (list x))) (mapcar #'dataset-case-id (dataset-cases ds))))
                datasetes-list)))
    (cl-store:store datasetes-list storefile)))

(defun workspace-make-dataset (workspace name)
  (let ((dataset (make-instance 'dataset 
                                :name name 
                                :schema (workspace-schema workspace)
                                :storage (workspace-storage workspace))))
    (push dataset (slot-value workspace 'datasets))
    dataset))

(defun workspace-make-temporary-dataset (workspace name)
  (let ((dataset (make-instance 'dataset 
                                :name name 
                                :schema (workspace-schema workspace))))
    (push dataset (slot-value workspace 'datasets))
    dataset))


(defun workspace-case-import (workspace cas)
  (dataset-kb-case-import (schema-package (workspace-schema workspace)) cas))

(defun workspace-cases-import (workspace kb)
  (multiple-value-bind (cases datasets-list)
    (dataset-kb-import (schema-package (workspace-schema workspace)) kb)
    (storage-add-cases (workspace-storage workspace) cases)
    (dolist (cases-list datasets-list)
      (let ((dataset (workspace-make-dataset workspace (car cases-list))))
        (dolist (c (cdr cases-list))
            (push c (slot-value dataset 'cases)))))
    (storage-save (workspace-storage workspace))
    (workspace-save workspace)))


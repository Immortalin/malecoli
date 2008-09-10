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
    :INITFORM nil
    :TYPE string)
   (pathname 
    :READER dataset-pathname
    :INITARG :pathname
    :TYPE pathname)
   (schema
    :READER dataset-schema
    :TYPE dataset-schema
    :INITARG :schema)
   (cases 
    :TYPE list
    :INITFORM nil
    :ACCESSOR dataset-cases)))

(defmethod initialize-instance :after ((ds dataset) &rest initargs)
  (declare (ignore initargs))
  (if (null (dataset-name ds))
      (setf (slot-value ds 'name) (pathname-name (dataset-pathname ds)))) 
  (dataset-load ds))

(defun dataset-store-file (dataset)
  (merge-pathnames
   (make-pathname :type "store")
   (dataset-pathname dataset)))

(defun dataset-load (dataset)
  (let ((storefile (dataset-store-file dataset)))
    (if (probe-file storefile)
        (setf (dataset-cases dataset) (cl-store:restore storefile)))))

(defun dataset-save (dataset)
  (let ((storefile (dataset-store-file dataset)))
    (cl-store:store (dataset-cases dataset) storefile)))

(defun dataset-import (dataset kb)
  (kb-load dataset kb)
  (dataset-save dataset))


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

(in-package :mlcl)

(defclass storage ()
  ((file 
    :READER storage-file
    :INITARG :file
    :TYPE pathname)
   (cases 
    :TYPE list
    :INITFORM (make-array 0 :adjustable t :fill-pointer t)
    :READER storage-cases)))

(defmethod initialize-instance :after ((storage storage) &rest initargs)
  (declare (ignore initargs))
  (storage-load storage))

(defun storage-load (storage)
  (let ((storefile (storage-file storage)))
    (if (probe-file storefile)
        (setf (slot-value storage 'cases) (cl-store:restore storefile)))))

(defun storage-save (storage)
  (let ((storefile (storage-file storage)))
    (cl-store:store (slot-value storage 'cases) storefile)))

(defun storage-add-case (storage c)
  (let ((pos (vector-push-extend c (slot-value storage 'cases))))
    (setf (slot-value c 'id) pos)))

(defun storage-add-cases (storage cases)
  (dolist (c cases)
    (storage-add-case storage c)))

(defun storage-remove (storage c)
  (let ((cas (vector-pop (slot-value storage 'cases))))
    (if (< (dataset-case-id c) (length (storage-cases storage)))
        (progn
          (setf (aref (dataset-case-id c) (slot-value storage 'cases)) cas)
          (setf (slot-value cas 'id) (dataset-case-id c))))
    (setf (slot-value c 'id) nil)))

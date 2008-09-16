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
   (schema
    :READER dataset-schema
    :TYPE schema
    :INITARG :schema)
   (storage 
    :READER dataset-storage
    :TYPE storage
    :INITFORM nil
    :INITARG :storage)
   (cases 
    :TYPE list
    :INITFORM nil
    :READER dataset-cases)))

(defun dataset-temporaryp (dataset)
  (null (dataset-storage dataset)))

(defun dataset-add-case (dataset cas)
  (if (and (null (dataset-case-id cas)) (not (dataset-temporaryp dataset)))
      (storage-add-case (dataset-storage dataset) cas))
  (push cas (slot-value dataset 'cases)))

(defun dataset-import-case-from-kb (dataset si-cas)
  (let ((c (dataset-kb-import-simple-instance (dataset-schema dataset) si-cas)))
    (dataset-add-case dataset c)))

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

;
; workspace
;

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
    :INITFORM nil)
   (algorithms 
    :READER workspace-algorithms
    :TYPE list
    :INITFORM nil)
   (makefiles 
    :READER workspace-makefiles
    :TYPE list
    :INITFORM nil)
   (variables
    :READER workspace-variables
    :TYPE list
    :INITFORM nil)))
  
; initialize
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

; i/o 

(defun workspace-load (workspace)
  (let ((storefile (workspace-file workspace)))
    (if (probe-file storefile)
        (with-open-file (strm storefile :direction :input :element-type '(unsigned-byte 8))
                        (let ((*clstore-schema* (workspace-schema workspace))
                              (*clstore-storage* (workspace-storage workspace)))
                          (setf (slot-value workspace 'datasets) (cl-store:restore strm))
                          (setf (slot-value workspace 'algorithms) (cl-store:restore strm))
                          (dolist (mk (cl-store:restore strm))
                            (workspace-add-makefile workspace mk))
                          (dolist (var (cl-store:restore strm))
                            (workspace-add-variable workspace (car var) (cdr var))))))))
                        
(defun workspace-save (workspace)
  (let ((storefile (workspace-file workspace)))
    (with-open-file (strm storefile :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
                    (cl-store:store (workspace-datasets workspace) strm)
                    (cl-store:store (workspace-algorithms workspace) strm)
                    (cl-store:store (mapcar #'(lambda(x) (makefile-algorithms-pprj-file x))
                                            (workspace-makefiles workspace)) strm)
                    (cl-store:store (mapcar #'(lambda(x) (cons (symbol-name x) (symbol-value x)))
                                            (workspace-variables workspace)) strm))))

; datasets
(defun workspace-find-dataset (workspace dadasetname)
  (find-if #'(lambda (x) (string-equal (dataset-name x) dadasetname))
           (workspace-datasets workspace)))

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

; case importing
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

; workspace algorithms
(defun workspace-add-algorithm (workspace algo)
  (push algo (slot-value workspace 'algorithms)))

(defun workspace-find-algorithm (workspace algoname)
  (find-if #'(lambda (x) (string-equal (algorithm-name x) algoname))
           (workspace-algorithms workspace)))

(defun workspace-make-algorithms (workspace makefile)
  (if (typep makefile 'pathname)
      (setf makefile (make-instance 'makefile 
                                    :algorithms-file makefile
                                    :schema (workspace-schema workspace))))
  (push makefile (slot-value workspace 'makefiles))
  (dolist (algo (makefile-make makefile))
    (push algo (slot-value workspace 'algorithms))))

(defun workspace-add-makefile (workspace makefile)
  (if (typep makefile 'pathname)
      (setf makefile (make-instance 'makefile 
                                    :algorithms-file makefile
                                    :schema (workspace-schema workspace))))
  (push makefile (slot-value workspace 'makefiles)))

; variables
(defun workspace-add-variable (workspace name &optional (val nil))
  (let* ((package (schema-package (workspace-schema workspace)))
         (symb (cl-kb:string->symbol name package)))
    (export symb package)
    (if (or val (not (boundp symb))) (setf (symbol-value symb) val))
    (push symb (slot-value workspace 'variables))))

(defun workspace-remove-variable (workspace symb)
    (setf (slot-value workspace 'variables) (delete symb (slot-value workspace 'variables))))

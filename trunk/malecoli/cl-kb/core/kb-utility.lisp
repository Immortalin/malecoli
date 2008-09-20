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

;;;; Created on 2008-04-30 13:03:56

(in-package :cl-kb)

;
; make an instance of a cls, slot, facet, and a simple-instance
;

(defun make-system-cls (name &key (kb *kb*))
  (check-type name string)
  (check-type kb kb)
  (make-instance 'cls :name name :definedp nil :systemp t :kb kb))

(defun make-system-slot (name &key (kb *kb*))
  (check-type name string)
  (check-type kb kb)  
  (make-instance 'slot :name name :definedp nil :systemp t :kb kb))

(defun make-system-facet (name &key (kb *kb*))
  (check-type name string)
  (check-type kb kb)
  (make-instance 'facet :name name :definedp nil :systemp t :kb kb))

(defun make-system-simple-instance (name &key (kb *kb*))
  (check-type name string)
  (check-type kb kb)
  (make-instance 'simple-instance :name name :definedp nil :systemp t :kb kb))


;
; get a frame from a kb
;

(defun get-cls (name &key (kb *kb*))
  (let ((it (element-name->element name kb)))
    (if (null it)
        (values (make-instance 'cls :name name :definedp nil :kb kb) t)
        (values it nil))))

(defun get-slot (name &key (kb *kb*))
  (let ((it (element-name->element name kb)))
    (if (null it)
        (values (make-instance 'slot :name name :definedp nil :kb kb) t)
        (values it nil))))

(defun get-facet (name &key (kb *kb*))
  (let ((it (element-name->element name kb)))
    (if (null it)
        (values (make-instance 'facet :name name :definedp nil :kb kb) t)
        (values it nil))))

(defun get-simple-instance (name &key (kb *kb*))
  (let ((it (element-name->element name kb)))
    (if (null it)
        (values (make-instance 'simple-instance :name name :definedp nil :kb kb) t)
        (values it nil))))


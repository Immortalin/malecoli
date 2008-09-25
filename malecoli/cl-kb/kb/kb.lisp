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

;;;; Created on 2008-09-25 17:19:38

(in-package :cl-kb)

;
; kb
;

; initialize
(defmethod kb-import-from-protege-pprj% ((kb kb))
  (if (kb-createdp kb)
      (kb-import-from-protege-pprj (kb-protege-pprj-file kb) kb)))

;
; protege create
;

(defun kb-create (&optional (kb *kb*))
  (check-type kb kb)
  (if (not (kb-openedp kb))
      (progn
        (dolist (ukb (kb-use-list kb))
          (kb-open ukb))
        (if (null (kb-protege-xml-file kb))
            (setf (kb-protege-xml-file kb) (merge-pathnames 
                                            (make-pathname :type "xml")
                                            (kb-protege-pprj-file kb))))
        (kb-export-to-protege-file (kb-protege-pprj-file kb) (kb-protege-xml-file kb) kb t t)
        (setf (kb-openedp kb) t))))

(defun kb-createdp (&optional (kb *kb*))
  (check-type kb kb)
  (probe-file (kb-protege-pprj-file kb)))

;
; protege save
;

(defun kb-save (&optional (kb *kb*))
  (check-type kb kb)
  (kb-export-to-protege-file (kb-protege-pprj-file kb) (kb-protege-xml-file kb) kb t t))


;
; open/close
;

(defun kb-open (&optional (kb *kb*))
  (check-type kb kb)  
  (if (not (kb-openedp kb))
      (progn
        (dolist (ukb (kb-use-list kb))
          (kb-open ukb))
        (kb-import-from-protege-file (kb-protege-pprj-file kb) (kb-protege-xml-file kb) kb)
        (setf (kb-openedp kb) t))))

(defun kb-close (&optional (kb *kb*))
  (check-type kb kb)
  (if (kb-openedp kb)
      (progn
        (kb-export-to-protege-file (kb-protege-pprj-file kb) (kb-protege-xml-file kb) kb nil nil)
        (kb-clear kb)
        (setf (kb-openedp kb) nil))))


;
;
;


(defmacro cls-do-subcls-list (cls subcls &rest body &key (kb *kb*))
  `(cls-do-subcls-list% ,cls ,subcls 
                        (if (frame-in-kb ,subcls ,kb) 
                            (progn 
                              ,@body))))

(defmacro cls-do-instance-list (cls inst &rest body &key (kb *kb*))
 `(cls-do-instance-list% ,cls ,inst 
                        (if (frame-in-kb ,inst ,kb) 
                            (progn 
                              ,@body))))


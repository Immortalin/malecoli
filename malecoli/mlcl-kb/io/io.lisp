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

;;;; Created on 2008-09-02 11:34:54

(in-package :mlcl-kb)


;
; kb
;

; initialize
(defmethod initialize-instance :after ((kb kb) &rest initargs)
  (declare (ignore initargs))
  (if (find-kb (kb-protege-pprj-file kb) nil)
      (error "Kb ~s already exists." (kb-protege-pprj-file kb)))
  (if (find-package (kb-name kb))
      (error "Package named ~s already exists." (kb-name kb)))
  (setf (slot-value kb 'package) (make-package (kb-name kb) :use nil))
  (let ((ul (kb-use-list kb)))
    (setf (slot-value kb 'use-list) nil)
    (dolist (u ul) (use-kb u kb)))
  (let ((kbsym (intern (kb-name kb) (find-package :mlcl-kbs))))
    (setf (symbol-value kbsym) kb))
  (kb-import-from-protege-pprj (kb-protege-pprj-file kb) kb)
  (push kb *all-kbs*))

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
  (kb-export-to-protege-file (kb-protege-pprj-file kb) (kb-protege-xml-file kb) kb t nil))


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
; import/export functions
;
                        
(defun kb-import-from-protege-file (pprj-file xml-file &optional (kb *kb*))
;  (check-type pprj-file (or nil pathname))
;  (check-type xml-file (or nil pathname))
  (check-type kb kb)
  (if pprj-file
      (progn
        (kb-import-from-protege-pprj pprj-file kb)
        (if (and (kb-protege-xml-file kb) (not (equal (kb-protege-xml-file kb) xml-file)))
            (kb-import-from-protege-xml (kb-protege-xml-file kb) kb))))
  (if xml-file
      (kb-import-from-protege-xml xml-file kb)))

(defun kb-export-to-protege-file (pprj-file xml-file &optional (kb *kb*) (xml-supersedep t) (pprj-supersedep nil))
  (check-type pprj-file pathname)
  (check-type xml-file pathname)
  (check-type kb kb)
  (kb-export-to-protege-pprj pprj-file
                             xml-file
                             kb :supersedep pprj-supersedep)
  (kb-export-to-protege-xml xml-file 
                            kb :supersedep xml-supersedep))


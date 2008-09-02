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
; protege create
;

(defun kb-create (&optional (kb *kb*))
  (check-type kb kb)
  (if (kb-protege-file kb)
      (kb-export-to-protege (kb-protege-file kb) kb t t)
      (error "Protege file does not set for kb ~A." (kb-name kb))))

(defun kb-createdp (&optional (kb *kb*))
  (check-type kb kb)
  (if (kb-protege-file kb)
      (and 
       (probe-file (merge-pathnames
                    (make-pathname :type "pprj")
                    (kb-protege-file kb)))
       (probe-file (merge-pathnames
                    (make-pathname :type "xml")
                    (kb-protege-file kb))))
      (error "Protege file does not set for kb ~A." (kb-name kb))))


;
; protege save
;

(defun kb-save (&optional (kb *kb*))
  (check-type kb kb)
  (if (kb-protege-file kb)
      (kb-export-to-protege (kb-protege-file kb) kb t nil)
      (error "Protege file does not set for kb ~A." (kb-name kb))))


;
; open/close
;

(defun kb-open (&optional (kb *kb*))
  (check-type kb kb)  
  (if (not (kb-openedp kb))
      (progn
        (dolist (ukb (kb-use-list kb))
          (kb-open ukb))
        (if (kb-protege-file kb) 
            (kb-import-from-protege (kb-protege-file kb) kb))
        (setf (kb-openedp kb) t))))

(defun kb-close (&optional (kb *kb*))
  (check-type kb kb)
  (if (kb-openedp kb)
      (progn
        (if (kb-protege-file kb) 
            (kb-export-to-protege (kb-protege-file kb) kb nil nil))
        (kb-clear kb)
        (setf (kb-openedp kb) nil))))


;
; import/export functions
;
                        
(defun kb-import-from-protege (pathname &optional (kb *kb*))
  (check-type pathname pathname)
  (check-type kb kb)
  (kb-import-from-protege-pprj (merge-pathnames
                              (make-pathname :type "pprj")
                              pathname) kb)
  (kb-import-from-protege-xml (merge-pathnames
                              (make-pathname :type "xml")
                              pathname) kb))

(defun kb-export-to-protege (pathname &optional (kb *kb*) (xml-supersedep t) (pprj-supersedep nil))
  (check-type pathname pathname)
  (check-type kb kb)
  (kb-export-to-protege-pprj (merge-pathnames
                              (make-pathname :type "pprj")
                              pathname) 
                             (merge-pathnames
                              (make-pathname :type "xml")
                              pathname)
                             kb :supersedep pprj-supersedep)
  (kb-export-to-protege-xml (merge-pathnames
                              (make-pathname :type "xml")
                              pathname) kb :supersedep xml-supersedep))

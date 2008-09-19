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

;;;; Created on 2008-04-23 15:28:45

(in-package :mlcl-kb)

;
; A generic element of a kb
;

(defclass kb-element ()
  ((name
    :TYPE string
    :READER kb-element-name
    :INITARG :name
    :documentation "the name of the element"))
  (:documentation "A generic element of a kb."))


;
; global variables
;

(defvar *kb* nil 
  "default kb")

(defvar *all-kbs* nil
  "list of all kbs")


;
; knowledge base
;

(defclass kb ()
  ((protege-pprj-file
    :TYPE (or nil pathname)
    :INITARG :protege-pprj-file
    :INITFORM nil
    :ACCESSOR kb-protege-pprj-file)
   (protege-xml-file
    :TYPE (or nil pathname)
    :INITARG :protege-xml-file
    :INITFORM nil
    :ACCESSOR kb-protege-xml-file)
   (package
    :READER kb-package
    :INITARG :package
    :TYPE package)
   (interned-elements
    :READER kb-interned-elements
    :INITFORM nil
    :TYPE list)
   (use-list
    :READER kb-use-list
    :INITARG :use
    :INITFORM nil)
   (openedp
    :INITFORM nil
    :ACCESSOR kb-openedp))
  (:documentation "A kb"))

(defun kb-name (kb)
  (check-type kb kb)
  (pathname-name (kb-protege-pprj-file kb)))

; kb file

(defvar *kb-paths* nil)
(defvar *kb-default-path* nil)

(deftype kb-file-designator ()
  `(or string pathname))

(defun find-kb-file (file-des &optional (errorp t))
  (check-type file-des kb-file-designator)
  (etypecase file-des
             (string 
              (let ((path (make-pathname :name file-des :type "pprj")))
                (if (probe-file path) 
                    path
                    (progn 
                      (setf path (do ((paths *kb-paths* (cdr paths))
                                      (p nil))
                                     ((or (null paths) (and p (probe-file p))) (and (probe-file p) p))
                                   (setf p (merge-pathnames 
                                            (make-pathname :name file-des :type "pprj")
                                            (car paths)))))
                      (if path
                          path
                          (if errorp (error "File designed by ~S does not exist." file-des) nil))))))
             (pathname
              (if (probe-file file-des) 
                  file-des
                  (if errorp (error "File designed by ~S does not exist." file-des) nil)))))
  
  

; kb designator
(deftype kb-designator ()
  `(or kb string symbol pathname))

(defun find-kb (kb-des &optional (errorp t) (importp nil))
  (check-type kb-des kb-designator)
  "Return kb designed by NAME. If there is no such kb NIL is returned
if ERRORP is false, otherwise an error is signalled."
  (etypecase kb-des
             (kb kb-des)
             (symbol
              (if (and (boundp kb-des) (typep (symbol-value kb-des) 'kb))
                  (symbol-value kb-des)
                  (if errorp (error "Kb designed by ~S does not exist." kb-des) nil)))
             (string 
              (or (find-if #'(lambda (x) (string= (kb-name x) kb-des)) *all-kbs*)
                  (if errorp (error "Kb designed by ~S does not exist." kb-des) nil)))
             (pathname
              (let ((path (find-kb-file kb-des nil)))
                (if path
                    (let ((kb (find-kb (pathname-name path) nil)))
                      (if (and (null kb) importp)
                          (setf kb (make-instance 'kb :protege-pprj-file path)))
                      (if kb 
                          kb
                          (if errorp (error "Kb designed by ~S does not exist." kb-des) nil)) )
                    (if errorp (error "Kb designed by ~S does not exist." kb-des) nil))))))

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
  (if (not (string-equal (kb-name kb) "protege")) (use-kb (find-kb "protege") kb))
  (let ((kbsym (intern (kb-name kb) (find-package :mlcl-kbs))))
    (setf (symbol-value kbsym) kb))
  (kb-import-from-protege-pprj% kb)
  (push kb *all-kbs*))

(defgeneric kb-import-from-protege-pprj% (kb))

; make, delete, and clear
(defun make-kb (protege-pprj-file &key (use nil))
  "make a new kb"
  (check-type protege-pprj-file kb-file-designator)
  (if (find-kb protege-pprj-file nil)
      (error "Kb  ~s already exists." protege-pprj-file))
  (if (typep protege-pprj-file 'string)
      (setf protege-pprj-file 
            (merge-pathnames 
             (pathname protege-pprj-file)
             *kb-default-path*)))
  (make-instance 'kb :protege-pprj-file protege-pprj-file :use use))

(defun delete-kb (kb-des)
  (check-type kb-des kb-designator)
  (let ((kb (find-kb kb-des)))
    (kb-clear kb)
    (let ((kbsym (find-symbol (kb-name kb) (find-package :mlcl-kbs))))
      (unintern kbsym))
    (delete-package (kb-package kb))))

(defun kb-clear (kb-des)
  (check-type kb-des kb-designator)
  (let ((kb (find-kb kb-des)))
    (dolist (el (kb-interned-elements kb))
      (let ((it (element-name->symbol (kb-element-name el) kb)))
        (if it
            (progn
              (setf (symbol-value it) nil)
              (unexport it (slot-value kb 'package))
              (unintern it (slot-value kb 'package))))))
    (setf (slot-value kb 'interned-elements) nil)))

; using other kbs
(defun use-kb (kb-to-use-des &optional (kb-des *kb*))
  (check-type kb-des kb-designator)
  (let ((kb (find-kb kb-des))
        (kb-to-use (find-kb kb-to-use-des)))
    (if (not (member kb-to-use (kb-use-list kb)))
        (progn 
          (dolist (u (kb-use-list kb-to-use))
            (use-kb u kb))
          (push kb-to-use (slot-value kb 'use-list))
          (use-package (kb-package kb-to-use) (kb-package kb))))))

(defun unuse-kb (kb-to-use-des &optional (kb-des *kb*))
  (check-type kb-des kb-designator)
  (let ((kb (find-kb kb-des))
        (kb-to-use (find-kb kb-to-use-des)))
    (setf (slot-value kb 'use-list) (delete kb-to-use (slot-value kb 'use-list)))
    (unuse-package (kb-package kb-to-use) (kb-package kb))))

; elements and symbols
(defun element-name->symbol (name &optional (kb-des *kb*))
  (check-type kb-des kb-designator)
  (check-type name string)
  (let* ((kb (find-kb kb-des)))
    (find-symbol name (kb-package kb))))

(defun element-name->element (name &optional (kb-des *kb*))
  (check-type kb-des kb-designator)
  (check-type name string)
  (let ((it (element-name->symbol name kb-des)))
    (if (boundp it)
        (symbol-value it)
        nil)))

; intern/uninter elements
(defun kb-intern (el &optional (kb-des *kb*))
  (check-type kb-des kb-designator)
  (check-type el kb-element)
  (let* ((kb (find-kb kb-des))
         (it (element-name->symbol (kb-element-name el) kb)))
    (if (and it (boundp it))
        (progn
          (setf (slot-value kb 'interned-elements) (delete el (slot-value kb 'interned-elements))))
        (progn
          (setf it (intern (kb-element-name el) (kb-package kb)))
          (export it (kb-package kb))))
    (setf (symbol-value it) el)
    (push (symbol-value it) (slot-value kb 'interned-elements))
    (symbol-value it)))

(defun kb-unintern (el &optional (kb-des *kb*))
  (check-type kb-des kb-designator)
  (check-type el kb-element)
  (let* ((kb (find-kb kb-des))
         (it (element-name->symbol (kb-element-name el) kb)))
    (setf (symbol-value it) nil)
    (unexport it (kb-package kb))
    (unintern it (kb-package kb))
    (setf (slot-value kb 'interned-elements) (delete el (slot-value kb 'interned-elements)))))


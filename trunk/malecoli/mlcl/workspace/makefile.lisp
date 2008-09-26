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

;;;; Created on 2008-09-18 16:28:13

(in-package :mlcl)

(defclass makefile ()
  ((file 
    :READER makefile-pprj-file
    :INITARG :file
    :INITFORM nil
    :TYPE pathname)
   (algorithms-file
    :READER makefile-algorithms-pprj-file
    :INITARG :algorithms-file
    :TYPE schema)
   (schema
    :READER makefile-dataset-schema
    :INITARG :schema
    :TYPE schema)
   (package
    :READER makefile-package
    :INITARG :package
    :TYPE package)
   (kb
    :TYPE cl-kb:kb
    :INITARG :kb
    :INITFORM nil
    :READER makefile-kb)))


(defmethod initialize-instance :after ((makefile makefile) &rest initargs)
  (declare (ignore initargs))
  (setf (slot-value makefile 'package) 
        (schema-package (makefile-dataset-schema makefile)))
  (if (null (makefile-pprj-file makefile))
      (setf (slot-value makefile 'file) (merge-pathnames 
                                         (make-pathname :name (makefile-fullname makefile))
                                         (makefile-algorithms-pprj-file makefile))))
  (if (null (makefile-kb makefile))
      (setf (slot-value makefile 'kb) 
            (or (cl-kb:find-kb (makefile-pprj-file makefile) nil)
                (cl-kb:make-kb (makefile-pprj-file makefile) :use (list 
                                                                   (cl-kb:find-kb (makefile-algorithms-pprj-file makefile) t t)
                                                                   (schema-kb (makefile-dataset-schema makefile)))))))
  (if (not (cl-kb:kb-createdp (makefile-kb makefile)))
      (cl-kb:kb-create (makefile-kb makefile)))
  (makefile-load makefile))


(defun makefile-fullname (makefile)
  (format nil "~A-~A" 
          (pathname-name (makefile-algorithms-pprj-file makefile))
          (schema-name (makefile-dataset-schema makefile))))

(defun makefile-source-list-file (makefile)
  (merge-pathnames
   (make-pathname 
    :name (makefile-fullname makefile)
    :type "lisp")
   (makefile-pprj-file makefile)))

(defun makefile-compiled-list-file (makefile)
  (merge-pathnames
   (make-pathname 
    :name (makefile-fullname makefile)
    :type "lisp")
   (makefile-pprj-file makefile)))
  
(defun makefile-load (makefile)
  (let ((lispfile (makefile-source-list-file makefile)))
    (if (or (not (probe-file lispfile)) 
            (< (file-write-date lispfile) (file-write-date (makefile-pprj-file makefile)))
            (< (file-write-date lispfile) (file-write-date (makefile-algorithms-pprj-file makefile)))
            (< (file-write-date lispfile) (file-write-date (schema-pprj-file (makefile-dataset-schema makefile)))))
        (progn
          (cl-kb:kb-open (makefile-kb makefile))
          (cl-kb:kb-open (schema-kb (makefile-dataset-schema makefile)))
          (with-open-file (strm (makefile-source-list-file makefile) :direction :output :if-exists :supersede)
                          (format strm "~{~S~%~}~%~%" (makefile-compile (makefile-package makefile) 
                                                                   (makefile-kb makefile) 
                                                                   (makefile-dataset-schema makefile) 
                                                                   (makefile-fullname makefile))))
          (cl-kb:kb-close (makefile-kb makefile))
          (cl-kb:kb-close (schema-kb (makefile-dataset-schema makefile)))
          (compile-file lispfile)
          (load (makefile-compiled-list-file makefile)))
        (progn
          (load (makefile-compiled-list-file makefile))))))

;
;
;

(defun makefile-compile (package kb schema-kb fullname)
  (cl-kb:with-kb kb nil
                 (let ((compinfo (make-algocomp-info)))
                   (append (makefile-compile-header package kb)
                           (let ((algo-list nil))
                             (cl-kb:cls-do-instance-list (cl-kb:find-cls '|algorithm|::|Algorithm|)
                                                         el
                                                         (push el algo-list))
                             (mapcan #'(lambda (algo) (makefile-compile-algorithm package algo schema-kb compinfo)) algo-list))
                           (makefile-compile-trailer package fullname compinfo)))))

(defstruct algocomp-info
  (algorithms nil))

;
; compile header/trailer
;

(defun makefile-compile-header (package kb)
  (declare (ignore kb))
  (list `(in-package ,(package-name package))))

(defun makefile-compile-trailer (package fullname compinfo)
  (let ((fsymb (cl-kb:string->symbol (format nil "get-~A-algorithms" fullname) package)))
    (list
     `(defun ,fsymb () (list ,@(algocomp-info-algorithms compinfo)))
     `(export '(,fsymb) (find-package ,(package-name package)))
     `(format t ,(format nil "!! loaded ~A !!~A" (package-name package) "~%~%")))))

;
;
;

(defun makefile-compile-algorithm (package algo schema-kb compinfo)
  (declare (ignore package))
  (let* ((compiler-frame 
          (cl-kb:frame-own-slot-value-r algo '|algorithm|::|algorithm_compiler|))
         (class-name
          (cl-kb:frame-own-slot-value compiler-frame '|algorithm|::|algorithm_compiler_class|))
         (class-package
          (cl-kb:frame-own-slot-value compiler-frame '|algorithm|::|algorithm_compiler_package|))
         (class-asdf
          (cl-kb:frame-own-slot-value compiler-frame '|algorithm|::|algorithm_compiler_asdf_package|)))
    (asdf:operate 'asdf:load-op (make-symbol class-asdf))
    (cons
     `(asdf:operate 'asdf:load-op ,class-asdf)
     (let ((symb (find-symbol class-name (find-package class-package))))
       (let ((algorithm-compiler (make-instance symb)))
         (let ((al (algorithm-compiler-compile algorithm-compiler algo schema-kb)))
           (push (nth 1 (car al)) (algocomp-info-algorithms compinfo))
           al))))))



;
;
;

(defun makefile-make (makefile)
  (let ((algorithms nil)
        (symb (find-symbol (format nil "get-~A-algorithms" (makefile-fullname makefile))
                           (makefile-package makefile))))
    (setf algorithms (funcall symb))
    algorithms))


;
; store/restore methods
;

(defvar *makefile-code* (cl-store:register-code 111 'makefile))

(cl-store:defstore-cl-store (obj makefile stream)
                            (cl-store:output-type-code *makefile-code* stream)
                            (cl-store:store-object (makefile-pprj-file obj) stream))

(cl-store:defrestore-cl-store (makefile stream)
                              (let ((file (cl-store:restore-object stream)))
                                (make-instance 'makefile
                                               :file file 
                                               :schema *clstore-schema*)))

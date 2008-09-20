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
    :TYPE pathname)
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
  (if (null (makefile-kb makefile))
      (setf (slot-value makefile 'kb) 
            (or (cl-kb:find-kb (makefile-pprj-file makefile) nil)
                (cl-kb:make-kb (makefile-pprj-file makefile)))))
  (makefile-load makefile))

(defun makefile-fullname (makefile)
  (format nil "~A-~A" 
          (pathname-name (makefile-pprj-file makefile))
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
            (< (file-write-date lispfile) (file-write-date (schema-pprj-file (makefile-dataset-schema makefile)))))
        (progn
          (cl-kb:kb-open (makefile-kb makefile))
          (with-open-file (strm (makefile-source-list-file makefile) :direction :output :if-exists :supersede)
                          (makefile-compile (makefile-package makefile) (makefile-kb makefile) (makefile-fullname makefile) strm))
          (cl-kb:kb-close (makefile-kb makefile))
          (compile-file lispfile)
          (load (makefile-compiled-list-file makefile))
          (funcall (find-symbol "INIT" (makefile-package makefile))))
        (progn
          (load (makefile-compiled-list-file makefile))))))

;
;
;

(defun makefile-compile (package kb fullname strm)
  (cl-kb:kb-open kb)
  (let ((compinfo (make-algocomp-info)))
    (makefile-compile-header package kb strm)
     (let ((algo-list nil))
       (cl-kb:cls-do-instance-list (cl-kb:find-cls '|algorithm|::|Algorithm|)
                                   el
                                   (push el algo-list))
       (dolist (algo algo-list)
         (makefile-compile-algorithm package algo compinfo strm)))
    (makefile-compile-trailer package fullname compinfo strm))
  (cl-kb:kb-close kb))

(defstruct algocomp-info
  (algorithms nil))

;
; compile header/trailer
;

(defun makefile-compile-header (package kb  strm)
  (declare (ignore kb))
  (format strm ";;;; Created on ~A~%~%" (get-universal-time))
  (format strm "(in-package \"~A\")~%~%" (package-name package))
  (format strm "~%~%"))

(defun makefile-compile-trailer (package fullname compinfo strm)
  (format strm "(defun init () ")
  (format strm "nil) ~%~%")
  (format strm "(defun |get-~A-algorithms| ()" fullname)
  (format strm "~%	(list ")
  (dolist (algo (algocomp-info-algorithms compinfo))
    (format strm "~%		(make-instance '~A :name \"~A\" ~{ ~A ~})" 
            (type-of algo) 
            (algorithm-name algo)
            (algorithm-init-arguments algo)))
  (format strm "))~%~%")
  (format strm "(export '(|get-~A-algorithms|))~%~%" fullname)
  (format strm "(format t \"!! loaded ~A !!~A\")" (package-name package) "~%~%")
  (format strm "~%~%")
  (format strm ";;;; Created on ~A~%" (get-universal-time)))

;
;
;

(defun makefile-compile-algorithm (package algo compinfo strm)
  (declare (ignore package))
  (let* ((compiler-frame 
          ;(cl-kb:frame-own-slot-value algo '|algorithm|::|algorithm_compiler|))
          (cl-kb:frame-own-slot-value-r algo '|algorithm|::|algorithm_compiler|))
         (class-name
          (cl-kb:frame-own-slot-value compiler-frame '|algorithm|::|algorithm_compiler_class|))
         (class-package
          (cl-kb:frame-own-slot-value compiler-frame '|algorithm|::|algorithm_compiler_package|))
         (class-asdf
          (cl-kb:frame-own-slot-value compiler-frame '|algorithm|::|algorithm_compiler_asdf_package|)))
    (format t "### ~A~%" compiler-frame)
    (format strm ";;;; Created algorithm ~A ~%~%" (cl-kb:frame-name algo))
    (format strm "(asdf:operate 'asdf:load-op '~A)~%~%" class-asdf)
    (asdf:operate 'asdf:load-op (make-symbol class-asdf))
    (let ((symb (find-symbol class-name (find-package class-package))))
      (let ((algorithm-compiler (make-instance symb)))
        (push (algorithm-compiler-compile algorithm-compiler algo)
              (algocomp-info-algorithms compinfo))))))

;
;
;

(defun makefile-make (makefile)
  (let ((algorithms nil)
        (symb (find-symbol (format nil "get-~A-algorithms" (makefile-fullname makefile))
                           (makefile-package makefile))))
    (setf algorithms (funcall symb))
    algorithms))


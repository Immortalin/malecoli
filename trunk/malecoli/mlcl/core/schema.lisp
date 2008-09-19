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

;;;; Created on 2008-09-10 11:08:30

(in-package :mlcl)

(defclass schema ()
  ((file 
    :READER schema-pprj-file
    :INITARG :file
    :TYPE pathname)
   (package
    :READER schema-package
    :INITARG :package
    :TYPE package)
   (kb
    :TYPE cl-kb:kb
    :INITARG :kb
    :INITFORM nil
    :READER schema-kb)))

(defmethod initialize-instance :after ((schema schema) &rest initargs)
  (declare (ignore initargs))
  (setf (slot-value schema 'package) 
        (or (find-package (format nil "~A-ws" (schema-name schema))) 
            (make-package (format nil "~A-ws" (schema-name schema)) 
                          :use '(:cl :cl-kb :mlcl))))
  (if (null (schema-kb schema))
      (setf (slot-value schema 'kb) 
            (or (cl-kb:find-kb (schema-name schema) nil)
                (cl-kb:make-kb (schema-pprj-file schema)
                                 :use '(cl-kbs::|dataset|)))))
  (dolist (ukb (cl-kb:kb-use-list (schema-kb schema)))
    (if (not (member ukb (list (cl-kb:find-kb 'cl-kbs::|dataset|) (cl-kb:find-kb 'cl-kbs::|protege|))))
        (let ((ns (make-instance 'schema :file (cl-kb:kb-protege-pprj-file ukb) :kb ukb)))
          (use-package (schema-package ns) (schema-package schema))))) 
  (schema-load schema))

(defun schema-name (schema)
  (pathname-name (schema-pprj-file schema)))

(defun schema-source-list-file (schema)
  (merge-pathnames
   (make-pathname :type "lisp")
   (schema-pprj-file schema)))

(defun schema-compiled-list-file (schema)
  (merge-pathnames
   (make-pathname :type nil)
   (schema-source-list-file schema)))

(defun schema-xml-kb-file (schema)
  (merge-pathnames
   (make-pathname :type "xml")
   (schema-pprj-file schema)))

(defun schema-load (schema)
  (let ((lispfile (schema-source-list-file schema)))
    (if (or (not (probe-file lispfile)) (< (file-write-date lispfile) (file-write-date (schema-xml-kb-file schema))))
        (progn
          (cl-kb:kb-open (schema-kb schema))
          (with-open-file (strm (schema-source-list-file schema) :direction :output :if-exists :supersede)
                          (schema-compile (schema-package schema) (schema-kb schema) strm))
          (cl-kb:kb-close (schema-kb schema))
          (compile-file lispfile)
          (load (schema-compiled-list-file schema))
          (funcall (find-symbol "INIT-DATASET" (schema-package schema))))
        (progn
          (load (schema-compiled-list-file schema))))))



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
  ((pathname 
    :READER dataset-pathname
    :INITARG :pathname
    :TYPE pathname)
   (kb
    :TYPE kb
    :READER dataset-kb)
   (sqldatabase
    :TYPE clsql:database
    :READER database-sqldatabase)))


(defmethod initialize-instance :after ((dataset dataset) &rest initargs)
  (declare (ignore initargs))
  (setf (slot-value dataset 'kb) (mlcl-kb:make-kb 
                                  (file-namestring (dataset-pathname dataset))
                                  :use-list '(mlcl-kbs::dataset-kb)
                                  :protege-file (merge-pathnames
                                                 (make-pathname :type "pprj")
                                                 (dataset-pathname dataset))))
  (setf (slot-value dataset 'sqldatabase) 
        (clsql:connect (list (format nil "~A" (merge-pathnames
                                               (make-pathname :type "sqlite3")
                                               (dataset-pathname dataset))))
                       :database-type :sqlite3)))
  

                                                  
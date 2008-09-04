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

;;;; Created on 2008-09-04 10:45:22

(in-package :mlcl-dataset)

(defun dataset-generate-lisp-file (name pathname kb)
  (let ((lispfile (merge-pathnames
                   (make-pathname :type "lisp")
                   pathname)))
    (with-open-file (strm lispfile :direction :output :if-exists :supersede)
                    (compile-header name strm)
                    (mlcl-kb:kb-open kb)
                    (let ((cls-list)
                          (si-list))
                      (dolist (el (mlcl-kb:kb-interned-elements kb))
                        (if (and (typep el 'mlcl-kb:cls) 
                                 (mlcl-kb:cls-has-supercls el 'dataset-kb::|DatasetThing|))
                            (push el cls-list))
                        (if (and (typep el 'mlcl-kb:simple-instance) 
                                 (mlcl-kb:instance-has-type el 'dataset-kb::|DatasetThing|))
                            (push el si-list)))
                      (compile-clses cls-list strm)
                      (compile-simple-instances si-list strm))
                    (mlcl-kb:kb-close kb)
                    (compile-trailer strm))))

                               
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

;;;; Created on 2008-08-26 16:52:11

(in-package :mlcl-kb)

;
; import/export protege pprj
;

(defvar *empty-pprj-pathname*)

(defun kb-import-from-protege-pprj (pathname kb)
  (check-type pathname pathname)
  (check-type kb kb)
  (if (probe-file pathname)
      nil
      (error "PPRJ protege file ~S does not exist." pathname)))

(defun kb-export-to-protege-pprj (pathname xml-file kb &key (supersedep t))
  (check-type pathname pathname)
  (check-type xml-file (or pathname string))
  (check-type kb kb)
  (if (or supersedep (not (probe-file pathname)))
      (let ((included-projects-line nil))
        (dolist (k (kb-use-list kb))
          (if (kb-protege-file k)
              (setf included-projects-line
                    (concatenate 'string included-projects-line
                                 (format nil "included_projects \"~a\"" 
                                         (file-namestring (merge-pathnames
                                          (make-pathname :type "pprj")
                                          (kb-protege-file k))))))))
        (with-open-file (in *empty-pprj-pathname* :direction :input)
                        (with-open-file (out pathname :direction :output :if-exists :supersede)
                                        (do ((line (read-line in nil)
                                                   (read-line in nil)))
                                            ((null line))
                                          (if included-projects-line 
                                              (setf line (cl-ppcre:regex-replace "next_frame_number" line
                                                                                 (format nil "~A)~%	~A" included-projects-line "(next_frame_number"))))
                                          (setf line (cl-ppcre:regex-replace "empty\.xml" line
                                                                             (format nil "~A" (file-namestring xml-file))))
                                          (write-line line out)))))))


;
; init variables
;

(eval-when (:LOAD-TOPLEVEL :EXECUTE)
  (if (null (boundp '*empty-pprj-pathname*))
      (setq *empty-pprj-pathname*            
            #-sbcl (merge-pathnames
                    (make-pathname
                     :directory '(:relative ".." "resources")
                     :name "empty" :type "pprj" :case :local)
                    *load-truename*)
            #+sbcl #p"/hardmnt/tharpe0/sra/serra/Software/Developing/MaLeCoLi/code.google.com/workspace/malecoli-trunk/malecoli/mlcl-kb/resources/empty.pprj")))

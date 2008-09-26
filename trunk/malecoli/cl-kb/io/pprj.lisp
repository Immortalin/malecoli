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

(in-package :cl-kb)

;
; import/export protege pprj
;

(defvar *empty-pprj-pathname* (find-kb-file "empty"))

(defun kb-import-from-protege-pprj (pathname kb)
  (check-type pathname pathname)
  (check-type kb kb)
  (if (probe-file pathname)
      (multiple-value-bind (xml-pathname includes_projects) (extract-info-from-protege-pprj pathname)
        (setf (kb-protege-xml-file kb) xml-pathname)
        (dolist (p includes_projects)
          (if (null (pathname-directory p))
              (let ((pp (merge-pathnames
                         (make-pathname :directory (pathname-directory pathname))
                         p)))
                (if (probe-file pp)
                    (setf p pp))))
          (let ((ukb (find-kb p t t)))
            (use-kb ukb kb))))             
      (error "PPRJ protege file ~S does not exist." pathname)))

(defun kb-export-to-protege-pprj (pprj-file xml-file kb &key (supersedep t))
  (check-type pprj-file pathname)
  (check-type xml-file (or pathname string))
  (check-type kb kb)
  (if (or supersedep (not (probe-file pprj-file)))
      (let ((pprj-old nil)
            (pprj-new nil)
            (included-pprj-file-list nil))
        (if (probe-file pprj-file)
            (setf pprj-old (file->seq pprj-file))
            (setf pprj-old (file->seq *empty-pprj-pathname*)))
        (dolist (k (kb-use-list kb))
          (push (kb-protege-pprj-file k) included-pprj-file-list))
        (setf pprj-new (put-info-into-protege-pprj pprj-old xml-file included-pprj-file-list))
        (seq->file pprj-new pprj-file))))


;
; put info
;

(defun put-info-into-protege-pprj (pprj xml-file  included-pprj-file-list)
  (let ((new-pprj nil)
        (included-projects-line nil))
    (dolist (k included-pprj-file-list)
      (setf included-projects-line
            (concatenate 'string included-projects-line
                         (format nil " \"~a\"" 
                                 (file-namestring k)))))
    (setf new-pprj (cl-ppcre:regex-replace 
                    "(.name\\s*\"source_file_name\"\\s*.\\s*.\\s*string_value\\s*\"(.*)\"\\s*.)"
                    pprj
                    (format nil "(name \"source_file_name\") (string_value \"~A\")" 
                            (file-namestring xml-file))))
    (setf new-pprj (cl-ppcre:regex-replace 
                    "included_projects\\s*((\"([^\"]*)\"\\s*)*)"
                    new-pprj
                    (format nil "included_projects ~A ~%" 
                            included-projects-line)))))
          

;
; extract info
;

(defun extract-info-from-protege-pprj (pathname)
  (check-type pathname pathname)
  (let ((xml-pathname nil)
        (includes_projects nil))
    (with-open-file (pprj pathname :direction :input)
                    (let ((cont  (stream->seq pprj)))
                      (setf xml-pathname 
                            (cl-ppcre:register-groups-bind (nil v1)
                                                           ("(name\\s*\"source_file_name\"\\s*.\\s*.\\s*string_value\\s*\"(.*)\")" cont) v1))
                      (setf includes_projects
                           (cl-ppcre:register-groups-bind (v1) 
                                                           ("included_projects\\s*((\"([^\"]*)\"\\s*)*)" cont) v1))))
    (if includes_projects
        (progn
          (setf includes_projects (cl-ppcre:regex-replace-all 
                                   "file:" 
                                   includes_projects         
                                   ""))
          (setf includes_projects (cl-ppcre:split "\\s+" includes_projects))))
    (if (equal (char xml-pathname 0) #\/ )
        (setf xml-pathname (pathname xml-pathname))
        (setf xml-pathname (merge-pathnames (pathname xml-pathname) (make-pathname :directory (pathname-directory pathname) :name xml-pathname))))
    (let ((ips nil))
      (dolist (ip includes_projects)
        (setf ip (string-trim "\" 	" ip))
        (setf ip (pathname ip))
        (push ip ips))
      (setf includes_projects ips))
    (values xml-pathname includes_projects)))



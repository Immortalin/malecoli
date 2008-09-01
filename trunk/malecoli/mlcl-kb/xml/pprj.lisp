;;;; Created on 2008-08-26 16:52:11

(in-package :mlcl-kb)


;
; open/close
;

(defun kb-open (&optional (kb *kb*))
  (check-type kb kb)  
  (if (not (kb-openedp kb))
      (progn
        (dolist (ukb (kb-use-list kb))
          (kb-open ukb))
        (if (kb-protege-file kb) (kb-import-from-protege (kb-protege-file kb) kb))
        (setf (kb-openedp kb) t))))

(defun kb-close (&optional (kb *kb*))
  (check-type kb kb)
  (if (kb-openedp kb)
      (progn
        (if (kb-protege-file kb) 
            (kb-export-to-protege (kb-protege-file kb) kb nil))
        (kb-clear kb)
        (setf (kb-openedp kb) nil)
        (setf (kb-loadedp kb) nil))))


;
; protege save
;

(defun kb-save (&optional (kb *kb*))
  (check-type kb kb)
  (if (kb-protege-file kb)
        (kb-export-to-protege (kb-protege-file kb) kb t)))


;
; protege files
;

(defun kb-xml-protege-pathname (&optional (kb *kb*))
  (merge-pathnames
   (make-pathname :type "xml")
   (kb-protege-file kb)))

(defun kb-pprj-protege-pathname (&optional (kb *kb*))
  (merge-pathnames
   (make-pathname :type "pprj")
   (kb-protege-file kb)))

(defun kb-lisp-protege-pathname (&optional (kb *kb*))
  (merge-pathnames
   (make-pathname :type "lisp")
   (kb-protege-file kb)))


;
; import/export functions
;
                        
(defun kb-import-from-protege (pathname &optional (kb *kb*))
  (check-type pathname pathname)
  (check-type kb kb)
  (kb-import-from-protege-pprj pathname kb)
  (kb-import-from-protege-xml pathname kb))

(defun kb-export-to-protege (pathname &optional (kb *kb*) (supersedep t))
  (check-type pathname pathname)
  (check-type kb kb)
  (kb-export-to-protege-pprj pathname kb :supersedep supersedep)
  (kb-export-to-protege-xml pathname kb :supersedep supersedep))


;
; import/export protege pprj
;

(defvar *empty-pprj-pathname*)

(defun kb-import-from-protege-pprj (pathname kb)
  (declare (ignore pathname)
           (ignore kb)))

(defun kb-export-to-protege-pprj (pathname kb &key (supersedep t))
  (check-type pathname pathname)
  (check-type kb kb)
  (let ((pprj-file (merge-pathnames
                    (make-pathname :type "pprj")
                    pathname))
        (xml-file (merge-pathnames
                   (make-pathname :type "xml")
                   pathname))
        (included-projects-line nil))
    (setf included-projects-line
          (format nil "included_projects ~{\"file:~a\" ~}" (mapcar #'(lambda (k) (kb-pprj-protege-pathname k)) (kb-use-list kb))))
    (if (or supersedep (not (probe-file pprj-file)))
        (with-open-file (in *empty-pprj-pathname* :direction :input)
                        (with-open-file (out pprj-file :direction :output :if-exists :supersede)
                                        (do ((line (read-line in nil)
                                                   (read-line in nil)))
                                            ((null line))
                                          (setf line (cl-ppcre:regex-replace "next_frame_number" line
                                                                             (format nil "~A )~%	~A" included-projects-line "(next_frame_number")))
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


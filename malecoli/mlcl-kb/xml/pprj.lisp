;;;; Created on 2008-08-26 16:52:11

(in-package :mlcl-kb)

(progn
  (defvar *empty-pprj-pathname*)
  (eval-when (:LOAD-TOPLEVEL :EXECUTE)
    (if (null (boundp '*empty-pprj-pathname*))
        (setq *empty-pprj-pathname*            
              #-sbcl (merge-pathnames
                      (make-pathname
                       :directory '(:relative ".." "resources")
                       :name "empty" :type "pprj" :case :local)
                      *load-truename*)
              #+sbcl #p"/hardmnt/tharpe0/sra/serra/Software/Developing/MaLeCoLi/code.google.com/workspace/malecoli-trunk/malecoli/mlcl-kb/resources/empty.pprj"))))

(defun save-new-pprj (kb)
  (let ((fn (file-namestring (kb-protege-file kb)))
        (pprj-pathname
         (merge-pathnames
          (make-pathname :type "pprj")
           (kb-protege-file kb))))
    (if (null (probe-file pprj-pathname))
        (with-open-file (in *empty-pprj-pathname* :direction :input)
                        (with-open-file (out pprj-pathname :direction :output)
                                        (do ((line (read-line in nil)
                                                   (read-line in nil)))
                                            ((null line))
                                          (write-line (cl-ppcre:regex-replace "empty\.xml" line fn) out)))))))

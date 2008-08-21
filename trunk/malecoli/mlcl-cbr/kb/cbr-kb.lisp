;;;; 2008-08-21 09:30:59
;;;; This is your lisp file. May it serve you well.

(in-package :mlcl-cbr)

(progn
  (defvar *cbr-kb-pathname*)
  (eval-when (:COMPILE-TOPLEVEL)
    (if (null (boundp '*cbr-kb-pathname*))
        (setq *cbr-kb-pathname*            
              #-sbcl (merge-pathnames
                      (make-pathname
                       :name "cbr" :type "xml" :case :local)
                      *compile-file-truename*)
              #+sbcl #p"/hardmnt/tharpe0/sra/serra/Software/Developing/MaLeCoLi/code.google.com/workspace/malecoli-trunk/malecoli/mlcl-cbr/kb/cbr.xml")))
  (eval-when (:LOAD-TOPLEVEL :EXECUTE)
    (if (null (boundp '*cbr-kb-pathname*))
        (setq *cbr-kb-pathname*            
              #-sbcl (merge-pathnames
                      (make-pathname
                       :name "cbr" :type "xml" :case :local)
                      *load-truename*)
              #+sbcl #p"/hardmnt/tharpe0/sra/serra/Software/Developing/MaLeCoLi/code.google.com/workspace/malecoli-trunk/malecoli/mlcl-cbr/kb/cbr.xml"))))

(mlcl-kb:def-kb "CBR-KB"
                :use (list mlcl-kbs::PROTEGE-KB) 
                :protege-file *cbr-kb-pathname*)

(mlcl-kb:in-kb "CBR-KB")

(mlcl-kb:def-cls-ref "cbr_case")

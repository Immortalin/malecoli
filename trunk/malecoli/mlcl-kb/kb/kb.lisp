;;;; 2008-08-21 09:30:59
;;;; This is your lisp file. May it serve you well.

(in-package :mlcl-kb)

(progn
  (defvar *kb-pathname*)
  (eval-when (:COMPILE-TOPLEVEL)
    (if (null (boundp '*kb-pathname*))
        (setq *kb-pathname*            
              #-sbcl (merge-pathnames
                      (make-pathname
                       :directory '(:relative ".." "resources")
                       :name "kb" :type "xml" :case :local)
                      *compile-file-truename*)
              #+sbcl #p"/hardmnt/tharpe0/sra/serra/Software/Developing/MaLeCoLi/code.google.com/workspace/malecoli-trunk/malecoli/mlcl-kb/resources/kb.xml")))
  (eval-when (:LOAD-TOPLEVEL :EXECUTE)
    (if (null (boundp '*kb-pathname*))
        (setq *kb-pathname*            
              #-sbcl (merge-pathnames
                      (make-pathname
                       :directory '(:relative ".." "resources")
                       :name "kb" :type "xml" :case :local)
                      *load-truename*)
              #+sbcl #p"/hardmnt/tharpe0/sra/serra/Software/Developing/MaLeCoLi/code.google.com/workspace/malecoli-trunk/malecoli/mlcl-kb/resources/kb.xml"))))
  

(mlcl-kb:def-kb "KB" 
                :use (list mlcl-kbs::PROTEGE-KB) 
                :protege-file *kb-pathname*)


(mlcl-kb:in-kb "KB")

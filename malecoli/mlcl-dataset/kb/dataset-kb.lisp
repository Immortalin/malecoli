;;;; 2008-08-21 09:30:59
;;;; This is your lisp file. May it serve you well.

(in-package :mlcl-dataset)

(progn
  (defvar *dataset-kb-pathname*)
  (eval-when (:COMPILE-TOPLEVEL)
    (if (null (boundp '*dataset-kb-pathname*))
        (setq *dataset-kb-pathname*            
              #-sbcl (merge-pathnames
                      (make-pathname
                       :name "dataset" :type "xml" :case :local)
                      *compile-file-truename*)
              #+sbcl #p"/hardmnt/tharpe0/sra/serra/Software/Developing/MaLeCoLi/code.google.com/workspace/malecoli-trunk/malecoli/mlcl-dataset/kb/dataset.xml")))
  (eval-when (:LOAD-TOPLEVEL :EXECUTE)
    (if (null (boundp '*dataset-kb-pathname*))
        (setq *dataset-kb-pathname*            
              #-sbcl (merge-pathnames
                      (make-pathname
                       :name "dataset" :type "xml" :case :local)
                      *load-truename*)
              #+sbcl #p"/hardmnt/tharpe0/sra/serra/Software/Developing/MaLeCoLi/code.google.com/workspace/malecoli-trunk/malecoli/mlcl-dataset/kb/dataset.xml"))))
  

(mlcl-kb:def-kb "DATASET-KB" 
                :use (list mlcl-kbs::PROTEGE-KB) 
                :protege-file *dataset-kb-pathname*)

(mlcl-kb:in-kb mlcl-kbs::DATASET-KB)

(mlcl-kb:def-cls-ref "dataset_case")

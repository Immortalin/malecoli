;;;; Created on 2008-09-12 11:06:01

(in-package :mlcl-dataset)

(defun get-resource-pathname (name type)
  (if mlcl-kb::*cusp-developmentp* 
      (merge-pathnames
       (make-pathname
        :name name
        :type type)
       #p"/hardmnt/tharpe0/sra/serra/Software/Developing/MaLeCoLi/code.google.com/workspace/malecoli-trunk/malecoli/mlcl-dataset/resources/")
      (merge-pathnames
       (make-pathname
        :name name 
        :type type)
       *load-truename*)))

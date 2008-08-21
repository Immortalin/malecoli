;;;; Created on 2008-04-24 12:44:13

(in-package :clone-ml)

(progn
  (defvar *gare-instances-kb-pathname*)
  (eval-when (:COMPILE-TOPLEVEL)
    (if (null (boundp '*gare-instances-kb-pathname*))
        (setq *gare-instances-kb-pathname*            
              #-sbcl (merge-pathnames
                      (make-pathname
                       :name "gare-instances" :type "xml" :case :local)
                      *compile-file-truename*)
              #+sbcl #p"/hardmnt/tharpe0/sra/serra/Software/Developing/MaLeCoLi/code.google.com/workspace/malecoli-trunk/tests/one/clone-ml/kb/gare-instances.xml")))
  (eval-when (:LOAD-TOPLEVEL :EXECUTE)
    (if (null (boundp '*gare-instances-kb-pathname*))
        (setq *gare-instances-kb-pathname*            
              #-sbcl (merge-pathnames
                      (make-pathname
                       :name "gare-instances" :type "xml" :case :local)
                      *load-truename*)
              #+sbcl #p"/hardmnt/tharpe0/sra/serra/Software/Developing/MaLeCoLi/code.google.com/workspace/malecoli-trunk/tests/one/clone-ml/kb/gare-instances.xml"))))


(mlcl-kb:def-kb "GARE-INSTANCES-KB"
                :use (list mlcl-kbs::PROTEGE-KB mlcl-kbs::negotiation-kb mlcl-kbs::gare-kb)
                :protege-file *gare-instances-kb-pathname*
                :autoload nil)

(mlcl-kb:in-kb "GARE-INSTANCES-KB")                 


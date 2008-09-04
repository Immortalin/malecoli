;;;; Created on 2008-04-24 12:44:13

(in-package :clone-ml)

(defvar *gare-instances-kb-pathname*)

(eval-when (:LOAD-TOPLEVEL :EXECUTE)
  (if (null (mlcl-kb:find-kb "GARE-INSTANCES-KB"))
      (progn
        (setf *gare-instances-kb-pathname*            
              ;#-sbcl (merge-pathnames
              ;        (make-pathname
              ;         :directory '(:relative ".." "resources")
              ;         :name "cbr" :type "xml" :case :local)
              ;        *load-truename*)
              ;#+sbcl 
              #p"/hardmnt/tharpe0/sra/serra/Software/Developing/MaLeCoLi/workspace/extra/gare/resources/gare-instances.xml")
        (mlcl-kb:make-kb "GARE-INSTANCES-KB" 
                         :use-list '(mlcl-kbs::protege-kb
                                     mlcl-kbs::dataset-kb  
                                     mlcl-kbs::negotiation-kb 
                                     mlcl-kbs::gare-kb) :protege-file *gare-instances-kb-pathname*))))



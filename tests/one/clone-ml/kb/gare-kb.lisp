;;;; Created on 2008-04-24 12:44:13


(in-package :clone-ml)

(defvar *gare-kb-pathname*)

(eval-when (:LOAD-TOPLEVEL :EXECUTE)
  (if (null (mlcl-kb:find-kb "GARE-KB" nil))
      (progn
        (setf *gare-kb-pathname*            
              #-sbcl (merge-pathnames
                      (make-pathname
                       :directory '(:relative ".." "resources")
                       :name "gare" :type "xml" :case :local)
                      *load-truename*)
              #+sbcl #p"/hardmnt/tharpe0/sra/serra/Software/Developing/MaLeCoLi/code.google.com/workspace/malecoli-trunk/tests/one/clone-ml/resources/gare.xml")
        (mlcl-kb:make-kb "GARE-KB" 
                         :use-list '(mlcl-kbs::protege-kb 
                                     mlcl-kbs::dataset-kb 
                                     mlcl-kbs::negotiation-kb) :protege-file *gare-kb-pathname*))))


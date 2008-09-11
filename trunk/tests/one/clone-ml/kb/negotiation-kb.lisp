;;;; Created on 2008-08-21 14:08:18

(in-package :clone-ml)


(defvar *negotiation-kb-pathname*)

(eval-when (:LOAD-TOPLEVEL :EXECUTE)
  (if (null (mlcl-kb:find-kb "NEGOTIATION-KB" nil))
      (progn
        (setf *negotiation-kb-pathname*            
              #-sbcl (merge-pathnames
                      (make-pathname
                       :directory '(:relative ".." "resources")
                       :name "negotiation" :type "xml" :case :local)
                      *load-truename*)
              #+sbcl #p"/hardmnt/tharpe0/sra/serra/Software/Developing/MaLeCoLi/code.google.com/workspace/malecoli-trunk/tests/one/clone-ml/resources/negotiation.xml")
        (mlcl-kb:make-kb "NEGOTIATION-KB" 
                         :use-list '(mlcl-kbs::protege-kb mlcl-kbs::dataset-kb) :protege-file *negotiation-kb-pathname*))))


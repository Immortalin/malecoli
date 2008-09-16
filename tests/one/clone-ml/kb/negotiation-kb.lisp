;;;; Created on 2008-08-21 14:08:18

(in-package :clone-ml)


(defvar *negotiation-kb-pathname*)

(eval-when (:LOAD-TOPLEVEL :EXECUTE)
  (if (null (mlcl-kb:find-kb "negotiation" nil))
      (progn
        (setf *negotiation-kb-pathname*            
              #-sbcl (merge-pathnames
                      (make-pathname
                       :directory '(:relative ".." "resources")
                       :name "negotiation" :type "pprj" :case :local)
                      *load-truename*)
              #+sbcl #p"/hardmnt/tharpe0/sra/serra/Software/Developing/MaLeCoLi/code.google.com/workspace/malecoli-trunk/tests/one/clone-ml/resources/negotiation.pprj")
        (mlcl-kb:make-kb *negotiation-kb-pathname* 
                         :use '(mlcl-kbs::|dataset|)))))


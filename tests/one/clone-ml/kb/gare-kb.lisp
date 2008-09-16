;;;; Created on 2008-04-24 12:44:13


(in-package :clone-ml)
kb
(defvar *gare-kb-pathname*)

(eval-when (:LOAD-TOPLEVEL :EXECUTE)
  (if (null (mlcl-kb:find-kb "gare" nil))
      (progn
        (setf *gare-kb-pathname*            
              #-sbcl (merge-pathnames
                      (make-pathname
                       :directory '(:relative ".." "resources")
                       :name "gare" :type "pprj" :case :local)
                      *load-truename*)
              #+sbcl #p"/hardmnt/tharpe0/sra/serra/Software/Developing/MaLeCoLi/code.google.com/workspace/malecoli-trunk/tests/one/clone-ml/resources/gare.pprj")
        (mlcl-kb:make-kb *gare-kb-pathname* 
                         :use '(mlcl-kbs::|negotiation|)))))


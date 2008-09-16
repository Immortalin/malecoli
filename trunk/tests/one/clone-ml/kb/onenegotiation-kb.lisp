;;;; Created on 2008-04-24 12:43:48\

(in-package :clone-ml)

(defvar *onenegotiation-kb-pathname*)

(eval-when (:LOAD-TOPLEVEL :EXECUTE)
  (if (null (mlcl-kb:find-kb "onenegotiation" nil))
      (progn
        (setf *onenegotiation-kb-pathname*            
              #-sbcl (merge-pathnames
                      (make-pathname
                       :directory '(:relative ".." "resources")
                       :name "onenegotiation" :type "pprj" :case :local)
                      *load-truename*)
              #+sbcl #p"/hardmnt/tharpe0/sra/serra/Software/Developing/MaLeCoLi/code.google.com/workspace/malecoli-trunk/tests/one/clone-ml/resources/onenegotiation.pprj")
        (mlcl-kb:make-kb *onenegotiation-kb-pathname* 
                         :use '(mlcl-kbs::|negotiation|)))))

;;;; Created on 2008-04-24 12:43:48\

(in-package :clone-ml)

(defvar *onenegotiation-kb-pathname*)

(eval-when (:LOAD-TOPLEVEL :EXECUTE)
  (if (null (mlcl-kb:find-kb "ONENEGOTIATION-KB" nil))
      (progn
        (setf *onenegotiation-kb-pathname*            
              #-sbcl (merge-pathnames
                      (make-pathname
                       :directory '(:relative ".." "resources")
                       :name "onenegotiation" :type "xml" :case :local)
                      *load-truename*)
              #+sbcl #p"/hardmnt/tharpe0/sra/serra/Software/Developing/MaLeCoLi/code.google.com/workspace/malecoli-trunk/tests/one/clone-ml/resources/onenegotiation.xml")
        (format t "GGGGGGGGGGGGGGGGGGGGGGGGGGGG~%")
        (format t "GGGGGGGGGGGGGGGGGGGGGGGGGGGG~%")
        (format t "GGGGGGGGGGGGGGGGGGGGGGGGGGGG~%")
        (mlcl-kb:make-kb "ONENEGOTIATION-KB" 
                         :use-list '(mlcl-kbs::negotiation-kb
                                     mlcl-kbs::protege-kb 
                                     mlcl-kbs::dataset-kb) 
                         :protege-file *onenegotiation-kb-pathname*))))

;;;; Created on 2008-04-24 12:43:48

(in-package :clone-ml)

(progn
  (defvar *onenegotiation-kb-pathname*)
  (eval-when (:COMPILE-TOPLEVEL)
    (if (null (boundp '*onenegotiation-kb-pathname*))
        (setq *onenegotiation-kb-pathname*            
              #-sbcl (merge-pathnames
                      (make-pathname
                       :name "onenegotiation" :type "xml" :case :local)
                      *compile-file-truename*)
              #+sbcl #p"/hardmnt/tharpe0/sra/serra/Software/Developing/MaLeCoLi/code.google.com/workspace/malecoli-trunk/tests/one/clone-ml/kb/onenegotiation.xml")))
  (eval-when (:LOAD-TOPLEVEL :EXECUTE)
    (if (null (boundp '*onenegotiation-kb-pathname*))
        (setq *onenegotiation-kb-pathname*            
              #-sbcl (merge-pathnames
                      (make-pathname
                       :name "onenegotiation" :type "xml" :case :local)
                      *load-truename*)
              #+sbcl #p"/hardmnt/tharpe0/sra/serra/Software/Developing/MaLeCoLi/code.google.com/workspace/malecoli-trunk/tests/one/clone-ml/kb/onenegotiation.xml"))))

(mlcl-kb:def-kb "ONENEGOTIATION-KB"
                :use (list mlcl-kbs::PROTEGE-KB mlcl-kbs::negotiation-kb) 
                :protege-file *onenegotiation-kb-pathname*)

(mlcl-kb:in-kb "ONENEGOTIATION-KB")


(mlcl-kb:def-cls-ref "one_case")
(mlcl-kb:def-cls-ref "one_context")
(mlcl-kb:def-cls-ref "one_conclusion")
(mlcl-kb:def-cls-ref "one_protocol")
(mlcl-kb:def-cls-ref "one_process")
(mlcl-kb:def-cls-ref "one_item")
(mlcl-kb:def-cls-ref "one_proposal")
(mlcl-kb:def-cls-ref "one_issue")

(mlcl-kb:def-cls-ref "one_image")
(mlcl-kb:def-cls-ref "one_binary_document")


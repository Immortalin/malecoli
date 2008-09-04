;;;; Created on 2008-08-21 14:08:18

(in-package :clone-ml)


(defvar *negotiation-kb-pathname*)

(eval-when (:LOAD-TOPLEVEL :EXECUTE)
  (if (null (mlcl-kb:find-kb "NEGOTIATION-KB"))
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

#|

(mlcl-kb:def-cls-ref "neg_case")
(mlcl-kb:def-cls-ref "neg_date")
(mlcl-kb:def-slot-ref "neg_case_context")
(mlcl-kb:def-slot-ref "neg_case_item")
(mlcl-kb:def-slot-ref "neg_case_protocol")
(mlcl-kb:def-slot-ref "neg_case_model")
(mlcl-kb:def-slot-ref "neg_case_conclusion")
(mlcl-kb:def-slot-ref "neg_case_agreement")
(mlcl-kb:def-slot-ref "neg_case_owner")
(mlcl-kb:def-slot-ref "date_year")
(mlcl-kb:def-slot-ref "date_month")
(mlcl-kb:def-slot-ref "date_day")
(mlcl-kb:def-slot-ref "neg_case_ending_date")
(mlcl-kb:def-slot-ref "neg_case_starting_price")
(mlcl-kb:def-slot-ref "neg_case_conclusion_date")
(mlcl-kb:def-slot-ref "neg_case_winner")

(mlcl-kb:def-slot-ref "neg_model_id")
(mlcl-kb:def-slot-ref "neg_model_version")

|#

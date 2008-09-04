;;;; Created on 2008-04-24 12:44:13


(in-package :clone-ml)

(defvar *gare-kb-pathname*)

(eval-when (:LOAD-TOPLEVEL :EXECUTE)
  (if (null (mlcl-kb:find-kb "GARE-KB"))
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


#|               
(mlcl-kb:def-cls-ref "gare_category")
(mlcl-kb:def-slot-ref "gare_category_code")
(mlcl-kb:def-slot-ref "gare_category_business_area")
(mlcl-kb:def-slot-ref "gare_category_description")

(mlcl-kb:def-cls-ref "gare_proposal")
(mlcl-kb:def-slot-ref "proposal-gare_issue_price")

(mlcl-kb:def-cls-ref "gare_location")
(mlcl-kb:def-slot-ref "gare_location_comune")
(mlcl-kb:def-slot-ref "gare_location_provincia")

(mlcl-kb:def-cls-ref "gare_party")
(mlcl-kb:def-cls-ref "gare_public_party")
(mlcl-kb:def-slot-ref "gare_party_location")
(mlcl-kb:def-slot-ref "gare_party_uuid")

(mlcl-kb:def-cls-ref "gare_case")
(mlcl-kb:def-cls-ref "gare_context")
(mlcl-kb:def-cls-ref "gare_conclusion")

(mlcl-kb:def-cls-ref "gare_issue_price")
(mlcl-kb:def-slot-ref "gare_price-amount")
(mlcl-kb:def-slot-ref "gare_price-currency")


(mlcl-kb:def-cls-ref "gare_item")
(mlcl-kb:def-slot-ref "gare_long_description")
(mlcl-kb:def-slot-ref "gare_description")
(mlcl-kb:def-slot-ref "gare_service_starting_date")
(mlcl-kb:def-slot-ref "gare_service_ending_date")
(mlcl-kb:def-slot-ref "gare_service_location")
(mlcl-kb:def-slot-ref "gare_service_categories")
(mlcl-kb:def-cls-ref "gare_protocol")
(mlcl-kb:def-slot-ref "gare_protocol_laws")
(mlcl-kb:def-slot-ref "gare_protocol_procedure")

|#

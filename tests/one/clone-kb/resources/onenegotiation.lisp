;;;; Created on 3445676101

(IN-PACKAGE "onenegotiation-ws")
(USE-PACKAGE "negotiation-ws")
(DEFCLASS |onenegotiation-ws|::|one_xml_document| (MLCL:|DatasetThing|) NIL)
(DEFCLASS |onenegotiation-ws|::|one_image| (MLCL:|DatasetThing|) NIL)
(DEFCLASS |onenegotiation-ws|::|one_binary_document| (MLCL:|DatasetThing|) NIL)
(DEFCLASS |onenegotiation-ws|::|one_issueset| (|negotiation-ws|:|neg_issueset|)
          NIL)
(DEFCLASS |onenegotiation-ws|::|one_itemset| (|negotiation-ws|:|neg_itemset|)
          NIL)
(DEFCLASS |onenegotiation-ws|::|one_issue| (|negotiation-ws|:|neg_issue|) NIL)
(DEFCLASS |onenegotiation-ws|::|one_model| (|negotiation-ws|:|neg_model|) NIL)
(DEFCLASS |onenegotiation-ws|::|one_proposal| (|negotiation-ws|:|neg_proposal|)
          NIL)
(DEFCLASS |onenegotiation-ws|::|one_party| (|negotiation-ws|:|neg_party|)
          ((|onenegotiation-ws|::|one_id| :ACCESSOR
                                          |onenegotiation-ws|::|one_id| :TYPE
                                          (OR NIL STRING) :INITFORM NIL)))
(DEFCLASS |onenegotiation-ws|::|one_item| (|negotiation-ws|:|neg_item|) NIL)
(DEFCLASS |onenegotiation-ws|::|one_process| (|negotiation-ws|:|neg_process|)
          NIL)
(DEFCLASS |onenegotiation-ws|::|one_protocol| (|negotiation-ws|:|neg_protocol|)
          NIL)
(DEFCLASS |onenegotiation-ws|::|one_conclusion|
          (|negotiation-ws|:|neg_conclusion|) NIL)
(DEFCLASS |onenegotiation-ws|::|one_context| (|negotiation-ws|:|neg_context|)
          ((|onenegotiation-ws|::|one_allow_nested| :ACCESSOR
                                                    |onenegotiation-ws|::|one_allow_nested|
                                                    :TYPE (OR NIL BOOLEAN)
                                                    :INITFORM NIL)
           (|onenegotiation-ws|::|one_id| :ACCESSOR
                                          |onenegotiation-ws|::|one_id| :TYPE
                                          (OR NIL STRING) :INITFORM NIL)
           (|onenegotiation-ws|::|one_parent_id| :ACCESSOR
                                                 |onenegotiation-ws|::|one_parent_id|
                                                 :TYPE (OR NIL STRING)
                                                 :INITFORM NIL)
           (|onenegotiation-ws|::|one_visibility| :ACCESSOR
                                                  |onenegotiation-ws|::|one_visibility|
                                                  :TYPE (OR NIL STRING)
                                                  :INITFORM NIL)
           (|onenegotiation-ws|::|one_name| :ACCESSOR
                                            |onenegotiation-ws|::|one_name|
                                            :TYPE (OR NIL STRING) :INITFORM
                                            NIL)
           (|onenegotiation-ws|::|one_creation_date| :ACCESSOR
                                                     |onenegotiation-ws|::|one_creation_date|
                                                     :TYPE (OR NIL T) :INITFORM
                                                     NIL)))
(DEFCLASS |onenegotiation-ws|::|one_case| (|negotiation-ws|:|neg_case|) NIL)
(EXPORT
 '(|onenegotiation-ws|::|one_xml_document| |onenegotiation-ws|::|one_image|
   |onenegotiation-ws|::|one_binary_document|
   |onenegotiation-ws|::|one_issueset| |onenegotiation-ws|::|one_itemset|
   |onenegotiation-ws|::|one_issue| |onenegotiation-ws|::|one_model|
   |onenegotiation-ws|::|one_proposal| |onenegotiation-ws|::|one_id|
   |onenegotiation-ws|::|one_party| |onenegotiation-ws|::|one_item|
   |onenegotiation-ws|::|one_process| |onenegotiation-ws|::|one_protocol|
   |onenegotiation-ws|::|one_conclusion|
   |onenegotiation-ws|::|one_creation_date| |onenegotiation-ws|::|one_name|
   |onenegotiation-ws|::|one_visibility| |onenegotiation-ws|::|one_parent_id|
   |onenegotiation-ws|::|one_id| |onenegotiation-ws|::|one_allow_nested|
   |onenegotiation-ws|::|one_context| |onenegotiation-ws|::|one_case|)
 (FIND-PACKAGE "onenegotiation-ws"))
(FORMAT T "!! loaded ~A !!~%" "onenegotiation-ws")



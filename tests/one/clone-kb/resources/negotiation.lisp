;;;; Created on 3445676101

(IN-PACKAGE "negotiation-ws")
(DEFCLASS |negotiation-ws|::|neg_issueset| (MLCL:|DatasetThing|) NIL)
(DEFCLASS |negotiation-ws|::|neg_itemset| (MLCL:|DatasetThing|) NIL)
(DEFCLASS |negotiation-ws|::|neg_issue| (MLCL:|DatasetThing|) NIL)
(DEFCLASS |negotiation-ws|::|neg_model| (MLCL:|DatasetThing|)
          ((|negotiation-ws|::|neg_model_name| :ACCESSOR
                                               |negotiation-ws|::|neg_model_name|
                                               :TYPE (OR NIL STRING) :INITFORM
                                               NIL)
           (|negotiation-ws|::|neg_model_id| :ACCESSOR
                                             |negotiation-ws|::|neg_model_id|
                                             :TYPE (OR NIL STRING) :INITFORM
                                             NIL)
           (|negotiation-ws|::|neg_model_version| :ACCESSOR
                                                  |negotiation-ws|::|neg_model_version|
                                                  :TYPE (OR NIL STRING)
                                                  :INITFORM NIL)))
(DEFCLASS |negotiation-ws|::|neg_proposal| (MLCL:|DatasetThing|)
          ((|negotiation-ws|::|neg_proposal_response| :ACCESSOR
                                                      |negotiation-ws|::|neg_proposal_response|
                                                      :TYPE (OR NIL STRING)
                                                      :INITFORM NIL)
           (|negotiation-ws|::|neg_msg_timestamp| :ACCESSOR
                                                  |negotiation-ws|::|neg_msg_timestamp|
                                                  :TYPE (OR NIL T) :INITFORM
                                                  NIL)
           (|negotiation-ws|::|neg_msg_sender| :ACCESSOR
                                               |negotiation-ws|::|neg_msg_sender|
                                               :TYPE (OR NIL T) :INITFORM NIL)
           (|negotiation-ws|::|neg_msg_receiver| :ACCESSOR
                                                 |negotiation-ws|::|neg_msg_receiver|
                                                 :TYPE (OR NIL LIST) :INITFORM
                                                 NIL)
           (|negotiation-ws|::|neg_proposal_id| :ACCESSOR
                                                |negotiation-ws|::|neg_proposal_id|
                                                :TYPE (OR NIL STRING) :INITFORM
                                                NIL)))
(DEFCLASS |negotiation-ws|::|neg_party| (MLCL:|DatasetThing|) NIL)
(DEFCLASS |negotiation-ws|::|neg_item| (MLCL:|DatasetThing|) NIL)
(DEFCLASS |negotiation-ws|::|neg_process| (MLCL:|DatasetThing|)
          ((|negotiation-ws|::|neg_case_proposal| :ACCESSOR
                                                  |negotiation-ws|::|neg_case_proposal|
                                                  :TYPE (OR NIL LIST) :INITFORM
                                                  NIL)))
(DEFCLASS |negotiation-ws|::|neg_protocol| (MLCL:|DatasetThing|)
          ((|negotiation-ws|::|neg_case_model| :ACCESSOR
                                               |negotiation-ws|::|neg_case_model|
                                               :TYPE (OR NIL T) :INITFORM NIL)
           (|negotiation-ws|::|neg_case_starting_date| :ACCESSOR
                                                       |negotiation-ws|::|neg_case_starting_date|
                                                       :TYPE (OR NIL T)
                                                       :INITFORM NIL)
           (|negotiation-ws|::|neg_case_ending_date| :ACCESSOR
                                                     |negotiation-ws|::|neg_case_ending_date|
                                                     :TYPE (OR NIL T) :INITFORM
                                                     NIL)))
(DEFCLASS |negotiation-ws|::|neg_conclusion| (MLCL:|DatasetThing|)
          ((|negotiation-ws|::|neg_case_state| :ACCESSOR
                                               |negotiation-ws|::|neg_case_state|
                                               :TYPE (OR NIL STRING) :INITFORM
                                               NIL)
           (|negotiation-ws|::|neg_case_agreement| :ACCESSOR
                                                   |negotiation-ws|::|neg_case_agreement|
                                                   :TYPE (OR NIL T) :INITFORM
                                                   NIL)
           (|negotiation-ws|::|neg_case_winner| :ACCESSOR
                                                |negotiation-ws|::|neg_case_winner|
                                                :TYPE (OR NIL T) :INITFORM NIL)
           (|negotiation-ws|::|neg_case_conclusion_date| :ACCESSOR
                                                         |negotiation-ws|::|neg_case_conclusion_date|
                                                         :TYPE (OR NIL T)
                                                         :INITFORM NIL)))
(DEFCLASS |negotiation-ws|::|neg_context| (MLCL:|DatasetThing|)
          ((|negotiation-ws|::|neg_case_itemset| :ACCESSOR
                                                 |negotiation-ws|::|neg_case_itemset|
                                                 :TYPE (OR NIL T) :INITFORM
                                                 NIL)
           (|negotiation-ws|::|neg_case_participant| :ACCESSOR
                                                     |negotiation-ws|::|neg_case_participant|
                                                     :TYPE (OR NIL LIST)
                                                     :INITFORM NIL)
           (|negotiation-ws|::|neg_case_owner| :ACCESSOR
                                               |negotiation-ws|::|neg_case_owner|
                                               :TYPE (OR NIL T) :INITFORM NIL)))
(DEFCLASS |negotiation-ws|::|neg_case| (MLCL:|DatasetCase|)
          ((|negotiation-ws|::|neg_case_context| :ACCESSOR
                                                 |negotiation-ws|::|neg_case_context|
                                                 :TYPE (OR NIL T) :INITFORM
                                                 NIL)
           (|negotiation-ws|::|neg_case_conclusion| :ACCESSOR
                                                    |negotiation-ws|::|neg_case_conclusion|
                                                    :TYPE (OR NIL T) :INITFORM
                                                    NIL)
           (|negotiation-ws|::|neg_case_protocol| :ACCESSOR
                                                  |negotiation-ws|::|neg_case_protocol|
                                                  :TYPE (OR NIL T) :INITFORM
                                                  NIL)
           (|negotiation-ws|::|neg_case_process| :ACCESSOR
                                                 |negotiation-ws|::|neg_case_process|
                                                 :TYPE (OR NIL T) :INITFORM
                                                 NIL)))
(EXPORT
 '(|negotiation-ws|::|neg_issueset| |negotiation-ws|::|neg_itemset|
   |negotiation-ws|::|neg_issue| |negotiation-ws|::|neg_model_version|
   |negotiation-ws|::|neg_model_id| |negotiation-ws|::|neg_model_name|
   |negotiation-ws|::|neg_model| |negotiation-ws|::|neg_proposal_id|
   |negotiation-ws|::|neg_msg_receiver| |negotiation-ws|::|neg_msg_sender|
   |negotiation-ws|::|neg_msg_timestamp|
   |negotiation-ws|::|neg_proposal_response| |negotiation-ws|::|neg_proposal|
   |negotiation-ws|::|neg_party| |negotiation-ws|::|neg_item|
   |negotiation-ws|::|neg_case_proposal| |negotiation-ws|::|neg_process|
   |negotiation-ws|::|neg_case_ending_date|
   |negotiation-ws|::|neg_case_starting_date|
   |negotiation-ws|::|neg_case_model| |negotiation-ws|::|neg_protocol|
   |negotiation-ws|::|neg_case_conclusion_date|
   |negotiation-ws|::|neg_case_winner| |negotiation-ws|::|neg_case_agreement|
   |negotiation-ws|::|neg_case_state| |negotiation-ws|::|neg_conclusion|
   |negotiation-ws|::|neg_case_owner| |negotiation-ws|::|neg_case_participant|
   |negotiation-ws|::|neg_case_itemset| |negotiation-ws|::|neg_context|
   |negotiation-ws|::|neg_case_process| |negotiation-ws|::|neg_case_protocol|
   |negotiation-ws|::|neg_case_conclusion| |negotiation-ws|::|neg_case_context|
   |negotiation-ws|::|neg_case|)
 (FIND-PACKAGE "negotiation-ws"))
(FORMAT T "!! loaded ~A !!~%" "negotiation-ws")



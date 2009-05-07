;;;; Created on 2009-01-28 14:55:40

(in-package :clone-cbr)


(defun kb->xml-log (negid)
  (declare (ignore negid))
  (let ((parties nil))
    (cl-who:with-html-output-to-string (strm)    
                                       (:ele "log"
                                         (:ele "Context"
                                           (let ((c (cl-kb:find-simple-instance "the context")))
                                             (cl-who:htm (:ele "InstanceID" 
                                                           (cl-who:fmt "~A" (cl-kb:frame-own-slot-value c '|onenegotiation|::|one_id|))))
                                             (cl-who:htm (:ele "Name"
                                                           (cl-who:fmt "~A" (cl-kb:frame-own-slot-value c '|onenegotiation|::|one_name|))))
                                             (cl-who:htm (:ele "Visibility"
                                                           (cl-who:fmt "~A" (cl-kb:frame-own-slot-value c '|onenegotiation|::|one_visibility|))))
                                             (cl-who:htm (:ele "CreationDate"
                                                           (cl-who:fmt "~A" (clone-kb:date->string
                                                                             (cl-kb:frame-own-slot-value c '|onenegotiation|::|one_creation_date|)))))
                                             (dolist (p (cl-kb:frame-own-slot-values c '|negotiation|::|neg_case_participant|))
                                               (push (cl-kb:frame-own-slot-value p '|onenegotiation|::|one_id|) parties))
                                             (push (cl-kb:frame-own-slot-value
                                                                                    (cl-kb:frame-own-slot-value c '|negotiation|::|neg_case_owner|)
                                                                                    '|onenegotiation|::|one_id|) parties)
                                             (cl-who:htm (:ele "Owner"
                                                           (cl-who:fmt "~A" 0)))
                                             
                                             (cl-who:htm (:ele "Partecipants"
                                                           (dolist (p (cdr parties))
                                                             (cl-who:htm (:ele "Partecipant"
                                                                           (cl-who:fmt "~A" (position p parties :test #'string=)))))))
                                             (cl-who:htm (:ele "PartecipantNumber"
                                                           (cl-who:fmt "~A" (length parties)))))
                                           (let ((m (cl-kb:find-simple-instance "this-model")))
                                             (cl-who:htm (:ele "ModelID"
                                                           (cl-who:fmt "~A" (cl-kb:frame-own-slot-value m '|negotiation|::|neg_model_id|))))
                                             (cl-who:htm (:ele "ModelName"
                                                           (cl-who:fmt "~A" (cl-kb:frame-own-slot-value m '|negotiation|::|neg_model_name|))))
                                             (cl-who:htm (:ele "ModelVersion"
                                                           (cl-who:fmt "~A" (cl-kb:frame-own-slot-value m '|negotiation|::|neg_model_version|))))
                                             (let ((p (cl-kb:find-simple-instance "the protocol")))
                                               (cl-who:htm (:ele "StartingDate"
                                                             (cl-who:fmt "~A" (clone-kb:date->string
                                                                               (cl-kb:frame-own-slot-value p '|negotiation|::|neg_case_starting_date|))))
                                                           (:ele "EndingDate"
                                                             (cl-who:fmt "~A" (clone-kb:date->string
                                                                               (cl-kb:frame-own-slot-value p '|negotiation|::|neg_case_ending_date|)))))))
                                           (cl-who:htm 
                                            (:ele "Items"
                                              (let ((its (cl-kb:find-simple-instance "the itemset")))
                                                (cl-kb:cls-do-instance-list |negotiation|::|m_item| mit
                                                                            (cl-who:htm 
                                                                             (:ele "item"
                                                                               (cl-who:htm (:ele "name"
                                                                                             (cl-who:fmt "~A" 
                                                                                                         (cl-kb:frame-own-slot-value mit '|negotiation|::|m_has_name|))))
                                                                               (let ((it (cl-kb:frame-own-slot-value
                                                                                          its
                                                                                          (cl-kb:frame-own-slot-value mit '|negotiation|::|m_slot|))))
                                                                                 (cl-who:htm 
                                                                                  (dolist (attr (cl-kb:frame-own-slot-values mit '|negotiation|::|m_has_attr|))
                                                                                    (cl-who:htm 
                                                                                     (:ele "attribute"
                                                                                       (:ele "name"
                                                                                         (cl-who:fmt "~A" 
                                                                                                     (cl-kb:frame-own-slot-value attr '|negotiation|::|m_has_name|)))
                                                                                       (:ele "value" 
                                                                                         (cl-who:fmt "~A" 
                                                                                                     (cl-kb:frame-own-slot-value 
                                                                                                      it 
                                                                                                      (cl-kb:frame-own-slot-value attr '|negotiation|::|m_slot|))))
                                                                                       ))))))))))))
                                         (:ele "proposals"
                                         
                                         (let ((pr (cl-kb:find-simple-instance "the process")))
                                           (dolist (msg (cl-kb:frame-own-slot-values pr '|negotiation|::|neg_case_proposal|))
                                             (cl-who:htm
                                                (:ele "message"
                                                  (:ele "ID"
                                                    (cl-who:fmt "~A" (cl-kb:frame-own-slot-value msg '|negotiation|::|neg_proposal_id|)))
                                                  (:ele "Timestamp"
                                                    (cl-who:fmt "~A" (clone-kb:date->string (cl-kb:frame-own-slot-value msg '|negotiation|::|neg_msg_timestamp|))))
                                                  (:ele "Sender"
                                                    (cl-who:fmt "~A"  (position (cl-kb:frame-own-slot-value 
                                                                       (cl-kb:frame-own-slot-value msg '|negotiation|::|neg_msg_sender|)
                                                                       '|onenegotiation|::|one_id|) parties :test #'string=)))
                                                  (:ele "Issues"
                                                    (cl-kb:cls-do-instance-list |negotiation|::|m_item| mit
                                                                                (cl-who:htm 
                                                                                 (:ele "item"
                                                                                   (:ele "name"
                                                                                     (cl-who:fmt "~A" 
                                                                                                 (cl-kb:frame-own-slot-value mit '|negotiation|::|m_has_name|)))
                                                                                   (:ele "issue"
                                                                                     (let ((issueset (cl-kb:frame-own-slot-value
                                                                                                      msg
                                                                                                      (cl-kb:frame-own-slot-value mit '|negotiation|::|m_issueset_slot|))))
                                                                                       (dolist (missue (cl-kb:frame-own-slot-values mit '|negotiation|::|m_has_issue|))
                                                                                         (cl-who:htm 
                                                                                          (:ele "name"
                                                                                            (cl-who:fmt "~A" (cl-kb:frame-own-slot-value missue '|negotiation|::|m_has_name|)))
                                                                                            (let ((issue (cl-kb:frame-own-slot-value issueset (cl-kb:frame-own-slot-value missue '|negotiation|::|m_slot|))))
                                                                                              (dolist (attr (cl-kb:frame-own-slot-values missue '|negotiation|::|m_has_attr|))
                                                                                                (cl-who:htm 
                                                                                                 (:ele "attribute"
                                                                                                   (:ele "name"
                                                                                                     (cl-who:fmt "~A" 
                                                                                                                 (cl-kb:frame-own-slot-value attr '|negotiation|::|m_has_name|)))
                                                                                                   (:ele "value" (cl-who:fmt "~A" 
                                                                                                                             (cl-kb:frame-own-slot-value issue 
                                                                                                                                                       (cl-kb:frame-own-slot-value attr '|negotiation|::|m_slot|)))))))))))))))))))))
                                         (:ele "agreement"
                                           (let ((p (cl-kb:find-simple-instance "the conclusion")))
                                             (cl-who:htm (:ele "Sate"
                                                               (cl-who:fmt "~A" (cl-kb:frame-own-slot-value p '|negotiation|::|neg_case_state|))))
                                             (if (cl-kb:frame-own-slot-value p '|negotiation|::|neg_case_agreement|)
                                                 (cl-who:htm (:ele "Date"
                                                               (cl-who:fmt "~A" (clone-kb:date->string
                                                                                 (cl-kb:frame-own-slot-value p '|negotiation|::|neg_case_conclusion_date|))))
                                                             (:ele "Agreement"
                                                               (cl-who:fmt "~A" (cl-kb:frame-own-slot-value
                                                                                 (cl-kb:frame-own-slot-value p '|negotiation|::|neg_case_agreement|)
                                                                                 |negotiation|::|neg_proposal_id|)))))))
                                           ))))
                                           
            




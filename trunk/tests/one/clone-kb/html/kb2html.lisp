;;;; Created on 2009-02-02 15:03:53

(in-package :clone-kb)

(defun kb->html (negid)
  (let ((case-id negid)
        (case-name nil)
        (case-date nil)
        (case-data (kb->html-as-string negid))
        (case-agreement (agreement-issues->html-as-string negid)))
    (let ((c (cl-kb:find-simple-instance "the context")))
      (setf case-name (cl-kb:frame-own-slot-value c '|onenegotiation|::|one_name|))
      (setf case-date (date->string
                       (cl-kb:frame-own-slot-value c '|onenegotiation|::|one_creation_date|)))
      (with-open-file (strm 
                       (merge-pathnames 
                        (make-pathname :type "html")
                        (cl-kb:kb-protege-pprj-file cl-kb:*kb*))
                       :direction :output :if-exists :supersede)
                      (format strm "~A" case-data))
      ;(with-open-file (strm 
      ;                 (merge-pathnames 
      ;                  (make-pathname :type "lsp")
      ;                  (cl-kb:kb-protege-pprj-file cl-kb:*kb*))
      ;                 :direction :output :if-exists :supersede)
      ;                (format strm "~S" (list case-id case-name case-date case-data case-agreement))))))
      )))



(defun kb->html-as-string (negid)
  (cl-who:with-html-output-to-string (strm)    
                                     (:h1 "Context")
                                     (:table :border 4 :cellpadding 4
                                       (let ((c (cl-kb:find-simple-instance "the context")))
                                         (if (null c)
                                             (error "invalid context"))
                                         (cl-who:htm (:tr :align "rigth"
                                                       (:td "Instance ID:")
                                                       (:td (cl-who:fmt "~A" (cl-kb:frame-own-slot-value c '|onenegotiation|::|one_id|)))))
                                         (cl-who:htm (:tr
                                                       (:td "Name:")
                                                       (:td (cl-who:fmt "~A" (cl-kb:frame-own-slot-value c '|onenegotiation|::|one_name|)))))
                                         (cl-who:htm (:tr 
                                                       (:td "Visibility:")
                                                       (:td (cl-who:fmt "~A" (cl-kb:frame-own-slot-value c '|onenegotiation|::|one_visibility|)))))
                                         (cl-who:htm (:tr 
                                                       (:td "Creation Date:")
                                                       (:td (cl-who:fmt "~A" (date->string
                                                          
                                                                                                  (cl-kb:frame-own-slot-value c '|onenegotiation|::|one_creation_date|))))))
                                         (cl-who:htm (:tr 
                                                       (:td "Owner:")
                                                       (:td (cl-who:fmt "~A"
                                                                        (if (cl-kb:frame-own-slot-value c '|negotiation|::|neg_case_owner|) 
                                                                            (cl-kb:frame-own-slot-value
                                                                             (cl-kb:frame-own-slot-value c '|negotiation|::|neg_case_owner|)
                                                                             '|onenegotiation|::|one_id|)
                                                                            "")))))
                                         (let ((parties 
                                                (cl-kb:frame-own-slot-values c '|negotiation|::|neg_case_participant|)))
                                           (cl-who:htm (:tr 
                                                         (:td "Partecipants:")
                                                         (:td 
                                                           (:table :border 1
                                                             (:tr 
                                                               (:td (:b "ID")))
                                                             (dolist (p parties)
                                                               (cl-who:htm 
                                                                (:tr 
                                                                  (:td
                                                                    (cl-who:fmt "~A" 
                                                                                (cl-kb:frame-own-slot-value p '|onenegotiation|::|one_id|)
                                                                                )))))))))
                                           (cl-who:htm (:tr 
                                                         (:td "Partecipants Number:")
                                                         (:td (cl-who:fmt "~A" (length parties)))))))
                                       (let ((m (cl-kb:find-simple-instance "this-model")))
                                         (cl-who:htm (:tr :align "rigth"
                                                       (:td "Model ID:")
                                                       (:td (cl-who:fmt "~A" (cl-kb:frame-own-slot-value m '|negotiation|::|neg_model_id|)))))
                                         (cl-who:htm (:tr
                                                       (:td "Model Name:")
                                                       (:td (cl-who:fmt "~A" (cl-kb:frame-own-slot-value m '|negotiation|::|neg_model_name|)))))
                                         (cl-who:htm (:tr 
                                                       (:td "Model Version:")
                                                       (:td (cl-who:fmt "~A" (cl-kb:frame-own-slot-value m '|negotiation|::|neg_model_version|)))))
                                         (let ((p (cl-kb:find-simple-instance "the protocol")))
                                           (cl-who:htm (:tr :align "rigth"
                                                         (:td "Starting Date:")
                                                         (:td (cl-who:fmt "~A" (date->string
                                                                                (cl-kb:frame-own-slot-value p '|negotiation|::|neg_case_starting_date|)))))
                                                       (:tr :align "rigth"
                                                         (:td "Ending Date:")
                                                         (:td (cl-who:fmt "~A" (date->string
                                                                                (cl-kb:frame-own-slot-value p '|negotiation|::|neg_case_ending_date|))))))))
                                       (:tr
                                         (:td "Items")
                                         (:td 
                                           (:table :border 2 :cellpadding 2
                                             (:tr 
                                               (:td (:b "Name"))
                                               (:td (:b "Attributes")))
                                             (let ((its (cl-kb:find-simple-instance "the itemset")))
                                               (cl-kb:cls-do-instance-list |negotiation|::|m_item| mit
                                                                           (cl-who:htm 
                                                                            (:tr 
                                                                              (:td 
                                                                                (cl-who:fmt "~A" 
                                                                                            (cl-kb:frame-own-slot-value mit '|negotiation|::|m_has_name|)))
                                                                              (let ((it (cl-kb:frame-own-slot-value
                                                                                         its
                                                                                         (cl-kb:frame-own-slot-value mit '|negotiation|::|m_slot|))))
                                                                                (cl-who:htm 
                                                                                 
                                                                                 (:td 
                                                                                   (:table :border 1
                                                                                     (dolist (attr (cl-kb:frame-own-slot-values mit '|negotiation|::|m_has_attr|))
                                                                                       (cl-who:htm 
                                                                                        (:tr
                                                                                          (:td (cl-who:fmt "~A" 
                                                                                                           (cl-kb:frame-own-slot-value attr '|negotiation|::|m_has_name|)))
                                                                                          (:td (cl-who:fmt "~A" 
                                                                                                           (let ((slo (cl-kb:frame-own-slot-value attr '|negotiation|::|m_slot|)))
                                                                                                             (if slo
                                                                                                                 (cl-kb:frame-own-slot-value 
                                                                                                                  it 
                                                                                                                  slo)
                                                                                                                 (format t "Error ~A  ~A~%" (cl-kb:frame-name it) (cl-kb:frame-name attr))))))
                                                                                                            
                                                                                          )))))
                                                                                 ))))))))))
                                     
                                     (:h1 "Proposals")
                                     (let ((pr (cl-kb:find-simple-instance "the process")))
                                       (dolist (msg (cl-kb:frame-own-slot-values pr '|negotiation|::|neg_case_proposal|))
                                         (cl-who:htm 
                                      (:table :border 4 :cellpadding 4
                                        (:tr :align "rigth"
                                          (:td "ID:")
                                          (:td (cl-who:fmt "~A" (cl-kb:frame-own-slot-value msg '|negotiation|::|neg_proposal_id|))))
                                        (:tr :align "rigth"
                                          (:td "Timestamp:")
                                          (:td (cl-who:fmt "~A" (date->string (cl-kb:frame-own-slot-value msg '|negotiation|::|neg_msg_timestamp|)))))
                                        (:tr :align "rigth"
                                          (:td "Sender:")
                                          (:td (cl-who:fmt "~A" (cl-kb:frame-own-slot-value (cl-kb:frame-own-slot-value msg '|negotiation|::|neg_msg_sender|)
                                                                                            '|onenegotiation|::|one_id|))))
                                        (:tr
                                          (:td "Issues:")
                                          (:td
                                            (cl-who:fmt "~A" (issues->html-as-string msg))))))))
                                            
                                         
                                     (:h1 "Agreement")
                                     (let ((p (cl-kb:find-simple-instance "the conclusion")))
                                       (if (cl-kb:frame-own-slot-value p '|negotiation|::|neg_case_agreement|)
                                           (cl-who:htm (:table :border 4 :cellpadding 4
                                                         (:tr :align "rigth"
                                                           (:td "Date:")
                                                           (:td (cl-who:fmt "~A" (date->string
                                                                                  (cl-kb:frame-own-slot-value p '|negotiation|::|neg_case_conclusion_date|)))))
                                                         (:tr :align "rigth"
                                                           (:td "Winner:")
                                                           (:td (cl-who:fmt "~A" (cl-kb:frame-own-slot-value
                                                                                  (cl-kb:frame-own-slot-value p '|negotiation|::|neg_case_winner|)
                                                                                  '|onenegotiation|::|one_id|))))
                                                         (:tr :align "rigth"
                                                           (:td "Agreement:")
                                                           (:td (cl-who:fmt "~A" (cl-kb:frame-own-slot-value
                                                                                  (cl-kb:frame-own-slot-value p '|negotiation|::|neg_case_agreement|)
                                                                                  '|negotiation|::|neg_proposal_id|))))))
                                           (cl-who:htm (:table :border 4 :cellpadding 4
                                                         (:tr :align "rigth"
                                                           (:td "Current/Final State:")
                                                           (:td (cl-who:fmt "~A" (cl-kb:frame-own-slot-value p '|negotiation|::|neg_case_state|))))))))))
                  

(defun agreement-issues->html-as-string (negid) 
  (declare (ignore negid))
  (let ((p (cl-kb:find-simple-instance "the conclusion")))
    (if (cl-kb:frame-own-slot-value p '|negotiation|::|neg_case_agreement|)
        (issues->html-as-string (cl-kb:frame-own-slot-value p '|negotiation|::|neg_case_agreement|)))))

(defun issues->html-as-string (msg)
  (cl-who:with-html-output-to-string (strm)  
                                     (cl-who:htm      
                                      (:table :border 2 :cellpadding 2
                                        (:tr 
                                          (:td (:b "Item"))
                                          (:td (:b "Issues"))
                                          (:td (:b "Attributes")))
                                        (cl-kb:cls-do-instance-list |negotiation|::|m_item| mit
                                                                    (cl-who:htm 
                                                                     (:tr
                                                                       (:td
                                                                         (cl-who:fmt "~A" 
                                                                                     (cl-kb:frame-own-slot-value mit '|negotiation|::|m_has_name|)))
                                                                       (let ((issueset (cl-kb:frame-own-slot-value
                                                                                        msg
                                                                                        (cl-kb:frame-own-slot-value mit '|negotiation|::|m_issueset_slot|))))
                                                                         (dolist (missue (cl-kb:frame-own-slot-values mit '|negotiation|::|m_has_issue|))
                                                                           (cl-who:htm 
                                                                            (:td (cl-who:fmt "~A" (cl-kb:frame-own-slot-value missue '|negotiation|::|m_has_name|)))
                                                                            (:td 
                                                                              (:table :border 1
                                                                                (let ((issue (cl-kb:frame-own-slot-value issueset (cl-kb:frame-own-slot-value missue '|negotiation|::|m_slot|))))
                                                                                  (dolist (attr (cl-kb:frame-own-slot-values missue '|negotiation|::|m_has_attr|))
                                                                                    (cl-who:htm 
                                                                                     (:tr
                                                                                       (:td (cl-who:fmt "~A" 
                                                                                                        (cl-kb:frame-own-slot-value attr '|negotiation|::|m_has_name|)))
                                                                                       (:td (cl-who:fmt "~A" 
                                                                                                        (cl-kb:frame-own-slot-value issue 
                                                                                                                                    (cl-kb:frame-own-slot-value attr '|negotiation|::|m_slot|)))))))))))))
                                                                       )))))))

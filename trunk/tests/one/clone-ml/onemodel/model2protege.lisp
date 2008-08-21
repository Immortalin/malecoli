;;;; Created on 2008-04-15 16:09:46

(in-package :clone-ml)

(progn
  (defvar *default-one-model-kb-pathname*)
  (eval-when (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
    (if (null (boundp '*default-one-model-kb-pathname*))
        (setq *default-one-model-kb-pathname*            
              #p"/hardmnt/tharpe0/sra/serra/Software/Developing/MaLeCoLi/workspace/extra/one/kb/"))))

(defun import-one-model (modelfile)
  (let ((model (load-model modelfile)))
    (let ((case-prefix (format nil "~A~A" (model-name model) (model-version model))))
      (let ((kb (make-kb case-prefix 
                         :use (list 
                               mlcl-kbs::PROTEGE-KB 
                               mlcl-kbs::negotiation-kb
                               mlcl-kbs::onenegotiation-kb)
                         :protege-file (merge-pathnames
                                        (make-pathname
                                         :directory '(:relative ".")
                                         :name case-prefix :type "xml" :case :local)
                                        *default-one-model-kb-pathname*))))
        (convert-one-model model kb)
        (kb-save kb)
        kb))))


(defun convert-one-model (model kb)
  (let ((case-prefix (format nil "~A~A" (model-name model) (model-version model))))
    (let ((case-id (format nil "case @ ~A" case-prefix)) 
          (context-id (format nil "context @ ~A" case-prefix))
          (conclusion-id (format nil "conclusion @ ~A" case-prefix))
          (protocol-id (format nil "protocol @ ~A" case-prefix))
          (process-id (format nil "process @ ~A" case-prefix))
          (item-id (format nil "item @ ~A" case-prefix))
          (issue-id (format nil "issue @ ~A" case-prefix))
          (proposal-id (format nil "proposal @ ~A" case-prefix)))
      (let ((cas (kb-make-cls case-id :kb kb)))
        (instance-add-direct-type cas protege-kb:|:STANDARD-CLASS|)
        (frame-add-own-slot-value cas protege-kb:|:ROLE| protege-kb:concrete-value)
        (cls-add-direct-supercls cas onenegotiation-kb:|one_case|))
      (let ((con (kb-make-cls context-id :kb kb)))
        (instance-add-direct-type con protege-kb:|:STANDARD-CLASS|)
        (frame-add-own-slot-value con protege-kb:|:ROLE| protege-kb:concrete-value)
        (cls-add-direct-supercls con onenegotiation-kb:|one_context|))
      (let ((con (kb-make-cls conclusion-id :kb kb)))
        (instance-add-direct-type con protege-kb:|:STANDARD-CLASS|)
        (frame-add-own-slot-value con protege-kb:|:ROLE| protege-kb:concrete-value)
        (cls-add-direct-supercls con onenegotiation-kb:|one_conclusion|))
      (let ((pro (kb-make-cls protocol-id :kb kb)))
        (instance-add-direct-type pro protege-kb:|:STANDARD-CLASS|)
        (frame-add-own-slot-value pro protege-kb:|:ROLE| protege-kb:concrete-value)
        (cls-add-direct-supercls pro onenegotiation-kb:|one_protocol|))
      (let ((pro (kb-make-cls process-id :kb kb)))
        (instance-add-direct-type pro protege-kb:|:STANDARD-CLASS|)
        (frame-add-own-slot-value pro protege-kb:|:ROLE| protege-kb:concrete-value)
        (cls-add-direct-supercls pro onenegotiation-kb:|one_process|))
      (let ((pro (kb-make-cls item-id :kb kb)))
        (instance-add-direct-type pro protege-kb:|:STANDARD-CLASS|)
        (frame-add-own-slot-value pro protege-kb:|:ROLE| protege-kb:concrete-value)
        (cls-add-direct-supercls pro onenegotiation-kb:|one_item|)
        (dolist (attr (item-attributes (im-item (model-im model))))
          (add-attribute model kb attr pro)))
      (let ((pro (kb-make-cls proposal-id :kb kb)))
        (instance-add-direct-type pro protege-kb:|:STANDARD-CLASS|)
        (frame-add-own-slot-value pro protege-kb:|:ROLE| protege-kb:concrete-value)
        (cls-add-direct-supercls pro onenegotiation-kb:|one_proposal|))
      (let ((iss (kb-make-cls issue-id :kb kb)))
        (instance-add-direct-type iss protege-kb:|:STANDARD-CLASS|)
        (frame-add-own-slot-value iss protege-kb:|:ROLE| protege-kb:concrete-value)
        (cls-add-direct-supercls iss onenegotiation-kb:|one_issue|)
        (dolist (is (item-issues (im-item (model-im model))))
          (add-issue model kb is issue-id proposal-id))))))


(defun add-issue (model kb is issue-id proposal-id)
  (let ((issuename (format nil "~A @ ~A~A " (item-issue-name is) (model-name model) (model-version model) )))
    (let ((issue (kb-make-cls issuename :kb kb)))
      (instance-add-direct-type issue protege-kb:|:STANDARD-CLASS|)
      (frame-add-own-slot-value issue protege-kb:|:ROLE| protege-kb:concrete-value)
      (cls-add-direct-supercls issue (kb-get-cls issue-id :kb kb))
      (dolist (attr (item-issue-attributes is))
        (add-attribute model kb attr issue))
      (let ((slotname (format nil "has ~A " issuename )))
        (let ((sl (kb-make-slot slotname :kb kb)))
          (instance-add-direct-type sl protege-kb:|:STANDARD-SLOT|)
          (cls-add-direct-template-slot (kb-get-slot proposal-id :kb kb) sl )
          (frame-add-own-slot-value sl protege-kb:|:SLOT-MAXIMUM-CARDINALITY| 1)
          (frame-add-own-slot-value sl protege-kb:|:SLOT-VALUE-TYPE| 
                                       `("Instance" ,issue)))))))

(defun add-attribute (model kb attr item)
  (let ((attrname (format nil "~A @ ~A~A " (item-attribute-name attr) (model-name model) (model-version model) )))
    (let ((at (kb-make-slot attrname :kb kb)))
      (instance-add-direct-type at protege-kb:|:STANDARD-SLOT|)
      (cls-add-direct-template-slot item at)
      (frame-add-own-slot-value at protege-kb:|:SLOT-MAXIMUM-CARDINALITY| 1)
      (multiple-value-bind (typ local) (model-get-type model (item-attribute-onetype attr))
        (if local
            (let ((typename (onetype-name typ))
                  (kind (onetype-kind typ)))
              (cond
               ((eq kind 'enum)
                (let ((v
                       (mapcan #'(lambda (x) (list x)) (onetype-vals typ))))
                  (frame-add-own-slot-value at protege-kb:|:SLOT-VALUE-TYPE|
                                               (append `("Symbol") v))))
               (t
                (format t "Not implemented, yet. ~A ~A~%" typename typ))))
            (let ((typename (onetype-name typ))) 
              (cond 
               ((string= typename "negmod:OneString")
                (frame-add-own-slot-value at protege-kb:|:SLOT-VALUE-TYPE| "String"))
               ((string= typename "negmod:OneInteger")
                (frame-add-own-slot-value at protege-kb:|:SLOT-VALUE-TYPE| "Integer"))
               ((string= typename "negmod:OneBoolean")
                (frame-add-own-slot-value at protege-kb:|:SLOT-VALUE-TYPE| "Boolean"))
               ((string= typename "negmod:OneDate")
                (frame-add-own-slot-value at protege-kb:|:SLOT-VALUE-TYPE| (list "Instance" negotiation-kb:|neg_date|)))
               ((string= typename "negmod:OneImage")
                (frame-add-own-slot-value at protege-kb:|:SLOT-VALUE-TYPE| (list "Instance" onenegotiation-kb::|one_image|)))
               ((string= typename "negmod:OneBinaryDocument")
                (frame-add-own-slot-value at protege-kb:|:SLOT-VALUE-TYPE| (list "Instance" onenegotiation-kb::|one_binary_document|)))
               ((string= typename "negmod:OneCurrency")
                (frame-add-own-slot-value at protege-kb:|:SLOT-VALUE-TYPE| "String"))
               ((string= typename "negmod:OneAmount")
                (frame-add-own-slot-value at protege-kb:|:SLOT-VALUE-TYPE| "Integer"))
               (t
                (format t "Not implemented, yet. ~A ~A~%" typename typ))
               ))))
      )))




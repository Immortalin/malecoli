;;;; Created on 2009-03-11 15:12:31

(in-package :clone-ml)

(defstruct caseinfo
  model-id 
  instance-id
  name
  date
  html
  agreement-html
  current-state 
  step-number
  participants)

(defun kb->caseinfo (instance-id)
  (let ((ci (make-caseinfo)))
    (setf (caseinfo-instance-id ci) instance-id)
    (setf (caseinfo-html ci) (clone-kb:kb->html-as-string instance-id))
    (setf (caseinfo-agreement-html ci) (clone-kb:agreement-issues->html-as-string instance-id))
    (let ((c (cl-kb:find-simple-instance "the context")))
      (setf (caseinfo-name ci) (cl-kb:frame-own-slot-value c '|onenegotiation|::|one_name|))
      (setf (caseinfo-date ci) (clone-kb:date->string
                                (cl-kb:frame-own-slot-value c '|onenegotiation|::|one_creation_date|)))
      (setf (caseinfo-participants ci) 
            (mapcar #'(lambda (x) (cl-kb:frame-own-slot-value x '|onenegotiation|::|one_id|))
                    (cl-kb:frame-own-slot-values c '|onenegotiation|::|neg_case_participant|))))
    (let ((c (cl-kb:find-simple-instance "the conclusion")))
      (setf (caseinfo-current-state ci) (cl-kb:frame-own-slot-value c '|onenegotiation|::|neg_case_state|)))
    (let ((c (cl-kb:find-simple-instance "the process")))
      (setf (caseinfo-step-number ci) (length (cl-kb:frame-own-slot-values c '|onenegotiation|::|neg_case_proposal|))))
    (let ((c (cl-kb:find-simple-instance "this-model")))
      (setf (caseinfo-model-id ci)
            (cl-kb:frame-own-slot-value c '|negotiation|::|neg_model_id|)))
    (with-open-file (strm 
                     (merge-pathnames 
                      (make-pathname :type "lsp" :name (format nil "caseinfo-~A" instance-id))
                      (cl-kb:kb-protege-pprj-file cl-kb:*kb*))
                     :direction :output :if-exists :supersede)
                    (format strm "~S" ci))
    ci))

(defun instance-id->caseinfo (instance-id)
  (let ((kb (clone-kb::find-model-instance-kb nil instance-id)))
    (cl-kb:with-kb kb t
                   (kb->caseinfo instance-id))))

(defun find-caseinfo (instance-id &optional (overwrite nil))
  (let ((casefilename (make-pathname :name (format nil "caseinfo-~A" instance-id) :type "lsp" 
                                     :directory (format nil "~A" cl-kb:*kb-default-path*))))
    (if (and 
         (probe-file casefilename)
         (not overwrite))
        (with-open-file (strm casefilename)
                        (read strm))
        (instance-id->caseinfo instance-id))))

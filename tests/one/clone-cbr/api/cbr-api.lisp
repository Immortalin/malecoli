;;;; Created on 2008-12-02 12:39:29

(in-package :clone-cbr)


(defvar *negotiation-instance* nil)
(defvar *negotiation-model* nil)
(defvar *messages* nil)
(defvar *endstate* nil)
(defvar *tmp-directory* '(:relative "tmp"))
(defvar *learned-instances* nil)

(defun update-model (neg-mod-id &optional (real t))
  (let ((instfilename (make-pathname :name neg-mod-id :type "nm" :directory *tmp-directory*)))
    (format t "update model  ~A   ~A~%" neg-mod-id (length *negotiation-model*))
    (if real
        (with-open-file (strm instfilename :direction :output :if-exists :supersede)
                        (format strm "~A" *negotiation-model*)))
    (let ((neg (clone-kb:xml-model-import instfilename)))
      (clone-kb:negotiation-model->kb neg t))))

(defun update-case (neg-inst-id &optional (real t))
  (if (not (is-learned-instance neg-inst-id))
      (let ((instfilename (make-pathname :name neg-inst-id :type "nmi" :directory *tmp-directory*))
            (logfilename (make-pathname :name (format nil "~A-messages" neg-inst-id) :type "xml" :directory *tmp-directory*))
            (statefilename (make-pathname :name (format nil "~A-state" neg-inst-id) :type "txt" :directory *tmp-directory*)))
        (if real
            (progn 
              (format t "update case  ~A   ~A ~A~%" neg-inst-id (length *negotiation-instance*) (length *messages*))
              (with-open-file (strm instfilename :direction :output :if-exists :supersede)
                              (format strm "~A" *negotiation-instance*))
              (with-open-file (strm logfilename :direction :output :if-exists :supersede)
                              (dolist (m *messages*)
                                (format strm "~A~%" m)))
              (with-open-file (strm statefilename :direction :output :if-exists :supersede)
                              (format strm "~S~%" *endstate*))))
        (let ((neg (clone-kb:xml-negotiation-import instfilename)))
          (clone-kb:negotiation->kb neg t)
          (clone-ml:find-caseinfo neg-inst-id t)))))


(defun learn-case (neg-inst-id)
  (if (not (is-learned-instance neg-inst-id))
      (progn
        (format t "Learn case= ~A~%" neg-inst-id)
        (clone-ml:cbr-add-negotiation-instance neg-inst-id)
        (add-learned-instance neg-inst-id)
        (clone-ml:cbr-save-workspaces))))


(defun process-case (neg-inst-id cbr-process-id)
  (format t "Process case= ~A ~A~%" neg-inst-id cbr-process-id)
  (let ((result (clone-ml:cbr-process-negotiation-instance neg-inst-id cbr-process-id)))
    (let ((current-case (car result))
          (similar-cases (cadr result))
          (most-similar-case (caadr result)))
      (let ((extrainfo (cond 
                         ((null most-similar-case)
                          nil)
                         ((string= cbr-process-id "partner_invitation")
                          (let ((users (clone-ml:caseinfo-participants (second most-similar-case))))
                            (list users)))
                         ((string= cbr-process-id "join_negotiation")
                          (let ((suc-rate 0.0))
                            (if similar-cases
                                (let ((prev-states (mapcar #'(lambda (x) (clone-ml:caseinfo-current-state (second x)))
                                                           similar-cases)))
                                  (dolist (a prev-states)
                                    (if (string= a "Agreed") 
                                        (setf suc-rate (1+ suc-rate))))
                                  (setf suc-rate (/ suc-rate (float (length prev-states))))))
                            (list (* 100 suc-rate))))
                         ((string= cbr-process-id "creating_offer")
                          (let ((offer (clone-ml:caseinfo-agreement-html (second most-similar-case))))
                            (list offer)))
                         ((string= cbr-process-id "evaluating_offer")
                          (let ((cur-steps (clone-ml:caseinfo-step-number current-case))
                                (exp-steps 0)
                                (weights 0)
                                (wei 1.0))
                            (let ((prev-steps (mapcar #'(lambda (x) (clone-ml:caseinfo-step-number (second x)))
                                                      similar-cases)))
                              (if prev-steps
                                  (dolist (a prev-steps)
                                    (setf exp-steps (+ exp-steps (* a wei)))
                                    (setf weights (+ weights wei))
                                    (setf wei (* wei 0.5)))
                                  (setf exp-steps (/ exp-steps weights)))
                            (list cur-steps exp-steps)))))))
        (list neg-inst-id 
              (list 
               (clone-ml:caseinfo-instance-id current-case)
               (clone-ml:caseinfo-name current-case)
               (clone-ml:caseinfo-date current-case)
               (clone-ml:caseinfo-html current-case))
              (if most-similar-case
                  (list 
                   (clone-ml:caseinfo-instance-id (second most-similar-case))
                   (clone-ml:caseinfo-name (second most-similar-case))
                   (clone-ml:caseinfo-date (second most-similar-case))
                   (clone-ml:caseinfo-html (second most-similar-case)))
                  nil) 
            extrainfo)))))

(defun get-case-kb (neg-inst-id)
  (format t "get case kb= ~A~%" neg-inst-id)
  "")

(defun get-case-kb-project (neg-inst-id)
  (format t "Get case kb process= ~A~%" neg-inst-id)
  "")

(defun get-case-log (neg-inst-id)
  (let ((log ""))
    (cl-kb:with-kb (cl-kb:find-kb (format nil "instance-~A" neg-inst-id) t t)
                   (setf log (kb->xml-log neg-inst-id)))
    log))

      
(defun get-learned-instances ()
  (if (null *learned-instances*)
      (let ((filename (make-pathname :name (format nil "caseinfo-learned") :type "lsp" 
                                     :directory (format nil "~A" cl-kb:*kb-default-path*))))
        (setf *learned-instances* (if (probe-file filename)
                                      (with-open-file (strm filename)
                                                      (read strm))
                                      nil))))
  *learned-instances*)
  
(defun is-learned-instance (inst)
  (member inst (get-learned-instances) :test #'string=))

(defun add-learned-instance (inst)
  (setf *learned-instances* (cons inst (get-learned-instances)))
  (let ((filename (make-pathname :name (format nil "caseinfo-learned") :type "lsp" 
                                 :directory (format nil "~A" cl-kb:*kb-default-path*))))
      (with-open-file (strm filename :direction :output :if-exists :supersede)
                          (format strm "~A" *learned-instances*))))

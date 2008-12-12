;;;; Created on 2008-12-02 12:39:29


(in-package :clone-cbr)

(defvar *negotiation-instance*)
(defvar *messages*)
(defvar *endstate*)

(defun update-case (neg-inst-id)
  (format t "update case  ~A   ~A ~A~%" neg-inst-id (length *negotiation-instance*) (length *messages*))
  (let ((instfilename (make-pathname :name neg-inst-id :type "nmi" :directory '(:relative "tmp")))
        (logfilename (make-pathname :name (format nil "~A-messages" neg-inst-id) :type "xml" :directory '(:relative "tmp"))))
    (with-open-file (strm instfilename :direction :output :if-exists :supersede)
                    (format strm "~A" *negotiation-instance*))
    (with-open-file (strm logfilename :direction :output :if-exists :supersede)
                    (dolist (m *messages*)
                      (format strm "~A~%" m)))
    (let* ((neg (clone-kb:xml-negotiation-import instfilename))
           (negkb (clone-kb:negotiation->kb neg t)))
      ;(update-metaproject negkb neg-inst-id))))
      )))


(defun learn-case (neg-inst-id)
  (format t "Learn case= ~A~%" neg-inst-id)
  nil)

(defun process-case (neg-inst-id cbr-process-id)
  (format t "Process case= ~A ~A~%" neg-inst-id cbr-process-id)
  "<H2>I'm sorry, I cannot give you any recommendation due to lack of similar cases in my knowledge base.</H2>")

(defun get-case-kb (neg-inst-id)
  (format t "get case kb= ~A~%" neg-inst-id)
  "")

(defun get-case-kb-project (neg-inst-id)
  (format t "Get case kb process= ~A~%" neg-inst-id)
  "")

(defun get-case-log (neg-inst-id)
  (format t "Get case log= ~A~%" neg-inst-id)
  "")


#|
(defun update-metaproject (negkb neg-inst-id)
  nil
  (cl-kb:with-kb (cl-kb:find-kb "metaproject") t
                 (let ((si (cl-kb:find-simple-instance (format nil "instance-~A" neg-inst-id) nil)))
                   (if (null si)
                       (cl-kb:mk-simple-instance (format nil "instance-~A" neg-inst-id) (cl-kb:find-cls "Project"))
                 )
  )

                 |#
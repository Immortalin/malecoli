;;;; Created on 2008-02-21 13:02:52

(in-package :common-lisp-user)

(defvar *output*)

(setf *output* #p"/tmp/output.xml")

(defun test01 ()
  (let ((kb (or 
             (cl-kb:find-kb "A")
             (cl-kb:make-kb "A" 
                              :protege-file #p"/tmp/a.pprj" 
                              :use-list '(cl-kbs::protege-kb)))))
    (if (not (cl-kb::kb-createdp kb)) (cl-kb:kb-create kb))
    (cl-kb:kb-open kb)
    (cl-kb:kb-save kb)
    (cl-kb:kb-close kb)
    kb))

(defun test02 ()
  (let ((kb (or 
             (cl-kb:find-kb "B")
             (cl-kb:make-kb "B" :use-list (list (cl-kb:find-kb "A")) :protege-file #p"/tmp/b.pprj"))))
    (if (not (cl-kb::kb-createdp kb)) (cl-kb:kb-create kb))
    (cl-kb:kb-open kb)
    (cl-kb:kb-save kb)
    (cl-kb:kb-close kb)
    kb))

;;;; Created on 2008-02-21 13:02:52

(in-package :common-lisp-user)

(defvar *output*)

(setf *output* #p"/tmp/output.xml")

(defun test01 ()
  (let ((kb (or 
             (mlcl-kb:find-kb "A")
             (mlcl-kb:make-kb "A" :protege-file #p"/tmp/a.pprj" :use-list (list mlcl-kbs:kb)))))
    (mlcl-kb:kb-open kb)
    (mlcl-kb:kb-save kb)
    (mlcl-kb:kb-close kb)
    kb))

(defun test02 ()
  (let ((kb (or 
             (mlcl-kb:find-kb "B")
             (mlcl-kb:make-kb "B" :use-list (list (mlcl-kb:find-kb "A")) :protege-file #p"/tmp/b.pprj"))))
    (mlcl-kb:kb-open kb)
    (mlcl-kb:kb-save kb)
    (mlcl-kb:kb-close kb)
    kb))

;;;; 2008-04-15 10:44:05
;;;; Behold, the power of lisp.

(in-package :common-lisp-user)

(progn
  (defvar *default-one-model-pathname*)
  (eval-when (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
    (if (null (boundp '*default-one-model-kb-pathname*))
        (setq *default-one-model-pathname*            
              #p"/hardmnt/tharpe0/sra/serra/Software/Developing/MaLeCoLi/workspace/extra/one/model/"))))


(defvar *english-auction-model* (merge-pathnames (make-pathname
                                                  :name "EnglishAuction" :type "nme" :case :local)
                                                 *default-one-model-pathname*))
  
 
(defvar *gare-model* (merge-pathnames (make-pathname
                                                  :name "Gare-Tender" :type "nme" :case :local)
                                                 *default-one-model-pathname*))
  

(defun test01 ()
  (let ((kb (clone-ml::onemodel-import *english-auction-model*)))
;    (cl-protege-kb:kb-export-to-xml-file *english-auction-output* :kb kb)
    kb))

  
(defun test02 ()
  (let ((kb (clone-ml::onemodel-import *gare-model*)))
;    (cl-protege-kb:kb-export-to-xml-file *gare-output* :kb kb)
    kb))                   
                   
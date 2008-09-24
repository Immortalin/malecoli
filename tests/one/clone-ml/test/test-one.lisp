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
  (test-one *english-auction-model*))
  
(defun test02 ()
  (test-one *gare-model*))                   

(defun test03 ()
  (let ((kb (cl-kb:find-kb 'cl-kbs::|gare|)))
    (let ((workspace (make-instance 'mlcl::workspace
                                    :file (merge-pathnames 
                                           (make-pathname :type "workspace")
                                           (cl-kb:kb-protege-pprj-file kb))
                                    :schema (make-instance 'mlcl:schema 
                                                           :file (cl-kb:kb-protege-pprj-file kb) 
                                                           :kb kb))))
      (if (eq (length (mlcl::storage-cases (mlcl::workspace-storage workspace))) 0)
          (progn
            (mlcl:workspace-save workspace)
            (format t "! ~A~%" workspace)
            (mlcl:workspace-cases-import workspace (cl-kb:find-kb 'cl-kbs::|gare-instances|))
            (format t "!!! ~A~%" workspace)))
      workspace
      )))


(defun test-one (modelfile)
  (let ((kb (clone-ml::onemodel-import modelfile)))
    (format t "!!! ~A~%" kb)
    (let ((workspace (make-instance 'mlcl::workspace
                                     :file (merge-pathnames 
                                           (make-pathname :type "workspace")
                                           (cl-kb:kb-protege-pprj-file kb)))))
      (mlcl:workspace-save workspace)
      workspace
      )))


;;;; Created on 2008-08-25 13:26:16

(in-package :clone-kb)

(progn
  (defvar *default-one-model-kb-pathname*)
  (eval-when (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
    (if (null (boundp '*default-one-model-kb-pathname*))
        (setq *default-one-model-kb-pathname*            
              #p"/hardmnt/tharpe0/sra/serra/Software/Developing/MaLeCoLi/runtime_ws/kbs/"))))

;
; names
;

(defun model-full-name (model)
  (format nil "model-~A" (model-id model)))

(defun instance-full-name (model) 
  (format nil "instance-~A" 
          (attribute-value (find-if #'(lambda (x) (string-equal (attribute-name x) "id")) (neginfo-attributes (model-neginfo model))))))

;
; find and make kbs
;


(defun find-model-kb (model)
  (cl-kb:find-kb (model-full-name model) nil t))

(defun find-instance-model-kb (model)
  (cl-kb:find-kb (instance-full-name model) nil t))

(defun make-model-kb (model &key (pathname (merge-pathnames
                                            (make-pathname
                                             :name (model-full-name model)
                                             :type "pprj" :case :local)
                                            *default-one-model-kb-pathname*)))
  (let ((kb (cl-kb:make-kb pathname 
                           :use '(cl-kbs::|onenegotiation|))))
    (cl-kb:kb-create kb)
    (let ((this (cl-kb:mk-simple-instance (format nil "model @ ~A" (model-full-name model))
                                          '|onenegotiation|::|one_model| 
                                          :kb kb)))
      (setf (cl-kb:frame-own-slot-value this '|negotiation|::|neg_model_id|) (model-name model))
      (setf (cl-kb:frame-own-slot-value this '|negotiation|::|neg_model_version|) (model-version model)))
    (cl-kb:kb-save kb)
    kb))
  
(defun make-instance-model-kb (model &key (pathname (merge-pathnames
                                                     (make-pathname
                                                      :name (instance-full-name model)
                                                      :type "pprj" :case :local)
                                                     *default-one-model-kb-pathname*)))
  (let ((kb (cl-kb:make-kb pathname 
                           :use (list (find-model-kb model)))))
    (cl-kb:kb-create kb)
    (cl-kb:kb-save kb)
    kb))


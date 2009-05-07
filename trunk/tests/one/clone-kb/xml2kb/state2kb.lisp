;;;; Created on 2009-02-09 10:57:46

(in-package :clone-kb)

(defun state->kb (state model)
  (let ((*model* model)
        (*model-id* (model-id model))
        (*model-instance-id* (model-instance-name model)))
    (let ((kb (get-model-instance-kb *model-id* *model-instance-id* #'model->kb-instance nil)))
      (cl-kb:with-kb kb t
                     (let ((cn (cl-kb:find-simple-instance 
                                (g-instance-conclusion-id))))
                       (if cn
                           (setf 
                            (cl-kb:frame-own-slot-value cn '|negotiation|::|neg_case_state|) state)))))))
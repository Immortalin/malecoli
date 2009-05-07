;;;; Created on 2008-11-19 16:07:01

(in-package :clone-kb)

(defun negotiation->kb (negotiation &optional (overwrite-instance-p nil))
  (if (null (find-model-kb (model-id (negotiation-model negotiation))))
      (error "Impossible to find the negotiartion model: ~A" (model-id (negotiation-model negotiation))))
  ;(model->kb (negotiation-model negotiation) overwrite-model-p)
  (model-instance->kb (negotiation-model negotiation)  overwrite-instance-p)
  (state->kb (negotiation-state negotiation) (negotiation-model negotiation))
  (messages->kb (negotiation-messages negotiation) (negotiation-model negotiation))
  (all-kb->html (negotiation-model negotiation)))

(defun negotiation-model->kb (negotiation-model &optional (overwrite-model-p nil))
  (model->kb negotiation-model overwrite-model-p))

(defun all-kb->html (model)
  (let ((*model* model)
        (*model-id* (model-id model))
        (*model-instance-id* (model-instance-name model)))
    (let ((kb (find-model-instance-kb)))
      (cl-kb:with-kb kb t
                     (kb->html *model-id*)))))

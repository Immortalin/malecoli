;;;; Created on 2008-11-19 16:07:01

(in-package :clone-kb)

(defun negotiation->kb (negotiation &optional (overwritep nil))
  (model->kb (negotiation-model negotiation) overwritep)
  (model-instance->kb (negotiation-model negotiation)  overwritep)
  (messages->kb (negotiation-messages negotiation) (negotiation-model negotiation)))


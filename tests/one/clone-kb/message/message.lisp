;;;; Created on 2008-11-17 17:15:00

(defstruct message
  (receiver)
  (sender)
  (id)
  (negotiation-id)
  (attributes)
  (item))

(defstruct actor
  (id)
  (role))


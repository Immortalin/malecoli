;;;; Created on 2008-11-17 17:15:00

(in-package :clone-kb)

; a message
(defstruct message
  (type)
  (receiver)
  (sender)
  (id)
  (negotiation-id)
  (attributes)
  (items)
  (responseto)
  (value))

(defstruct actor
  (id)
  (role))

(defstruct msg-attr 
  (name)
  (value)
  (type)
  (is-mandatory)
  (is-static)
  (file-name)
  (mine-type))
                    
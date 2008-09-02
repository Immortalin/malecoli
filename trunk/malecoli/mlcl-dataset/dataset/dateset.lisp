;;;; Created on 2008-09-01 09:29:03

(in-package :mlcl-dataset)

(defclass dataset ()
  ((name 
   :READER dataset-name)
   (kb
    :READER dataset-kb)))

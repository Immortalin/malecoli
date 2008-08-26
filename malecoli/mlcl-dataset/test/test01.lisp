;;;; Created on 2008-08-26 11:52:30

(defvar *arff-01*)

(setf *arff-01* #p"/hardmnt/tharpe0/sra/serra/Software/Developing/MaLeCoLi/runtime_ws/arff/UCI/autos.arff")

(defun test01 ()
  (format t "begin~%~%")
  (mlcl-dataset::arff-import *arff-01*))

  
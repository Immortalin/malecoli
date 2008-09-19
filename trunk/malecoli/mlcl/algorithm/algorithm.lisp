;;;; Created on 2008-09-17 15:45:10


(in-package :mlcl)

(defclass algorithm ()
  ())

(defclass algorithm-compiler ()
  ())

(defgeneric algorithm-compiler-compile (algorithm-compiler algo-frame))

;;;; Created on 2008-09-17 15:45:10

(in-package :mlcl)

;
; Algorithm
;

(defclass algorithm ()
  ((name 
    :TYPE string
    :READER algorithm-name
    :INITARG :name)))

(defgeneric algorithm-store (algorithm strm))

(defgeneric algorithm-restore (algorithm strm))

(defgeneric algorithm-init-arguments (algorithm))


;
; Algorithm Compiler
;

(defclass algorithm-compiler ()
  ())

(defgeneric algorithm-compiler-compile (algorithm-compiler algo-frame))


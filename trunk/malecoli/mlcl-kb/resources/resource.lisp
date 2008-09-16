;;;; Created on 2008-09-12 11:06:01

(in-package :mlcl-kb)

(defvar *cusp-developmentp*
  #+sbcl t
  #-sbcl nil)

(defun get-resource-pathname (name type)
  (if *cusp-developmentp* 
      (merge-pathnames
       (make-pathname
        :name name
        :type type)
       #p"/hardmnt/tharpe0/sra/serra/Software/Developing/MaLeCoLi/code.google.com/workspace/malecoli-trunk/malecoli/mlcl-kb/resources/")
      (merge-pathnames
       (make-pathname
        :name name 
        :type type)
       *load-truename*)))

(defun stream->seq (stream)
  (let ((seq (make-array (file-length stream) :element-type 'character :fill-pointer t)))
    (setf (fill-pointer seq) (read-sequence seq stream))
    seq))

(defmacro init-variable (var code)
  `(eval-when (:LOAD-TOPLEVEL :EXECUTE)
     (if (null (boundp ',var))
      (setq ,var ,code))))

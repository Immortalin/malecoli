;;;; Created on 2008-09-12 11:06:01

(in-package :mlcl-kb)

(defvar *cusp-developmentp*
  #+sbcl t
  #-sbcl nil)

(push (if *cusp-developmentp* 
          #p"/hardmnt/tharpe0/sra/serra/Software/Developing/MaLeCoLi/code.google.com/workspace/malecoli-trunk/malecoli/mlcl-kb/resources/"
          *load-truename*)
      *kb-paths*)

(setf *kb-default-path* #p"/tmp")


(defun file->seq (file)
 (with-open-file (in file :direction :input)
                 (stream->seq in)))

(defun stream->seq (stream)
  (let ((seq (make-array (file-length stream) :element-type 'character :fill-pointer t)))
    (setf (fill-pointer seq) (read-sequence seq stream))
    seq))

(defun seq->stream (seq stream)
  (write-sequence seq stream))

(defun seq->file (seq file)
  (with-open-file (out file :direction :output :if-exists :supersede) 
                  (seq->stream seq out)))

(defmacro init-variable (var code)
  `(eval-when (:LOAD-TOPLEVEL :EXECUTE)
     (if (null (boundp ',var))
      (setq ,var ,code))))

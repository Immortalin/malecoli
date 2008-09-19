
(in-package :mlcl-knn)

;(defclass knn (algorithm)
;  ())

(defclass knn ()
  ((workspace 
    :TYPE mlcl-dataset:workspace
    :READER knn-workspace)
   (k
    :ACCESSOR knn-k)
   (similarity-fn
    :TYPE symbol
    :ACCESSOR knn-similarity-fn)))
   
   

(defun knn-similarity (knn x y)
  (funcall (knn-similarity-fn knn) x y))

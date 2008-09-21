;;;; 2008-08-21 11:50:25


(in-package :common-lisp-user)

(defpackage :mlcl-knn
  (:nicknames :ml-knn)
  (:use :cl)
  (:export
    
    #|
    knn
    |#
    knn
    knn-k
    knn-similarity-fn
    knn-dataset-name
))



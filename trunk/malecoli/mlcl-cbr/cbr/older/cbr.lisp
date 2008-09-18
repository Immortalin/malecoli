

(in-package :mlcl-cbr)



(defclass kb ()
  ((cb
   :INITFORM (make-instance 'cb)
   :ACCESSOR kb-cb)))


(defclass cb ()
  ((cases
    :INITFORM nil
    :ACCESSOR cb-cases)))

(defun cb-add-case (cb c)
  (push c (cb-cases cb)))

(defclass cbr-case ()
  ((episode
    :INITFORM nil
    :ACCESSOR episode
    :INITARG :episode)))

(defgeneric similarity (c q))

(defmethod similarity (c q)
  (declare (ignore c q))
  0.0001)

(defmethod similarity ((c float) (q float))
  (abs (- q c)))


(defun get-rank (kb q)
  (sort 
   (mapcar #'(lambda (x) (list x (similarity (episode x) q))) 
	   (cb-cases (kb-cb kb))) 
   #'(lambda (x y) (< (second x) (second y)))))


(defun retrieve (kb q)
  (let ((s (get-rank kb q)))
    (values (first (first s)) (second (first s)))))


(defgeneric adaptation (c q))

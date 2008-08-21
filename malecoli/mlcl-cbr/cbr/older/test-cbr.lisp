


(defvar *kb*)
(setf *kb* (make-instance 'kb))

(cb-add-case (kb-cb *kb*) 
	     (make-instance 'cbr-case :episode (test-get-pc01)))
(cb-add-case (kb-cb *kb*) 
	     (make-instance 'cbr-case :episode (test-get-pc02)))
(cb-add-case (kb-cb *kb*) 
	     (make-instance 'cbr-case :episode (test-get-pc03)))

(describe-object (second (cb-cases (kb-cb *kb*))) *standard-output*)
(defvar *q*)
(setf *q* (test-get-pc04))

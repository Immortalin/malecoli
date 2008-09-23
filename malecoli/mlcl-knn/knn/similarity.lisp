;;;; Created on 2008-09-23 11:00:26

(in-package :mlcl-knn)

(defclass similarity-measure (mlcl:algorithm)
  ())

(defclass knn-similarity-compiler (mlcl:algorithm-compiler)
  ())

(defmethod mlcl:algorithm-compiler-compile ((algorithm-compiler knn-similarity-compiler) algo-frame schema-kb strm)
  (let ((cl-kb:*kb* schema-kb)
        (funname (format nil "~A" (cl-kb:frame-name algo-frame))))
    (format strm "(defmethod |~A| (x y)" funname)
    (format strm "~%	(mlcl-knn:knn-default-similarity x y))~%~%")
    (cl-kb:cls-do-subcls-list (cl-kb:find-cls '|dataset|::|DatasetThing|)
                              el
                              (if (not (cl-kb:frame-in-kb-p el (cl-kb:find-kb "dataset")))
                                  (similarity-function-gen funname el strm)))
    (dolist (own-val (cl-kb:frame-own-slot-values algo-frame '|knn|::|knn_similarity_algorithm_weights|))
      (let ((slot (cl-kb:frame-own-slot-value own-val '|knn|::|knn_similarity_weight_slot|))
            (we (cl-kb:frame-own-slot-value own-val '|knn|::|knn_similarity_weight|)))
        (format strm "(setf ~A ~A)~%" (format nil "|~A-~A-weight|" funname (cl-kb:frame-name slot)) we)))
    (list (cl-kb:frame-name algo-frame) `(make-instance 'similarity-measure 
                                                        :name ,(cl-kb:frame-name algo-frame)))))



;
;  similarity-function-gen 
;

(defun similarity-function-gen (name frame strm)
  (let ((weights nil))
    (format strm "(defmethod |~A| ((x |~A|) (y |~A|))" 
            name (cl-kb:frame-name frame) (cl-kb:frame-name frame))
    (format strm "~%	(let ((s (call-next-method)) (w 0))")
    (dolist (slot (cl-kb:cls-direct-template-slots frame))
      (let ((wd (format nil "|~A-~A-weight|" name (cl-kb:frame-name slot))))
        (push wd weights)
        (format strm "~%		(setf w (+ w ~A))" wd)
        (format strm "~%		(setf s (+ s (* ~A (|~A| (slot-value x '|~A|) (slot-value y '|~A|)))))"
                wd name (cl-kb:frame-name slot) (cl-kb:frame-name slot))))
    (format strm "~%		(float (/ s w))")
    (format strm "))~%~%")
    (dolist (w weights)
        (format strm "(defvar ~A 1.0)~%" w))))

   
      

;
;
;

(defgeneric knn-default-similarity  (x y)
  (:method (x y)
    0)
  (:method ((x float) (y float))
    (/ 1 (+ 1 (abs (- x y)))))
  (:method ((x integer) (y integer))
    (/ 1 (+ 1 (abs (- x y)))))
  (:method ((x string) (y string))
    (if (string-equal x y) 1 0)))


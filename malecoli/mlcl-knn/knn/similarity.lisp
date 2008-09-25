;;;
;;; MaLeCoLi
;;; Copyright (C) 2008 Alessandro Serra
;;; 
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;

;;;; Created on 2008-09-23 11:00:26

(in-package :mlcl-knn)

;
; similarity measure algorithm
;

(defclass similarity-measure (mlcl:algorithm)
  ())


;
; default similarity function
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


;
; similarity compiler
;

(defclass knn-similarity-compiler (mlcl:algorithm-compiler)
  ())

(defmethod mlcl:algorithm-compiler-compile ((algorithm-compiler knn-similarity-compiler) algo-frame schema-kb strm)
  (let ((cl-kb:*kb* schema-kb)
        (funname (format nil "~A" (cl-kb:frame-name algo-frame))))
    (format strm "(defgeneric |~A| (x y))~%~%" funname)
    (format strm "(defmethod |~A| (x y)" funname)
    (format strm "~%	(mlcl-knn:knn-default-similarity x y))~%~%")
    (let ((weights nil)
          (main nil)) 
      (setf main (with-output-to-string (out)
                                        (cl-kb:cls-do-subcls-list (cl-kb:find-cls '|dataset|::|DatasetThing|) el
                                                                  (if (or (cl-kb:frame-in-kb-p el schema-kb)
                                                                          (and  (not (cl-kb:frame-in-kb-p el (cl-kb:find-kb "dataset")))
                                                                                (member (cl-kb:frame-kb el) (cl-kb:kb-use-list schema-kb))))
                                                                      (progn 
                                                                        (setf weights (append (similarity-function-gen funname el out)
                                                                                              weights)))))))
      (dolist (w weights)
        (format strm "(defvar ~A 1.0)~%" w))
      (format strm "~%~%")
      (write-string main strm)
      (format strm "~%~%"))
    (dolist (own-val (cl-kb:frame-own-slot-values algo-frame '|knn|::|knn_similarity_algorithm_weights|))
      (let ((slot (cl-kb:frame-own-slot-value own-val '|knn|::|knn_similarity_weight_slot|))
            (we (cl-kb:frame-own-slot-value own-val '|knn|::|knn_similarity_weight|)))
        (format strm "(setf ~A ~A)~%" (format nil "|~A-~A-weight|" funname (cl-kb:frame-name slot)) we)))
    (list (cl-kb:frame-name algo-frame) `(make-instance 'similarity-measure 
                                                        :name ,(cl-kb:frame-name algo-frame)))))

(defun similarity-function-gen (name frame strm)
  (let ((weights nil))
    (format strm "(defmethod |~A| ((x |~A|) (y |~A|))" 
            name (cl-kb:frame-name frame) (cl-kb:frame-name frame))
    (format strm "~%	(if (or (null x) (null y))")
    (format strm "~%		0")
    (format strm "~%		(let ((s (call-next-method)) (w 1))")
    (dolist (slot (cl-kb:cls-direct-template-slots frame))
      (let ((wd (format nil "|~A-~A-weight|" name (cl-kb:frame-name slot))))
        (push wd weights)
        (format strm "~%			(setf w (+ w ~A))" wd)
        (format strm "~%			(if (and (slot-boundp x '|~A|) (slot-boundp y '|~A|))"  (cl-kb:frame-name slot) (cl-kb:frame-name slot))
        (format strm "~%			(setf s (+ s (* ~A (|~A| (slot-value x '|~A|) (slot-value y '|~A|))))))"
                wd name (cl-kb:frame-name slot) (cl-kb:frame-name slot))))
    (format strm "~%		(float (/ s w))")
    (format strm ")))~%~%")
    weights))

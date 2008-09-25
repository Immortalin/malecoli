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

;;;; 2008-08-21 09:30:59

(in-package :mlcl-knn)

;
; knn
;

(defclass knn (mlcl:algorithm)
  ((k
    :TYPE integer
    :ACCESSOR knn-k
    :INITFORM 1
    :INITARG :k)
   (similarity-fn
    :TYPE symbol
    :ACCESSOR knn-similarity-fn
    :INITFORM nil
    :INITARG :similarity-fn)
   (dataset-name
    :TYPE string
    :ACCESSOR knn-dataset-name
    :INITFORM "knn"
    :INITARG :dataset-name)))
      
;
; knn algorithms
;

(defun knn-init (knn workspace)
  (let ((ds (mlcl:workspace-find-dataset workspace (knn-dataset-name knn))))
    (if (null ds)
        (progn 
          (setf ds (mlcl:workspace-make-dataset workspace (knn-dataset-name knn)))
          (map nil #'(lambda (x) (mlcl:dataset-add-case ds x)) 
               (mlcl:storage-cases (mlcl:workspace-storage workspace)))))
    ds))

(defun knn-search (knn workspace cas)
  (let ((ds (mlcl:workspace-find-dataset workspace (knn-dataset-name knn)))
        (fn (symbol-function (find-symbol (knn-similarity-fn knn) (mlcl:schema-package (mlcl:workspace-schema workspace)))))
        (tops nil))
    (dolist (c (mlcl:dataset-cases ds))
      (let ((s (funcall fn c cas)))
        (push (cons s c) tops)))
    (let ((so (sort  tops #'(lambda (x y) (< (car x) (car y))))))
      (if (< (length so) (knn-k knn))
          (setf tops so)
          (setf tops (nthcdr (- (length so) (knn-k knn)) so))))
    tops))


;
; knn algorithm compiler
;

(defclass knn-algorithm-compiler (mlcl:algorithm-compiler)
  ())

(defmethod mlcl:algorithm-compiler-compile ((algorithm-compiler knn-algorithm-compiler) algo-frame schema-kb strm)
  (declare (ignore schema-kb))
  (let ((k (cl-kb:frame-own-slot-value algo-frame '|knn|::|knn_k|))
        (datasetname (cl-kb:frame-own-slot-value algo-frame '|knn|::|knn_dataset_name|))
        (simfn (cl-kb:frame-name (cl-kb:frame-own-slot-value algo-frame '|knn|::|knn_similarity_measure|))))
    (list (cl-kb:frame-name algo-frame) `(make-instance 'knn 
                                                        :name ,(cl-kb:frame-name algo-frame)
                                                        :k ,k
                                                        :similarity-fn ,simfn
                                                        :dataset-name ,datasetname))))


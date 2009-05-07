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
    nil)
  (:method ((x float) (y float))
    (/ 1.0 (+ 1 (abs (- x y)))))
  (:method ((x integer) (y integer))
    (/ 1.0 (+ 1 (abs (- x y)))))
  (:method ((x string) (y string))
    (if (string-equal x y) 1 0))
  (:method ((x mlcl:|time|) (y mlcl:|time|))
    (let ((usec-a (mlcl:time->usec x))
          (usec-b (mlcl:time->usec y)))
      (knn-default-similarity usec-a usec-b))))
      
         

;
; similarity compiler
;

(defclass knn-similarity-compiler (mlcl:algorithm-compiler)
  ())


(defmethod mlcl:algorithm-compiler-compile ((algorithm-compiler knn-similarity-compiler) algo-frame schema)
  (let ((funsymb (cl-kb:string->symbol (format nil "~A" (cl-kb:frame-own-slot-value algo-frame '|algorithm|::|algorithm_function_name|))
                                        (mlcl:schema-package schema) t))
        (funsymbp (cl-kb:string->symbol (format nil "~A%" (cl-kb:frame-own-slot-value algo-frame '|algorithm|::|algorithm_function_name|))
                                        (mlcl:schema-package schema) t))
        (codes nil)
        (weights nil)
        (maincodes nil))
    (push `(defun ,funsymbp (x y slot)  
             (let ((sim
                    (cond 
                      ((and (not (SLOT-BOUNDP MLCL-KNN::X slot))
                            (not (SLOT-BOUNDP MLCL-KNN::Y slot)))
                      1.0)
                      ((or (not (SLOT-BOUNDP MLCL-KNN::X slot))
                           (not (SLOT-BOUNDP MLCL-KNN::Y slot)))
                       0)
                      ((and (null (SLOT-VALUE MLCL-KNN::X slot))
                            (null (SLOT-VALUE MLCL-KNN::Y slot)))
                       1.0)
                      ((or (null (SLOT-VALUE MLCL-KNN::X slot))
                           (null (SLOT-VALUE MLCL-KNN::Y slot)))
                       0)
                      (t
                       (,funsymb
                        (SLOT-VALUE MLCL-KNN::X slot)
                        (SLOT-VALUE MLCL-KNN::Y slot)
                        slot)))))
               ;(format t "~A ~A ~A ~A ~%" x y slot sim)
               sim))
          codes)
    (push `(defmethod ,funsymb (x y slot)
               (declare (ignore slot))
             (let ((sim (mlcl-knn:knn-default-similarity x y)))
               (if sim 
                   (values sim sim 1.0)
                   (values 0.0 0.0 0.0))))
          codes)
    (push `(defgeneric ,funsymb (x y slot)) 
          codes)
    (let ((clses (make-hash-table)))
      (setf (gethash nil clses) (list nil nil nil))
      (cl-kb:cls-do-subcls-list (cl-kb:find-cls '|dataset|::|DatasetThing|) el 
                                (if (or (cl-kb:frame-in-kb-p el (mlcl:schema-kb schema)) 
                                        (and  (not (cl-kb:frame-in-kb-p el (cl-kb:find-kb "dataset")))
                                              (member (cl-kb:frame-kb el) (cl-kb:kb-use-list (mlcl:schema-kb schema)))))
                                    (setf (gethash el clses) (list nil nil nil))))
      (cl-kb:cls-do-instance-list (cl-kb:find-cls '|knn|::|SimilarityMethod|) inst
                                  (if (eq (cl-kb:frame-own-slot-value inst '|knn|::|knn_similary_fn|)
                                          algo-frame)
                                      (let ((funcls (cl-kb:frame-own-slot-value inst '|knn|::|knn_similarity_algorithm_cls|))
                                            (funslot  (cl-kb:frame-own-slot-value inst '|knn|::|knn_similarity_algorithm_slot|))
                                            (funrec  (cl-kb:frame-own-slot-value inst '|knn|::|knn_similarity_algorithm_cls_rec|))
                                            (funcode  (cl-kb:frame-own-slot-value inst '|knn|::|knn_similarity_algorithm_code|)))
                                        (if funrec
                                            (cl-kb:cls-do-subcls-list funcls el
                                                                      (if (or (cl-kb:frame-in-kb-p el (mlcl:schema-kb schema)) 
                                                                              (and  (not (cl-kb:frame-in-kb-p el (cl-kb:find-kb "dataset")))
                                                                                    (member (cl-kb:frame-kb el) (cl-kb:kb-use-list (mlcl:schema-kb schema)))))
                                                                          (push inst (nth
                                                                                      (if funslot 
                                                                                          2 
                                                                                          (if funcode 
                                                                                              1
                                                                                              0))
                                                                                      (gethash el clses))))))
                                        (push inst (nth
                                                    (if funslot 
                                                        2 
                                                        (if funcode 
                                                            1
                                                            0))
                                                    (gethash funcls clses))))))
      (maphash #'(lambda (key mets) 
                   (labels ((makefn (met)
                              (let ((funslot  (cl-kb:frame-own-slot-value met '|knn|::|knn_similarity_algorithm_slot|))
                                    (funcls  (cl-kb:frame-own-slot-value met '|knn|::|knn_similarity_algorithm_cls|))
                                    (funcode  (cl-kb:frame-own-slot-value met '|knn|::|knn_similarity_algorithm_code|)))
                                (if (or (null funcode) (eq funcls key))
                                    (let ((s (similarity-function-gen funsymb funsymbp key funslot funcode (mlcl:schema-package schema))))
                                      (setf weights (append  (car s) weights))
                                      (push (car (cdr s)) maincodes))))))
                     (if (nth 2 mets) 
                         (dolist (m (nth 2 mets)) 
                           (makefn m)))
                     (if (nth 1 mets) 
                         (makefn (car (nth 1 mets)))
                         (if (nth 0 mets) (makefn (car (nth 0 mets)))))))  
               clses))
    (let ((weies (make-hash-table)))
      (dolist (w weights)
        (setf (gethash w weies) 1.0))
      (let ((weight-slots nil))
        (cl-kb:cls-do-instance-list (cl-kb:find-cls '|knn|::|KnnSimilarityWeigh|) inst
                                    (if (eq (cl-kb:frame-own-slot-value inst '|knn|::|knn_similary_fn|)
                                            algo-frame)
                                        (push inst weight-slots)))
        (mapcar #'(lambda(own-val) 
                    (let* ((slot (cl-kb:frame-own-slot-value own-val '|knn|::|knn_similarity_weight_slot|))
                           (we (cl-kb:frame-own-slot-value own-val '|knn|::|knn_similarity_weight|))
                           (wd (cl-kb:string->symbol (format nil "~A-~A-weight" (symbol-name funsymb) (cl-kb:frame-name slot)) (mlcl:schema-package schema) t)))
                      (setf (gethash wd weies) we)))
                weight-slots))
      (maphash #'(lambda (key val) 
                   (push `(defvar ,key ,val) codes))
               weies))
    (setf codes (append 
                 codes 
                 maincodes))
    (cons
     `(defvar ,(cl-kb:frame->symbol algo-frame (mlcl:schema-package schema) t)
        (make-instance 'similarity-measure 
                       :name ,(cl-kb:frame-name algo-frame)))
     codes)))


(defun similarity-function-gen (funsymb funsymbp funcls funslot funcode package)
  (let* ((codes nil)
         (weights nil))
    (setf codes `(defmethod ,funsymb ((x ,(if funcls (cl-kb:frame->symbol funcls package) t)) 
                                      (y ,(if funcls (cl-kb:frame->symbol funcls package) t))
                                      (slot ,(if funslot `(eql ',(cl-kb:frame->symbol funslot package t)) t)))
                   ,@(if funcode
                         (let ((co nil))
                           (with-input-from-string (strm funcode)
                                                   (do ((c (read strm nil) (read strm nil)))
                                                       ((null c) nil)
                                                     (push c co)))
                           (nreverse co))
                         `((MULTIPLE-VALUE-BIND (sim s w) (call-next-method)
                             (declare (ignore sim))
                             ,@(if funcls
                                   (mapcar 
                                    #'(lambda (slot) (let ((wd (cl-kb:string->symbol (format nil "~A-~A-weight" (symbol-name funsymb) (cl-kb:frame-name slot)) package))
                                                           (sd (cl-kb:string->symbol (cl-kb:frame-name slot) package)))
                                                       (push wd weights) 
                                                       `(progn      
                                                          (setf w (+ w ,wd))
                                                          (setf s (+ s (* ,wd (,funsymbp x y ',sd )))))))
                                    (cl-kb:cls-direct-template-slots funcls)))             
                             (values (float (if (> w 0) (/ s w) 1)) s w))))))
    (list weights codes)))

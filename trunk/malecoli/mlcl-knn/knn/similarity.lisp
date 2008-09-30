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

(defmethod mlcl:algorithm-compiler-compile ((algorithm-compiler knn-similarity-compiler) algo-frame schema)
  (let* ((funsymb (cl-kb:string->symbol (format nil "~A" (cl-kb:frame-own-slot-value algo-frame '|knn|::|knn_similarity_algorithm_name|))
                                        (mlcl:schema-package schema) t))
         (funslot  (cl-kb:frame-own-slot-value algo-frame '|knn|::|knn_similarity_algorithm_slot|))
         (funcls  (cl-kb:frame-own-slot-value algo-frame '|knn|::|knn_similarity_algorithm_cls|))
         (funcode  (cl-kb:frame-own-slot-value algo-frame '|knn|::|knn_similarity_algorithm_code|)))
    (let ((codes nil)
          (weights nil)
          (maincodes nil))
      (if (null funslot)
          (progn
            (push `(defmethod ,funsymb (x y slot)
                     (declare (ignore slot))
                     (mlcl-knn:knn-default-similarity x y)) codes)
            (push `(defgeneric ,funsymb (x y slot)) codes)
            
            (cl-kb:cls-do-subcls-list (cl-kb:find-cls '|dataset|::|DatasetThing|) el
                                      (if (or (cl-kb:frame-in-kb-p el (mlcl:schema-kb schema)) 
                                              (and  (not (cl-kb:frame-in-kb-p el (cl-kb:find-kb "dataset")))
                                                    (member (cl-kb:frame-kb el) (cl-kb:kb-use-list (mlcl:schema-kb schema)))))
                                          (progn 
                                            (let ((s (similarity-function-gen funsymb el nil nil (mlcl:schema-package schema))))
                                              (setf weights (append  (car s) weights))
                                              (push (car (cdr s)) maincodes))))))
          (progn            
            (let ((s (similarity-function-gen funsymb funcls funslot funcode (mlcl:schema-package schema))))
              (setf weights (car s))
              (push (car (cdr s)) maincodes))))
      (dolist (w weights)
        (push `(defvar ,w 1.0) codes))
      (setf codes (append 
                   codes 
                   maincodes
                   (let ((weight-slots nil))
                     (cl-kb:cls-do-instance-list (cl-kb:find-cls '|knn|::|KnnSimilarityWeigh|) inst
                                                 (if (eq (cl-kb:frame-own-slot-value inst '|knn|::|knn_similary_weight_fn|)
                                                         algo-frame)
                                                     (push inst weight-slots)))
                     (mapcar #'(lambda(own-val) 
                                 (let* ((slot (cl-kb:frame-own-slot-value own-val '|knn|::|knn_similarity_weight_slot|))
                                        (we (cl-kb:frame-own-slot-value own-val '|knn|::|knn_similarity_weight|))
                                        (wd (cl-kb:string->symbol (format nil "~A-~A-weight" (symbol-name funsymb) (cl-kb:frame-name slot)) (mlcl:schema-package schema) t)))
                                   `(setf ,wd ,we)))
                             weight-slots))))
      (cons
       `(defvar ,(cl-kb:frame->symbol algo-frame (mlcl:schema-package schema) t)
          (make-instance 'similarity-measure 
                         :name ,(cl-kb:frame-name algo-frame)))
       codes))))


(defun similarity-function-gen (funsymb funcls funslot funcode package)
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
                         `((if (or (null x) (null y))
                               0
                               (let ((s (call-next-method)) (w 1))
                                 ,@(mapcar 
                                    #'(lambda (slot) (let ((wd (cl-kb:string->symbol (format nil "~A-~A-weight" (symbol-name funsymb) (cl-kb:frame-name slot)) package))
                                                           (sd (cl-kb:string->symbol (cl-kb:frame-name slot) package)))
                                                       (push wd weights) 
                                                       `(progn      
                                                          (setf w (+ w ,wd))
                                                          (if (and (slot-boundp x ',sd) 
                                                                   (slot-boundp y ',sd))
                                                              (setf s (+ s (* ,wd 
                                                                              (,funsymb (slot-value x ',sd) 
                                                                                        (slot-value y ',sd)
                                                                                        ',sd))))))))
                                    (cl-kb:cls-direct-template-slots funcls))             
                                 (float (/ s w))))))))
    (list weights codes)))

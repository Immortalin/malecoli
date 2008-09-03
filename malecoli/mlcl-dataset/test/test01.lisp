;;;; Created on 2008-08-26 11:52:30

(defvar *arff-01*)

(defvar UCI-arffs '("anneal" "anneal.ORIG" "audiology" "autos"
                           "balance-scale" "breast-cancer" "breast-w"
                           "colic" "colic.ORIG" "credit-a" "credit-g"
                           "diabetes" "glass" "heart-c" "heart-h"
                           "heart-statlog" "hepatitis" "hypothyroid"
                           "ionosphere" "iris" "kr-vs-kp" "labor"
                           "letter" "lymph" "mushroom" "primary-tumor"
                           "segment" "sick" "sonar" "soybean" "splice"
                           "vehicle" "vote" "vowel" "waveform-5000" 
                           "zoo"))

(defvar UCI-dir #p"/hardmnt/tharpe0/sra/serra/Software/Developing/MaLeCoLi/runtime_ws/arff/UCI/")
 
(setf *arff-01* #p"/hardmnt/tharpe0/sra/serra/Software/Developing/MaLeCoLi/runtime_ws/arff/UCI/anneal.arff")

(defun test01 ()
  (test-one "zoo"))

(defun test02 ()
  (test-ds-one "zoo"))

(defun test-all ()
  (dolist (arff UCI-arffs)
    (test-one arff)))

(defun test-one (arff)
  (format t "@@ ~A~%" arff)
  (multiple-value-bind 
    (kb kbd) (mlcl-dataset::arff-import (merge-pathnames
                                         (make-pathname 
                                          :name arff
                                          :type "arff")
                                         UCI-dir))
    (mlcl-kb:kb-close kb)
    (mlcl-kb:kb-close kbd)))

(defun test-ds-all ()
  (dolist (arff UCI-arffs)
    (test-ds-one arff)))
  
(defun test-ds-one (arff)
  (format t "@@ ~A~%" arff)
  (let ((ds (mlcl-dataset::make-dataset arff (merge-pathnames
                                              (make-pathname 
                                               :directory '(:relative "mlcl-tmp")
                                               :name arff)
                                              UCI-dir))))))
  
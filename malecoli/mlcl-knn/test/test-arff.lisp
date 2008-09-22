;;;; Created on 2008-09-22 17:29:33


(defvar UCI-arffs '(
                    "anneal" 
                    "anneal.ORIG"
                    "audiology" "autos"
                    "balance-scale" "breast-cancer" "breast-w"
                    "colic" "colic.ORIG" "credit-a" "credit-g"
                    "diabetes" "glass" "heart-c" "heart-h"
                    "heart-statlog" "hepatitis" "hypothyroid"
                    "ionosphere" "iris" "kr-vs-kp" "labor"
                    "letter" "lymph" 
                    "mushroom" 
                    "primary-tumor"
                    "segment" "sick" "sonar" "soybean" "splice"
                    "vehicle" "vote" "vowel" "waveform-5000" 
                    "zoo"))


(defvar UCI-dir #p"/hardmnt/tharpe0/sra/serra/Software/Developing/MaLeCoLi/runtime_ws/arff/UCI/") 
(setf cl-kb:*kb-default-path* #p"/hardmnt/tharpe0/sra/serra/Software/Developing/MaLeCoLi/runtime_ws/arff-kbs/")

;(defvar UCI-dir #p"/home/alex/Software/Developing/MaLeCoLi/runtime-ws/arff/UCI/")
;(setf cl-kb:*kb-default-path* #p"/home/alex/Software/Developing/MaLeCoLi/runtime-ws/arff-kbs/")


(defun test01 ()
  (test-one "zoo"))

(defun test-all ()
  (dolist (arff UCI-arffs)
    (time (test-one arff))))

(defun test-one (arff)
  (format t "@@ ~A~%" arff)
  (let* ((workspace (make-instance 'mlcl::workspace
                                   :file (merge-pathnames
                                          (make-pathname 
                                           :name arff
                                           :type "workspace")
                                          cl-kb:*kb-default-path*)))
         (storage (mlcl::workspace-storage workspace))
         (schema (mlcl::workspace-schema workspace)))
    (if (eq (length (mlcl::workspace-algorithms workspace)) 0)
        (mlcl::workspace-make-algorithms workspace (merge-pathnames
                                                 (make-pathname 
                                                  :name "make-knn-01"
                                                  :type "pprj")
                                                 cl-kb:*kb-default-path*)))
    (format t "#cases=~A~%" (length (mlcl::storage-cases storage)))
    (format t "#datasets=~A~%" (length (mlcl::workspace-datasets workspace)))        
    (format t "#algorithms=~A~%" (length (mlcl::workspace-algorithms workspace)))
    workspace
    ))


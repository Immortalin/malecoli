
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
 

(defun test01 ()
  (test-one "zoo"))


(defun test-all ()
  (dolist (arff UCI-arffs)
    (time (test-one arff))))

(defun test-one (arff)
  (format t "@@ ~A~%" arff)
  (let* ((workspace (make-instance 'mlcl-dataset::workspace
                                   :file (merge-pathnames
                                          (make-pathname 
                                           :directory '(:relative "mlcl-tmp")
                                           :name arff
                                           :type "workspace")
                                          UCI-dir)))
         (storage (mlcl-dataset::workspace-storage workspace))
         (schema (mlcl-dataset::workspace-schema workspace)))
    (format t "#cases=~A~%" (length (mlcl-dataset::storage-cases storage)))
    (format t "#datasets=~A~%" (length (mlcl-dataset::workspace-datasets workspace)))
    (mlcl-dataset::workspace-datasets workspace)
    (let ((mk (make-instance 'mlcl-algorithm::makefile 
                             :file (merge-pathnames
                                    (make-pathname 
                                     :name "make-01"
                                     :directory '(:relative "mlcl-tmp")
                                     :type "pprj")
                                    UCI-dir)
                             :schema schema))))))
                 

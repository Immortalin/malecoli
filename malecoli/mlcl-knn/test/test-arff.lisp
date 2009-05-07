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


(defvar UCI-dir #p"/home/serra/Software/Developing/MaLeCoLi/runtime_ws/examples/arff/UCI/") 
(setf cl-kb:*kb-default-path* #p"/home/serra/Software/Developing/MaLeCoLi/runtime_ws/examples/arff-kbs/")

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
         (storage (mlcl::workspace-storage workspace)))
    ;(format t "#cases=~A~%" (length (mlcl::storage-cases storage)))
    ;(format t "#datasets=~A~%" (length (mlcl::workspace-datasets workspace)))        
    ;(format t "#algorithms=~A~%" (length (mlcl::workspace-algorithms workspace)))
    ;(format t "#makefiles=~A~%" (length (mlcl::workspace-makefiles workspace)))
    
    (if (eq (length (mlcl::workspace-makefiles workspace)) 0)
        (progn
          (mlcl::workspace-make-algorithms workspace (merge-pathnames
                                                      (make-pathname 
                                                       :name "make-knn-01"
                                                       :type "pprj")
                                                      cl-kb:*kb-default-path*))
          (mlcl:workspace-save workspace)))
    (format t "#cases=~A~%" (length (mlcl::storage-cases storage)))
    (format t "#datasets=~A~%" (length (mlcl::workspace-datasets workspace)))        
    (format t "#algorithms=~A~%" (length (mlcl::workspace-algorithms workspace)))
    (format t "#makefiles=~A~%" (length (mlcl::workspace-makefiles workspace)))
    (test-knn workspace)
    ))

(defun test-knn (workspace)
  (let ((knn (mlcl:workspace-find-algorithm workspace "make-knn-01")))
    (let ((cas (nth 10 (mlcl:dataset-cases (mlcl:workspace-find-dataset workspace (mlcl-knn:knn-dataset-name knn))))))
      (let ((tops (mlcl-knn:knn-search knn workspace cas)))
        (list workspace knn tops)))))


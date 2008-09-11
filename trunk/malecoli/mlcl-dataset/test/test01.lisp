;;;; Created on 2008-08-26 11:52:30

(defvar *arff-01*)

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
 
(setf *arff-01* #p"/hardmnt/tharpe0/sra/serra/Software/Developing/MaLeCoLi/runtime_ws/arff/UCI/anneal.arff")

(defun test01 ()
  (test-one "zoo"))

(defun test02 ()
  (test-ds-one "zoo"))
(defun test03 ()
  (test-ds-one "audiology")
  )

(defun test-all ()
  (dolist (arff UCI-arffs)
    (time (test-one arff))))

(defun test-one (arff)
  (format t "@@ ~A~%" arff)
  (multiple-value-bind 
    (kb kbd) (mlcl-dataset::arff->dataset-kb 
              (merge-pathnames
               (make-pathname 
                :name arff
                :type "arff")
               UCI-dir))
    (values kb kbd)))

(defun test-ds-all ()
  (dolist (arff UCI-arffs)
    (test-ds-one arff)))
  

(defun test-ds-one (arff)
  (format t "@@ ~A~%" arff)
  (let* ((workspace (make-instance 'mlcl-dataset::workspace
                                  :pathname (merge-pathnames
                                             (make-pathname 
                                              :directory '(:relative "mlcl-tmp")
                                              :name arff)
                                             UCI-dir)))
         (storage (mlcl-dataset::workspace-storage workspace))
         (schema (mlcl-dataset::workspace-schema workspace)))
    (if (eq (length (mlcl-dataset::storage-cases storage)) 0)
        (mlcl-dataset::workspace-cases-import workspace 
                                              (or
                                               (mlcl-kb::find-kb (format nil "~A-data" arff) nil)
                                               (make-instance 'mlcl-kb:kb 
                                                              :protege-file (merge-pathnames
                                                                             (make-pathname 
                                                                              :directory '(:relative "mlcl-tmp")
                                                                              :name (format nil "~A-data" arff))
                                                                             UCI-dir)
                                                              :use-list (list 'mlcl-kbs::dataset-kb 'mlcl-kbs::protege-kb 
                                                                              (mlcl-dataset::schema-kb schema))))))
    (format t "#cases=~A~%" (length (mlcl-dataset::storage-cases storage)))
    (format t "#datasets=~A~%" (length (mlcl-dataset::workspace-datasets workspace)))
    (mlcl-dataset::workspace-datasets workspace)
    ))
;   
;    (let ((ds (make-instance 'mlcl-dataset::dataset
;                             :pathname (merge-pathnames
;                                        (make-pathname 
;                                         :directory '(:relative "mlcl-tmp")
;                                         :name arff)
;                                        UCI-dir)
;                             :schema schema)))
;      (mlcl-dataset::dataset-import ds (or
;                                        (mlcl-kb::find-kb (format nil "~A-data" arff) nil)
;                                        (make-instance 'mlcl-kb:kb 
;                                                       :protege-file (merge-pathnames
;                                                                      (make-pathname 
;                                                                       :directory '(:relative "mlcl-tmp")
 ;                                                                      :name (format nil "~A-data" arff))
 ;                                                                     UCI-dir)
;                                                       :use-list (list 'mlcl-kbs::dataset-kb 'mlcl-kbs::protege-kb))))
;      ds)))
      

  #|
  (let ((ds (mlcl-dataset::make-dataset arff ))
    (mlcl-dataset::dataset-import-data ds (merge-pathnames
                             (make-pathname 
                              :directory '(:relative "mlcl-tmp")
                              :name (format nil "~A-data" arff))
                             UCI-dir))))
;    (clsql:select '|~A-ds|::|~A-case| :refresh t))
|#
  
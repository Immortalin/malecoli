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


(defvar UCI-dir #p"/home/serra/Software/Developing/MaLeCoLi/runtime_ws/examples/arff/UCI/") 
(setf cl-kb:*kb-default-path* #p"/home/serra/Software/Developing/MaLeCoLi/runtime_ws/examples/arff-kbs/")

;(defvar UCI-dir #p"/home/alex/Software/Developing/MaLeCoLi/runtime-ws/arff/UCI/")
;(setf cl-kb:*kb-default-path* #p"/home/alex/Software/Developing/MaLeCoLi/runtime-ws/arff-kbs/")


(defun test01 ()
  (test-one "zoo"))


(defun test02 ()
  ;(test-ds-one "extra_zoo")
  (test-ds-one "zoo"))


(defun test-all ()
  (dolist (arff UCI-arffs)
    (time (test-one arff))))

(defun test-one (arff)
  (format t "@@ ~A~%" arff)
  (multiple-value-bind 
    (kb kbd) (mlcl::arff->dataset-kb 
              (merge-pathnames
               (make-pathname 
                :name arff
                :type "arff")
               UCI-dir)
              (merge-pathnames
               (make-pathname 
                :name arff
                :type nil)
               cl-kb:*kb-default-path*))
    (values kb kbd)))

(defun test-ds-all ()
  (dolist (arff UCI-arffs)
    (test-ds-one arff)))
  

(defun test-ds-one (arff)
  (format t "@@ ~A~%" arff)
  (let* ((workspace (make-instance 'mlcl::workspace
                                   :file (merge-pathnames
                                          (make-pathname 
                                           :name arff
                                           :type "workspace")
                                          cl-kb:*kb-default-path*)))
         (storage (mlcl::workspace-storage workspace))
         (schema (mlcl::workspace-schema workspace)))

    (if (eq (length (mlcl::storage-cases storage)) 0)
        (progn
          (time (mlcl::workspace-cases-import workspace 
                                              (or
                                               (cl-kb::find-kb (format nil "~A-data" arff) nil)
                                               (make-instance 'cl-kb:kb 
                                                              :protege-pprj-file (merge-pathnames
                                                                                  (make-pathname 
                                                                                   :name (format nil "~A-data" arff)
                                                                                   :type "pprj")
                                                                                  cl-kb:*kb-default-path*)
                                                              :use (list 'cl-kbs::|dataset| 
                                                                         (mlcl::schema-kb schema))))))
          (mlcl:workspace-save workspace)))
    (if (eq (length (mlcl::workspace-algorithms workspace)) 0)
        (mlcl::workspace-make-algorithms workspace (merge-pathnames
                                                 (make-pathname 
                                                  :name "make-01"
                                                  :type "pprj")
                                                 cl-kb:*kb-default-path*)))
    (format t "#cases=~A~%" (length (mlcl::storage-cases storage)))
    (format t "#datasets=~A~%" (length (mlcl::workspace-datasets workspace)))        
    (format t "#algorithms=~A~%" (length (mlcl::workspace-algorithms workspace)))
    (format t "#makefiles=~A~%" (length (mlcl::workspace-makefiles workspace)))
    workspace
    ))


(defun test-algo-01 ()
  (test-algo-one "zoo"))


(defun test-algo-all ()
  (dolist (arff UCI-arffs)
    (time (test-algo-one arff))))

(defun test-algo-one (arff)
  (format t "@@ ~A~%" arff)
  (let* ((workspace (make-instance 'mlcl::workspace
                                   :file (merge-pathnames
                                          (make-pathname 
                                           :directory '(:relative "mlcl-tmp")
                                           :name arff
                                           :type "workspace")
                                          UCI-dir)))
         (storage (mlcl::workspace-storage workspace))
         (schema (mlcl::workspace-schema workspace)))
    (mlcl:workspace-save workspace)
    (format t "#cases=~A~%" (length (mlcl::storage-cases storage)))
    (format t "#datasets=~A~%" (length (mlcl::workspace-datasets workspace)))
    ;(if (eq (length (mlcl::workspace-algorithms workspace)) 0)
    ;    (progn
    ;      (mlcl::workspace-make-algorithms workspace (merge-pathnames
    ;                                                  (make-pathname 
    ;                                                   :name "make-01"
    ;                                                   :type "pprj")
    ;                                                  cl-kb:*kb-default-path*))
    ;      (mlcl:workspace-save workspace)))))
                 ))

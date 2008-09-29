;;;; 2008-04-15 10:44:05
;;;; Behold, the power of lisp.

(in-package :common-lisp-user)

(defvar *default-one-model-pathname*            
  #p"/hardmnt/tharpe0/sra/serra/Software/Developing/MaLeCoLi/workspace/extra/one/model/")


(defvar *default-one-inst-pathname*            
  #p"/hardmnt/tharpe0/sra/serra/Software/Developing/MaLeCoLi/workspace/extra/one/instances/")
  
(defvar *models* 
  (list (merge-pathnames (make-pathname
                          :name "EnglishAuction" :type "nme" :case :local)
                         *default-one-model-pathname*)
        (merge-pathnames (make-pathname
                          :name "Gare-Tender" :type "nme" :case :local)
                         *default-one-model-pathname*)))


(defun gni (name)
  (merge-pathnames (make-pathname
                    :name name :type "nmi" :case :local)
                   *default-one-inst-pathname*))

(defvar *model-instances* 
  (list (gni "canovi")
        (gni "Gara+Round+1")
        (gni "Gare-Tender-instance001")
        (gni "LOGISTICA+E+TRASPORTI")
        (gni "Mora_2")
        (gni "scocco_1")
        (gni "Gara_Alb")
        (gni "Gara+Servizi+IT")
        (gni "Gare-Tender-instance002")
        (gni "Mora_1")
        (gni "SAN")))

(defun test-import-all ()
  (dolist (m *models*)
    (test-import m)))

(defun test-import-01 ()
  (test-import (car (reverse *models*))))

(defun test-import-02 ()
  (test-import (car *model-instances*)))

(defun test-import (modelfile)
  (let ((model (clone-ml::onemodel-import modelfile)))
    (format t "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@~%~A~%" model)
    (clone-ml::import-model-instance model)))

(defun test03 ()
  (let ((kb (cl-kb:find-kb 'cl-kbs::|gare|)))
    (let ((workspace (make-instance 'mlcl::workspace
                                    :file (merge-pathnames 
                                           (make-pathname :type "workspace")
                                           (cl-kb:kb-protege-pprj-file kb))
                                    :schema (make-instance 'mlcl:schema 
                                                           :file (cl-kb:kb-protege-pprj-file kb) 
                                                           :kb kb))))
      (if (eq (length (mlcl::storage-cases (mlcl::workspace-storage workspace))) 0)
          (progn
            (mlcl:workspace-save workspace)
            (format t "! ~A~%" workspace)
            (mlcl:workspace-cases-import workspace (cl-kb:find-kb 'cl-kbs::|gare-instances|))
            (format t "!!! ~A~%" workspace)))
      (if (eq (length (mlcl::workspace-algorithms workspace)) 0)
          (mlcl::workspace-make-algorithms workspace (merge-pathnames
                                                      (make-pathname 
                                                       :name "make-knn-02"
                                                       :type "pprj")
                                                      clone-ml::*default-one-model-kb-pathname*))
          (mlcl:workspace-save workspace))
      (format t "#cases=~A~%" (length (mlcl::storage-cases (mlcl:workspace-storage workspace))))
      (format t "#datasets=~A~%" (length (mlcl::workspace-datasets workspace)))        
      (format t "#algorithms=~A~%" (length (mlcl::workspace-algorithms workspace)))
      (time (test-knn workspace)))))


(defun test-one (modelfile)
  (let ((kb (clone-ml::onemodel-import modelfile)))
    (format t "!!! ~A~%" kb)
    (let ((workspace (make-instance 'mlcl::workspace
                                     :file (merge-pathnames 
                                           (make-pathname :type "workspace")
                                           (cl-kb:kb-protege-pprj-file kb)))))
      (mlcl:workspace-save workspace)
      (if (eq (length (mlcl::workspace-algorithms workspace)) 0)
          (mlcl::workspace-make-algorithms workspace (merge-pathnames
                                                      (make-pathname 
                                                       :name "make-knn-02"
                                                       :type "pprj")
                                                      clone-ml::*default-one-model-kb-pathname*))
          (mlcl:workspace-save workspace))
      (format t "#cases=~A~%" (length (mlcl::storage-cases (mlcl:workspace-storage workspace))))
      (format t "#datasets=~A~%" (length (mlcl::workspace-datasets workspace)))        
      (format t "#algorithms=~A~%" (length (mlcl::workspace-algorithms workspace)))
      ;(test-knn workspace)))
      workspace)))


(defun test-knn (workspace)
  (let ((knn (mlcl:workspace-find-algorithm workspace "make-knn-02")))
    (mlcl-knn:knn-init knn workspace)
    (let ((cas (nth 10 (mlcl:dataset-cases (mlcl:workspace-find-dataset workspace (mlcl-knn:knn-dataset-name knn))))))
      (let ((tops (mlcl-knn:knn-search knn workspace cas)))
        (list workspace knn cas tops)))))

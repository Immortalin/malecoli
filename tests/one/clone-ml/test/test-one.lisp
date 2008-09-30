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
     ;   (gni "Gare-Tender-instance001")
        (gni "LOGISTICA+E+TRASPORTI")
        (gni "Mora_2")
        (gni "scocco_1")
        (gni "Gara_Alb")
        (gni "Gara+Servizi+IT")
     ;   (gni "Gare-Tender-instance002")
        (gni "Mora_1")
        (gni "SAN")))

(defun test-import-all ()
  (dolist (m *model-instances*)
    (test-import m)))

(defun test-import-01 ()
  (test-import (car (reverse *models*))))

(defun test-import-02 ()
  (test-import (nth 1 *model-instances*)))

(defun test-import (modelfile)
  (let ((model (clone-ml::onemodel-import modelfile)))
    (clone-ml::import-model-instance model)))

(defun test-onecbr-all ()
  (setf (clone-ml::onecbr-workspaces clone-ml::*onecbr*) nil)
  (dolist (m *model-instances*)
    (test-onecbr m)))

(defun test-onecbr (modelfile)
  (let ((model (clone-ml::onemodel-import modelfile)))
    (clone-ml::onecbr-add-instance-model clone-ml::*onecbr* model)
    (format t "~A ~%" (clone-ml::onecbr-search-models clone-ml::*onecbr* model))))


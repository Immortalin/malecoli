;;;; 2008-04-15 10:44:05
;;;; Behold, the power of lisp.

(in-package :common-lisp-user)

(defvar *default-one-model-pathname*            
  #p"/hardmnt/tharpe0/sra/serra/Software/Developing/MaLeCoLi/workspace/extra/one/model/")

(defvar *default-one-inst-pathname*            
  #p"/hardmnt/tharpe0/sra/serra/Software/Developing/MaLeCoLi/workspace/extra/one/instances/")

(defvar *default-one-log-pathname*            
  #p"/hardmnt/tharpe0/sra/serra/Software/Developing/MaLeCoLi/workspace/extra/one/logs/")
  

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

(defun gnilog (name)
  (merge-pathnames (make-pathname
                    :name name :type "nmi" :case :local)
                   *default-one-log-pathname*))

(defvar *model-instances*)
(setf *model-instances*
  (list 
        ;(gni "canovi")
        ;(gni "Gara+Round+1")
        ;(gni "LOGISTICA+E+TRASPORTI")
        ;(gni "Mora_2")
        ;(gni "scocco_1")
        ;(gni "Gara_Alb")
        ;(gni "Gara+Servizi+IT")
        ;(gni "Mora_1")
        ;(gni "SAN")
        ;(gnilog "668f31fa-53e9-4cb2-8da3-9d540f3bd995")
        (gnilog "a34d5089-eed7-45a4-889b-c5e3aed3032a")
        ))

(defun test-import-all ()
  (dolist (m *model-instances*)
    (test-import m)))

(defun test-import-01 ()
  (test-import (car (reverse *models*))))

(defun test-import-02 ()
  (test-import (nth 1 *model-instances*)))

(defun test-import (modelfile)
  (let ((model (clone-kb::xml-model-import modelfile)))
    (clone-kb::import-model-instance model)
    (clone-kb::xml-messages-import (merge-pathnames 
                                   (make-pathname :type "xml" :name (format nil "~A-messages" (pathname-name modelfile)))
                                   modelfile))))


;(defun test-onecbr-all ()
;  (setf (clone-kb::onecbr-workspaces clone-kb::*onecbr*) nil)
;  (dolist (m *model-instances*)
;    (test-onecbr m)))

;(defun test-onecbr (modelfile)
;  (let ((model (clone-kb::onemodel-import modelfile)))
;    (clone-kb::onecbr-add-instance-model clone-kb::*onecbr* model)
;    (format t "~A ~%" (clone-kb::onecbr-search-models clone-kb::*onecbr* model))))


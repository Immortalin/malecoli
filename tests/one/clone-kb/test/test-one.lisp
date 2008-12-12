;;;; 2008-04-15 10:44:05
;;;; Behold, the power of lisp.

(in-package :common-lisp-user)

(defvar *default-one-model-pathname*            
  #p"/home/serra/Software/Developing/MaLeCoLi/runtime_ws/examples/one/model/")

(defvar *default-one-inst-pathname*            
  #p"/home/serra/Software/Developing/MaLeCoLi/runtime_ws/examples/one/instances/")

(defvar *default-one-log-pathname*            
  #p"/home/serra/Software/Developing/MaLeCoLi/runtime_ws/examples/one/logs/")
  

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
   ;(gnilog "3b03188e-e882-4d63-a355-1a7dbf3d227b")
        ;(gnilog "668f31fa-53e9-4cb2-8da3-9d540f3bd995")
        ;(gnilog "a34d5089-eed7-45a4-889b-c5e3aed3032a")
   ;(gnilog "7d956f02-6ec9-4798-80ca-841174c7424e")
   (gnilog "fce68abd-45f4-4f06-b47f-43c82f41baab")
        ))

(defun test-import-all ()
  (dolist (m *model-instances*)
    (test-import m)))

(defun test-import-01 ()
  (test-import (car (reverse *models*))))

(defun test-import-02 ()
  (test-import (nth 1 *model-instances*)))

(defun test-import (modelfile)
  (let ((neg (clone-kb:xml-negotiation-import modelfile)))
    (clone-kb:negotiation->kb neg t)))

;(defun test-onecbr-all ()
;  (setf (clone-kb::onecbr-workspaces clone-kb::*onecbr*) nil)
;  (dolist (m *model-instances*)
;    (test-onecbr m)))

;(defun test-onecbr (modelfile)
;  (let ((model (clone-kb::onemodel-import modelfile)))
;    (clone-kb::onecbr-add-instance-model clone-kb::*onecbr* model)
;    (format t "~A ~%" (clone-kb::onecbr-search-models clone-kb::*onecbr* model))))


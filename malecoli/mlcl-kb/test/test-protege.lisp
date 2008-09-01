;;;; Created on 2008-02-21 13:02:52

(in-package :common-lisp-user)

(defvar *protege-ex-01*)
(defvar *protege-ex-02*)
(defvar *protege-ex-03*)
(defvar *protege-ex-04*)
(defvar *output*)
(defvar *output-txt*)

(setf *protege-ex-01* #p"/hardmnt/tharpe0/sra/serra/Work/ONEv0.1/CBR/protege_frame_ontologies/examples/xml/test.xml")
(setf *protege-ex-02* #p"/hardmnt/tharpe0/sra/serra/Work/ONEv0.1/CBR/protege_frame_ontologies/examples/xml/newpaper.xml")
(setf *protege-ex-03* #p"/hardmnt/tharpe0/sra/serra/Work/ONEv0.1/CBR/protege_frame_ontologies/examples/xml/wines.xml")
(setf *protege-ex-04* #p"/hardmnt/tharpe0/sra/serra/Work/ONEv0.1/CBR/protege_frame_ontologies/examples/xml/test-values.xml")
(setf *output* #p"/tmp/output.xml")
(setf *output-txt* #p"/tmp/output.txt")


(defun test03 ()
  (let ((kb (mlcl-kb:find-kb "ciao"))
        (lxml nil)
        (genlxml nil))
    (if (null kb)
        (setf kb (mlcl-kb:make-kb "ciao" :use (list 'mlcl-kbs:kb) :protege-file *output*)))
    (mlcl-kb:kb-clear kb)
    (setf lxml (mlcl-kb::kb-import-from-protege-xml *protege-ex-01* kb))
    (setf genlxml (mlcl-kb::kb-export-to-protege-xml *output* kb))
    (with-open-file (strm *output-txt* :direction :output :if-exists :supersede)
                    (mlcl-kb::kb-dump strm kb))
    (mlcl-kb:kb-save kb)
    ;(format t "~%~%~A~%~A" lxml genlxml)
    kb))

(mlcl-kb:def-kb "newpaper" 
                      :use '("PROTEGE-KB") 
                      :protege-file #p"/hardmnt/tharpe0/sra/serra/Work/ONEv0.1/CBR/protege_frame_ontologies/examples/xml/newpaper.xml")

(mlcl-kb:in-kb "newpaper")
(mlcl-kb:def-cls "Salesperson")
(cl:in-package :common-lisp-user)


(defun test04 ()
  (mlcl-kb::kb-element-dump |newpaper|:|Salesperson| t)
  (mlcl-kb::kb-element-dump protege-kb:|:META-CLASS| t)
  (mlcl-kb::kb-element-dump protege-kb:|:ANNOTATION-TEXT| t)
  (mlcl-kb::kb-element-dump protege-kb:|:ROLE| t))
  

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
(setf *output* #p"/tmp/ciao.pprj")
(setf *output-txt* #p"/tmp/ciao.txt")


(defun test03 ()
  (let ((kb (cl-kb:find-kb "ciao" nil)))
    (if (null kb)
        (progn
          (setf kb (cl-kb:make-kb *output* :use '(cl-kbs::|protege|)))
          (cl-kb:kb-create kb)))
    (cl-kb:kb-clear kb)
    (cl-kb::kb-import-from-protege-xml *protege-ex-01* kb)
    (cl-kb:kb-save kb)
    (with-open-file (strm *output-txt* :direction :output :if-exists :supersede)
                    (cl-kb::kb-dump strm kb))
    kb))




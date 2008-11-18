;;;; Created on 2008-02-21 13:02:52

(in-package :common-lisp-user)

(defvar *protege-ex-01*)
(defvar *protege-ex-02*)
(defvar *protege-ex-03*)
(defvar *protege-ex-04*)
(defvar *output*)
(defvar *output-txt*)

(setf *protege-ex-01* #p"/hardmnt/tharpe0/sra/serra/Software/Developing/MaLeCoLi/runtime_ws/examples/breast-cancer.pprj")
(setf *output-txt* #p"/hardmnt/tharpe0/sra/serra/Software/Developing/MaLeCoLi/runtime_ws/examples/breast-cancer.txt")

(defun test03 ()
  (let ((kb (cl-kb:find-kb "breast-cancer" nil t)))
    (cl-kb:kb-open kb)
    (with-open-file (strm *output-txt* :direction :output :if-exists :supersede)
                    (cl-kb::kb-dump strm kb))
    kb))




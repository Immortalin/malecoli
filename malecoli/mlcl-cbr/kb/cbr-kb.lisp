;;;; 2008-08-21 09:30:59

(in-package :mlcl-cbr)


(eval-when (:LOAD-TOPLEVEL :EXECUTE)
  (if (null (mlcl-kb:find-kb "cbr" nil))
        (mlcl-kb:make-kb (get-resource-pathname "cbr" "pprj"))))
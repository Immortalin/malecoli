;;;; Created on 2008-04-24 12:44:13

(in-package :clone-ml)

;
; gare instance knowledge base
;

(defvar *gare-instances-kb-pathname*)

(eval-when (:LOAD-TOPLEVEL :EXECUTE)
  (if (null (mlcl-kb:find-kb "gare-instances" nil))
      (progn
        (setf *gare-instances-kb-pathname*            
              #p"/hardmnt/tharpe0/sra/serra/Software/Developing/MaLeCoLi/workspace/extra/gare/resources/gare-instances.pprj")
        (mlcl-kb:make-kb *gare-instances-kb-pathname* 
                         :use '(mlcl-kbs::|gare|)))))
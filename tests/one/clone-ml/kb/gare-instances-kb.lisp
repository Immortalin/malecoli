;;;; Created on 2008-04-24 12:44:13

(in-package :clone-ml)

(progn
  (defvar *gare-instances-kb-pathname*)
  (eval-when (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
    (if (null (boundp '*gare-instances-kb-pathname*))
        (setq *gare-instances-kb-pathname*            
              #p"/hardmnt/tharpe0/sra/serra/Software/Developing/MaLeCoLi/workspace/extra/gare/resources/gare-instances.xml"))))


(mlcl-kb:def-kb "GARE-INSTANCES-KB"
                :use (list mlcl-kbs::PROTEGE-KB mlcl-kbs::negotiation-kb mlcl-kbs::gare-kb)
                :protege-file *gare-instances-kb-pathname*
                :autoload nil)

(mlcl-kb:in-kb "GARE-INSTANCES-KB")                 


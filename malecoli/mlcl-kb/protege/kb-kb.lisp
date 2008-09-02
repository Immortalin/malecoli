;;;; 2008-08-21 09:30:59

(in-package :mlcl-kb)

(defvar *kb-pathname*)

(eval-when (:LOAD-TOPLEVEL :EXECUTE)
  (if (null (mlcl-kb:find-kb "KB"))
      (progn
        (setf *kb-pathname*            
              #-sbcl (merge-pathnames
                      (make-pathname
                       :directory '(:relative ".." "resources")
                       :name "kb" :type "xml" :case :local)
                      *load-truename*)
              #+sbcl #p"/hardmnt/tharpe0/sra/serra/Software/Developing/MaLeCoLi/code.google.com/workspace/malecoli-trunk/malecoli/mlcl-kb/resources/kb.xml")
        (mlcl-kb:make-kb "KB" :use-list '(mlcl-kbs::protege-kb) :protege-file *kb-pathname*))))



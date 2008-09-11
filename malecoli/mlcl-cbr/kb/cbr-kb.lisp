;;;; 2008-08-21 09:30:59

(in-package :mlcl-cbr)

(defvar *cbr-kb-pathname*)

(eval-when (:LOAD-TOPLEVEL :EXECUTE)
  (if (null (mlcl-kb:find-kb "CBR-KB" nil))
      (progn
        (setf *cbr-kb-pathname*            
              #-sbcl (merge-pathnames
                      (make-pathname
                       :directory '(:relative ".." "resources")
                       :name "cbr" :type "xml" :case :local)
                      *load-truename*)
              #+sbcl #p"/hardmnt/tharpe0/sra/serra/Software/Developing/MaLeCoLi/code.google.com/workspace/malecoli-trunk/malecoli/mlcl-cbr/resources/cbr.xml")
        (mlcl-kb:make-kb "CBR-KB" :use-list '(mlcl-kbs::protege-kb) :protege-file *cbr-kb-pathname*))))
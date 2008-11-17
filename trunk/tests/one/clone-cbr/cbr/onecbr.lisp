;;;; Created on 2008-09-30 13:15:57

(in-package :clone-ml)

(defclass onecbr ()
  ((workspaces 
    :TYPE list
    :INITFORM nil
    :ACCESSOR onecbr-workspaces)))

(defvar *onecbr* (make-instance 'onecbr))

(defun onecbr-find-workspace (onecbr model)
  (let ((ws-file (merge-pathnames
                  (make-pathname
                   :name (model-full-name model)
                   :type "workspace" :case :local)
                  *default-one-model-kb-pathname*)))
    (let ((ws (find-if #'(lambda (x) (equal 
                                      (mlcl:workspace-file x)
                                      ws-file))
                       (onecbr-workspaces onecbr))))
      (if (null ws)
          (progn
            (let ((kb (import-model model)))
              (setf ws (make-instance 'mlcl:workspace 
                                      :file ws-file
                                      :schema (make-instance 'mlcl:schema 
                                                             :file (merge-pathnames
                                                                    (make-pathname
                                                                     :type "pprj")
                                                                    ws-file)
                                                             :kb kb)))
              (mlcl::workspace-make-algorithms ws (merge-pathnames
                                                   (make-pathname 
                                                    :name "make-knn-01"
                                                    :type "pprj")
                                                   clone-ml::*default-one-model-kb-pathname*))
              (mlcl:workspace-save ws)
              (push ws (onecbr-workspaces onecbr)))))
      ws)))
    
(defun onecbr-add-instance-model (onecbr model)
  (let ((ws (onecbr-find-workspace onecbr model))
        (kb (import-model-instance model)))    
    (mlcl:workspace-cases-import ws kb)    
    (format t "#cases=~A~%" (length (mlcl::storage-cases (mlcl:workspace-storage ws))))
    (format t "#datasets=~A~%" (length (mlcl::workspace-datasets ws)))        
    (format t "#algorithms=~A~%" (length (mlcl::workspace-algorithms ws)))
    (mlcl:workspace-save ws)))

(defun onecbr-search-models (onecbr model)
  (let ((ws (onecbr-find-workspace onecbr model))
        (kb (import-model-instance model)))
  (let ((knn (mlcl:workspace-find-algorithm ws "make-knn-01"))
        (ds (mlcl:workspace-find-dataset ws "make-knn-01-tmp"))
        (c nil))
    (if (null ds)
        (setf ds (mlcl:workspace-make-temporary-dataset ws "make-knn-01-tmp")))
    (cl-kb:with-kb kb nil
                   (setf c (mlcl:dataset-import-case-from-kb ds (car (cl-kb:cls-direct-instances (cl-kb:find-cls 
                                                                                                  (model-case-id model)))))))
    (mlcl-knn:knn-init knn ws)
    (let ((tops (mlcl-knn:knn-search knn ws c)))
      (list ws knn c tops)))))

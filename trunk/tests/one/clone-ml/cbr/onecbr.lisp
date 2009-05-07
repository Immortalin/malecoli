;;;; Created on 2008-09-30 13:15:57

(in-package :clone-ml)

(defclass onecbr ()
  ((workspaces 
    :TYPE list
    :INITFORM nil
    :ACCESSOR onecbr-workspaces)))

(defvar *onecbr* (make-instance 'onecbr))

(defvar *makefiles* (list "make-knn-one"))
(defvar *dataset-name-all* "knn-one-dataset-all")
(defvar *dataset-name-agreed* "knn-one-dataset-agreed")
(defvar *tmp-dataset-name* "knn-one-dataset-tmp")
  

(defun cbr-add-negotiation-instance (instance-id) 
  (let ((ci (instance-id->caseinfo instance-id)))
    (if (null (cbr-find-instance-model (caseinfo-model-id ci) instance-id))
        (cbr-add-instance-model (caseinfo-model-id ci) instance-id))))

(defun cbr-process-negotiation-instance (instance-id process-id)
  (let* ((ci (instance-id->caseinfo instance-id))
         (model-id (caseinfo-model-id ci))
         (ws (cbr-find-workspace model-id))
         (kb (clone-kb::find-model-instance-kb model-id instance-id)))
    (let ((knn (mlcl:workspace-find-algorithm ws process-id))
          (ds (mlcl:workspace-find-dataset ws *tmp-dataset-name*))
          (c nil))
      (if (null ds)
          (setf ds (mlcl::workspace-make-temporary-dataset ws *tmp-dataset-name*)))
      (cl-kb:with-kb kb nil        
                     (setf c (mlcl:dataset-import-case-from-kb ds (cl-kb:find-simple-instance "the case"))))
      (let ((tops (mlcl-knn:knn-search knn ws c)))
        (format t "@@@@@@@@@ ~A~%" tops)
        (list ci (mapcar #'(lambda (x) (list (car x) (find-caseinfo (cadr x)))) tops))))))

(defun cbr-clear-cache ()
  (setf (onecbr-workspaces *onecbr*) nil))

(defun cbr-save-workspaces ()
  (dolist (w (onecbr-workspaces *onecbr*))
    (mlcl:workspace-save w)))

(defun cbr-find-workspace (model-id)
  (let ((ws-file (merge-pathnames
                  (make-pathname
                   :name (clone-kb::model-kb-name model-id)
                   :type "workspace" :case :local)
                  cl-kb:*kb-default-path*)))
    (let ((ws (find-if #'(lambda (x) (equal 
                                      (mlcl:workspace-file x)
                                      ws-file))
                       (onecbr-workspaces *onecbr*))))
      (if (null ws)
          (progn
            (setf ws (make-instance 'mlcl:workspace 
                                    :file ws-file))
            (if (= (length (mlcl::workspace-makefiles ws)) 0)
                (progn 
                  (dolist (mak *makefiles*)
                    (mlcl::workspace-make-algorithms ws (merge-pathnames
                                                         (make-pathname 
                                                          :name mak
                                                          :type "pprj")
                                                         cl-kb:*kb-default-path*))
                    (mlcl:workspace-save ws))))
            (push ws (onecbr-workspaces *onecbr*))))
      (format t "#cases=~A   " (length (mlcl::storage-cases (mlcl:workspace-storage ws))))
      (format t "#datasets=~A   " (length (mlcl::workspace-datasets ws)))        
      (format t "#algorithms=~A~%" (length (mlcl::workspace-algorithms ws)))
      ws)))

(defun cbr-add-instance-model (model-id instance-id)
 (let ((ws (cbr-find-workspace model-id))
       (kb (clone-kb::find-model-instance-kb model-id instance-id)))
   (cl-kb:with-kb kb nil
                  (let ((ds (mlcl:workspace-find-dataset ws *dataset-name-all*))
                        (dsa (mlcl:workspace-find-dataset ws *dataset-name-agreed*))
                        (cas (cl-kb:find-simple-instance "the case")))
             
                    (mlcl:dataset-import-case-from-kb ds cas)
                    (if (string= 
                         (cl-kb:frame-own-slot-value (cl-kb:find-simple-instance "the conclusion") '|negotiation|::|neg_case_state|)
                         "Agreed")
                        (mlcl:dataset-import-case-from-kb dsa cas))
                    (format t "#all=~A   " (length (mlcl:dataset-cases ds)))
                    (format t "#agreed=~A   ~%" (length (mlcl:dataset-cases dsa)))))))

(defun cbr-find-instance-model (model-id instance-id)
  (let ((ws (cbr-find-workspace model-id)))
    (let ((ds (mlcl:workspace-find-dataset ws *dataset-name-all*)))
      (find-if #'(lambda (x) (string=
                              (|onenegotiation-ws|::|one_id| 
                               (|negotiation-ws|::|neg_case_context| x))
                              instance-id))
               (mlcl:dataset-cases ds)))))
  

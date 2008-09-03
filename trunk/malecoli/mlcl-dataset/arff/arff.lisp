;;;; Created on 2008-08-26 09:57:12

(in-package :mlcl-dataset)

;
; import
;

(defun arff-import (pathname)
  (let* ((fn (pathname-name pathname))
         (fnd (format nil "~A-data" fn))
         (kb (mlcl-kb:find-kb fn))
         (kbd (mlcl-kb:find-kb fnd)))
    (if (null kb)
        (progn
          (setf kb (mlcl-kb:make-kb fn
                                    :use-list '(mlcl-kbs::DATASET-KB)
                                    :protege-file (merge-pathnames
                                                   (make-pathname 
                                                    :directory '(:relative "mlcl-tmp")
                                                    :type "xml")
                                                   pathname)))
          (mlcl-kb:kb-create kb))
          (mlcl-kb:kb-clear kb))
    (if (null kbd)
        (progn
          (setf kbd (mlcl-kb:make-kb fnd
                                    :use-list (list 'mlcl-kbs::DATASET-KB kb)
                                    :protege-file (merge-pathnames
                                                   (make-pathname 
                                                    :directory '(:relative "mlcl-tmp")
                                                    :name fnd
                                                    :type "xml")
                                                   pathname)))
          (mlcl-kb:kb-create kbd))
          (mlcl-kb:kb-clear kbd))
        
    (mlcl-kb:kb-open kb)
    (mlcl-kb:kb-open kbd)
    (with-open-file (strm pathname)
                    (multiple-value-bind (relation-name attributes comments) (arff-read-header strm)
                      (arff-import-header relation-name attributes comments kb)
                      (arff-import-data relation-name attributes strm kbd)))
    (mlcl-kb:kb-save kb)
    (mlcl-kb:kb-save kbd)
    kb))


;
; import arff header
;

(defun arff-import-header (relation-name attributes comments kb)
  (let ((mlcl-kb:*kb* kb))
    (let ((insi (mlcl-kb:make-simple-instance (format nil "~A-info" relation-name)))
          (cacl (mlcl-kb:make-cls (format nil "~A-case" relation-name)))
          (slots nil)) 
      (mlcl-kb:instance-add-direct-type insi 'dataset-kb::|DatasetInfo|)
      (mlcl-kb:instance-add-direct-type cacl 'protege-kb::|:STANDARD-CLASS|)
      (mlcl-kb:cls-add-direct-supercls cacl 'dataset-kb::|DatasetCase|)
      (setf (mlcl-kb:cls-concretep cacl) t)
      (dolist (attr attributes)
        (let ((slot (mlcl-kb:make-slot (format nil "~A-~A" relation-name (car attr)))))
          (mlcl-kb:instance-add-direct-type slot 'protege-kb::|:STANDARD-SLOT|)
          (mlcl-kb:cls-add-direct-template-slot cacl slot)
          (cond 
           ((eq (cdr attr) 'real)
            (setf (mlcl-kb:slot-value-type slot) protege-kb:float-type-value))
           ((eq (cdr attr) 'numeric)
            (setf (mlcl-kb:slot-value-type slot) protege-kb:float-type-value))
           ((eq (cdr attr) 'integer)
            (setf (mlcl-kb:slot-value-type slot) protege-kb:integer-type-value))
           ((eq (cdr attr) 'string)
            (setf (mlcl-kb:slot-value-type slot) protege-kb:string-type-value))
           ((eq (cdr attr) 'date)
            (setf (mlcl-kb:slot-value-type slot) (list protege-kb:instance-type-value 
                                                       'dataset-kb::|time|)))
           ((and (listp (cdr attr)) (eq (second attr) 'nominal))
            (setf (mlcl-kb:slot-value-type slot) (cons protege-kb:symbol-type-value
                                                (caddr attr)))))
          (setf (mlcl-kb:slot-maximum-cardinality slot) 1)
          (push slot slots)))
      (mlcl-kb:frame-add-own-slot-value insi 'dataset-kb::|dataset_name| relation-name)
      (mlcl-kb:frame-add-own-slot-value insi 'dataset-kb::|dataset_version| "0.0.1")
      (mlcl-kb:frame-add-own-slot-value insi 'dataset-kb::|dataset_comment| (format nil "~{~a~%~}" comments))
      (mlcl-kb:frame-add-own-slot-value insi 'dataset-kb::|dataset_case_class| cacl)
      (mlcl-kb:frame-add-own-slot-value insi 'dataset-kb::|dataset_default_target_slot| (first slots)))))
      
; import arff data
;

(defun arff-import-data (relation-name attributes strm kbd)
  (let ((mlcl-kb:*kb* kbd)
        (seed nil)
        (num 0)
        (dssi nil))
    (setf dssi (mlcl-kb:make-simple-instance (format nil "~A" relation-name)))
    (mlcl-kb:instance-add-direct-type dssi 'dataset-kb::|Dataset|)
    (mlcl-kb:frame-add-own-slot-value dssi 'dataset-kb::|dataset_info| 
                                      (mlcl-kb:find-frame (format nil "~A-info" relation-name)))
    (arff-read-data attributes strm 
                    seed
                    #'(lambda (seed) 
                        (declare (ignore seed))
                        (let ((si (mlcl-kb:make-simple-instance (format nil "case-~9,'0d" num))))
                          (setf num (+ 1 num))
                          (mlcl-kb:instance-add-direct-type si (format nil "~A-case" relation-name))
                          (push si (mlcl-kb:frame-own-slot-values 
                                    dssi 'dataset-kb::|dataset_case|))
                          si))
                    #'(lambda (seed) 
                        (declare (ignore seed))
                        nil)
                    #'(lambda (seed attr val) 
                        (if (not (string-equal val "?"))
                            (let ((valc val))
                              (cond 
                               ((eq (cdr attr) 'real)
                                (setf valc (float (with-input-from-string (strm val)
                                                                   (read strm)))))
                               ((eq (cdr attr) 'numeric)
                                (setf valc (with-input-from-string (strm val)
                                                                   (read strm))))
                               ((eq (cdr attr) 'integer)
                                (setf valc (parse-integer val)))
                               ((eq (cdr attr) 'date)
                                (setf valc nil)))
                              (mlcl-kb:frame-add-own-slot-value seed (format nil "~A-~A" relation-name (car attr)) valc)))))))

;
; read arff header
;
  
(defun arff-read-header (strm)
  (let ((relation-name nil)
        (attributes nil)
        (comments nil))
    (do ((line (arff-read-header-line strm)
               (arff-read-header-line strm)))
        ((or (null line)
             (string-equal "data" (first line))))
      (cond 
       ((string-equal (first line) "%")
        (push (second line) comments))
       ((string-equal (first line) "relation")
        (setf relation-name (cl-ppcre:register-groups-bind (nil v1 v2) 
                                                ("\\s*((\\w+)|\"(.*)\")\\s*" (second line)) 
                                                (or v1 v2))))
       ((string-equal (first line) "attribute")
        (push (cl-ppcre:register-groups-bind (nil v1 v2 nil v3 v4) 
                                             ("\\s*(([\\w-]+)|\"(.*)\")\\s*((\\w+)|{(.*)})" (second line)) 
                                             (cons (or v1 v2) (arff-read-type v3 v4)))
              attributes))))      
    (setf comments (nreverse comments))
    (setf attributes (nreverse attributes))
    (values relation-name attributes comments)))

(defun arff-read-type (v3 v4)
  (if v3
      (cond 
       ((string-equal v3 "numeric") 'numeric)
       ((string-equal v3 "real") 'real)
       ((string-equal v3 "integer") 'integer)
       ((string-equal v3 "string") 'string)
       ((string-equal v3 "date") 'date))
      (progn
        (let ((vals (cl-ppcre:split "(\\s*,\\s*)" v4)))
          (setf (car vals) (string-left-trim " " (car vals)))
          (list 'nominal vals)))))

(defun arff-read-header-line (strm)
  (let ((line (read-line strm nil))
        (it nil))
    (setf it (cl-ppcre:register-groups-bind (v1) ("%(.+)$" line) v1))
    (if it 
        (list "%" it)
        (progn
          (setf it (cl-ppcre:register-groups-bind (v1 v2) ("@(\\w+)\\s+(.*)$" line) (list v1 v2)))
          (if it
              it
              nil)))))



;
; read arff data
;

(defun arff-read-data (attributes strm seed new-record-fn finish-record-fn field-fn)
  (do ((line (arff-read-data-line strm)
               (arff-read-data-line strm)))
        ((null line))
    (if (eq (first line) 'values)
        (progn
          (setf seed (funcall new-record-fn seed))
          (do ((v (rest line) (cdr v)) (attr attributes (cdr attr)))
              ((and (null attr) (null v)))
            (funcall field-fn seed (car attr) (car v)))
          (setf seed (funcall finish-record-fn seed))))))

(defun arff-read-data-line (strm)
  (let ((line (read-line strm nil))
        (it nil))
    (if (null line)
        nil
        (progn
          (setf it (cl-ppcre:register-groups-bind (v1) ("%(.*)$" line) v1))
          (if it 
              (list "%" it)              
              (cons 'values (cl-ppcre:split "[#\,\\s]" line)))))))

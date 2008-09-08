;;;; Created on 2008-09-08 10:49:11

(in-package :mlcl-dataset)

;
; load
;

(defun kb-load (dataset kb)
  (mlcl-kb:kb-open kb)
  (let ((si-list))
    (dolist (el (mlcl-kb:kb-interned-elements kb))
      (if (and (typep el 'mlcl-kb:simple-instance) 
               (mlcl-kb:instance-has-type el 'dataset-kb::|DatasetThing|))
          (push el si-list)))
    (kb-load-simple-instances dataset si-list))
  (mlcl-kb:kb-close kb))


;
; load instance
;

(defun kb-load-simple-instances (dataset si-list)
  (dolist (si si-list)
    (let ((el (kb-load-simple-instance-header dataset si)))
      (kb-load-simple-instance dataset el si)
      (kb-load-simple-instance-trailer el))))

(defun kb-load-simple-instance-header (dataset si)
  (make-instance (find-symbol (frame->sql-name (mlcl-kb:instance-direct-type si)) 
                              (dataset-package dataset))
                 :name-id (frame->sql-name si)))

(defun kb-load-simple-instance-trailer (el)
  (clsql:update-records-from-instance el))

(defun kb-load-simple-instance (dataset el si)
   (dolist (osv (mlcl-kb:frame-own-slot-values-list si))
    (kb-load-own-slot-value dataset el si osv)))

(defun kb-load-own-slot-value (dataset el si osv)
  (declare (ignore si))
  (let ((slot (slot-value%-slot osv))
        (vals (slot-value%-vals osv)))
    (if vals
        (progn
          (eval (list 'setf 
                   (list (find-symbol 
                           (frame->sql-name slot)
                           (dataset-package dataset))
                         el)
                   (list 'quote (kb-load-slot-value slot vals))))))))


(defun kb-load-slot-value (slot vals)
  (let ((typ (mlcl-kb:slot-value-type slot)))
    (let ((converter-values
           (cond 
            ((eq typ 'protege-kb::integer-type-value)
             vals)
            ((eq typ 'protege-kb::float-type-value)
             vals)
            ((eq typ 'protege-kb::string-type-value)
             (mapcar #'(lambda (x) (format nil "\"~A\"" x)) vals))
            ((eq typ 'protege-kb::boolean-type-value)
             vals)
            ((eq typ 'protege-kb::symbol-type-value)
             (mapcar #'(lambda (x) (format nil "\"~A\"" x)) vals))
            ((eq typ 'protege-kb::instance-type-value)
             (mapcar #'frame-name vals)))))
      (if (eq (mlcl-kb:slot-maximum-cardinality slot) 1)
          (progn 
            (car converter-values))
          (progn
            converter-values)))))



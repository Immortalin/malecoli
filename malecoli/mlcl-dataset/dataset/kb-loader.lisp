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
      (kb-load-simple-instance-trailer dataset el))))

(defun kb-load-simple-instance-header (dataset si)
  (make-instance (find-symbol (frame->lisp-name (mlcl-kb:instance-direct-type si)) 
                              (dataset-schema-package (dataset-schema dataset)))
                 :name-id (frame->lisp-name si)))

(defun kb-load-simple-instance-trailer (dataset el)
  (push el (dataset-cases dataset)))

(defun kb-load-simple-instance (dataset el si)
   (dolist (osv (mlcl-kb:frame-own-slot-values-list si))
    (kb-load-own-slot-value dataset el si osv)))

(defun kb-load-own-slot-value (dataset el si osv)
  (declare (ignore si))
  (let ((slot (slot-values%-slot osv))
        (vals (slot-values%-vals osv)))
    (if vals
        (progn
          (eval (list 'setf 
                   (list (find-symbol 
                           (frame->lisp-name slot)
                           (dataset-schema-package (dataset-schema dataset)))
                         el)
                   (list 'quote (kb-load-slot-value slot vals))))))))


(defun kb-load-slot-value (slot vals)
  (let ((typ (mlcl-kb:slot-value-type slot)))
    (let ((converter-values
           (cond 
            ((eq typ 'mlcl-kb:integer-type-value)
             vals)
            ((eq typ 'mlcl-kb:float-type-value)
             vals)
            ((eq typ 'mlcl-kb:string-type-value)
             vals)
            ((eq typ 'mlcl-kb:boolean-type-value)
             vals)
            ((eq typ 'mlcl-kb:symbol-type-value)
             vals)
            ((eq typ 'mlcl-kb:instance-type-value)
             (mapcar #'frame-name vals)))))
      (if (eq (mlcl-kb:slot-maximum-cardinality slot) 1)
          (progn 
            (car converter-values))
          (progn
            converter-values)))))



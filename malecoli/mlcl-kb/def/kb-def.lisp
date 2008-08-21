;;;; Created on 2008-04-23 11:25:54

(in-package :mlcl-kb)

(defpackage :mlcl-kbs)

(in-package :mlcl-kb)

;
; kb def
;

(defmacro def-kb (name &key (use nil) (protege-file nil) (autoload t))
  (declare (type string name))
  (let ((s (intern name (find-package :mlcl-kbs))))
    `(progn
       (defvar ,s)
       (eval-when (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
                     (if (null (boundp (quote ,s)))
                         (progn
                           (setq ,s (make-kb ,name :use ,use :protege-file ,protege-file))
                           (if ,autoload
                               (kb-load ,s))))))))

(defmacro in-kb (kbname)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf *kb* (find-kb ,kbname))
     (in-package ,kbname)))


;
; references
;

(defmacro def-instance-ref (name make-code)
  (let ((s (intern name (kb-package *kb*))))
    `(progn
       (defvar ,s)
       (eval-when (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
         (if (null (boundp (quote ,s)))
               (setq ,s ,make-code))))))

(defmacro def-instance-init (name init-code)
  `(let ((it (kb-find-element ,name)))
     (if (not (kb-element-definedp it))
         (progn 
           ,init-code
           (setf (kb-element-definedp it) t)))))

(defmacro def-cls-ref (name)
  `(def-instance-ref ,name (kb-get-cls ,name)))

(defmacro def-slot-ref (name)
  `(def-instance-ref ,name (kb-get-slot ,name)))

(defmacro def-facet-ref (name)
  `(def-instance-ref ,name (kb-get-facet ,name)))

(defmacro def-simple-instance-ref (name)
  `(def-instance-ref ,name (kb-get-simple-instance ,name)))


;
; element defs
;

(defmacro def-cls (name &key (types nil) (superclses nil) (template-slots nil) (own-slot-values-list nil))
  `(progn
     (def-cls-ref ,name)
     (def-instance-init ,name (init-cls it ,types ,superclses ,template-slots ,own-slot-values-list))))
    
(defmacro def-slot (name &key (types nil) (superslots nil) (own-slot-values-list nil))
  `(progn
     (def-slot-ref ,name)
     (def-instance-init ,name (init-slot it ,types ,superslots ,own-slot-values-list))))
  
(defmacro def-facet (name &key (types nil) (own-slot-values-list nil))
  `(progn
     (def-facet-ref ,name)
     (def-instance-init ,name (init-facet it ,types ,own-slot-values-list))))
  
(defmacro def-simple-instance (name &key (types nil) (own-slot-values-list nil))
  `(progn
     (def-simple-instance-ref ,name)
     (def-instance-init ,name (init-simple-instance it ,types ,own-slot-values-list))))

;
; element inits
;

(defun init-instance (el type-or-types own-slot-values-list)
  (if (listp type-or-types)
      (dolist (ty type-or-types)
        (instance-add-direct-type el ty))
      (instance-add-direct-type el type-or-types))
  (dolist (own-slot-values own-slot-values-list)
    (frame-add-own-slot-value el (car own-slot-values) (cdr own-slot-values)))
  el)

(defun init-cls (el type-or-types supercls-or-superclses template-slots own-slot-values-list)
  (init-instance el type-or-types own-slot-values-list)
  (if (listp supercls-or-superclses)
      (dolist (cl supercls-or-superclses)
        (cls-add-direct-supercls el cl))
      (cls-add-direct-supercls el supercls-or-superclses))
  (dolist (slot template-slots)
        (cls-add-direct-template-slot el slot))
  el)

(defun init-slot (el type-or-types superslots own-slot-values-list)
  (init-instance el type-or-types own-slot-values-list)
  (dolist (sl superslots)
    (cls-add-direct-supercls el sl))
  el)

(defun init-facet (el type-or-types own-slot-values-list)
  (init-instance el type-or-types own-slot-values-list))

(defun init-simple-instance (el type-or-types own-slot-values-list)
  (init-instance el type-or-types own-slot-values-list))
  
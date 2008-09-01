;;;; Created on 2008-04-30 13:03:56

(in-package :mlcl-kb)


;
; make for clses slots facets anf simple-instances
;

(defun kb-make-cls (name &key (kb *kb*) (definedp t))
  (check-type name string)
  (check-type kb kb)
  (make-instance 'cls :name name :definedp definedp :kb kb))

(defun kb-make-slot (name &key (kb *kb*) (definedp t))
  (check-type name string)
  (check-type kb kb)  
  (make-instance 'slot :name name :definedp definedp :kb kb))

(defun kb-make-facet (name &key (kb *kb*) (definedp t))
  (check-type name string)
  (check-type kb kb)
  (make-instance 'facet :name name :definedp definedp :kb kb))

(defun kb-make-simple-instance (name &key (kb *kb*) (definedp t))
  (check-type name string)
  (check-type kb kb)
  (make-instance 'simple-instance :name name :definedp definedp :kb kb))


;
; get
;

(defun kb-get-cls (name &key (kb *kb*))
  (check-type name string)
  (check-type kb kb)
  (let ((it (kb-find-element name kb)))
    (if (null it)
        (values (kb-make-cls name :kb kb :definedp nil) t)
        (values it nil))))

(defun kb-get-slot (name &key (kb *kb*))
  (check-type name string)
  (check-type kb kb)
  (let ((it (kb-find-element name kb)))
    (if (null it)
        (values (kb-make-slot name :kb kb :definedp nil) t)
        (values it nil))))

(defun kb-get-facet (name &key (kb *kb*))
  (check-type name string)
  (check-type kb kb)
  (let ((it (kb-find-element name kb)))
    (if (null it)
        (values (kb-make-facet name :kb kb :definedp nil) t)
        (values it nil))))

(defun kb-get-simple-instance (name &key (kb *kb*))
  (check-type name string)
  (check-type kb kb)
  (let ((it (kb-find-element name kb)))
    (if (null it)
        (values (kb-make-simple-instance name :kb kb :definedp nil) t)
        (values it nil))))

;;;; Created on 2008-04-30 12:23:57

(in-package :mlcl-kb)

;
; A frame of a kb
;

(defclass frame (kb-element)
  ((kb
    :READER frame-kb
    :INITARG :kb
    :TYPE kb)
   (own-slot-values
    :ACCESSOR frame-own-slot-values%
    :INITFORM nil)))


;
; An instance of a kb
;

(defclass instance (frame)
  ((direct-types
    :ACCESSOR instance-direct-types
    :INITFORM nil)))


;
; A class of a kb
;

(defclass cls (instance)
  ((direct-superclses
    :ACCESSOR cls-direct-superclses
    :INITFORM nil)
   (direct-template-slots
    :ACCESSOR cls-direct-template-slots
    :INITFORM nil)
   (direct-template-facet-values
    :ACCESSOR cls-direct-template-facet-values%
    :INITFORM nil)))


;
; A slot of an instance
;

(defclass slot (instance)
  ((direct-superslots
   :ACCESSOR slot-direct-superslots
   :INITFORM nil)))


;
; A facet of a kb
;

(defclass facet (instance)
  ())


;
; A simple instance of a kb
;

(defclass simple-instance (instance)
  ())


;
; own slot value
;

(defstruct slot-value%
  (slot nil :type slot)
  (values))


;
; template facet value
; 

(defstruct facet-value%
  (slot nil :type slot)
  (facet nil :type facet)
  (values))


;
; frame functions
;

(defmethod initialize-instance :after ((fr frame) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (kb-intern fr (frame-kb fr)))

(defun frame-name (frame)
  (kb-element-name frame))

(defun frame-symbol (frame) 
  (kb-find-element-symbol (frame-kb frame) frame))

(defun frame-ref-own-slot-values (frame slot)
  (declare (type frame frame)
           (type slot slot))
  (find-if #'(lambda (x) (eq (slot-value%-slot x) slot)) 
                         (slot-value frame 'own-slot-values)))

(defun frame-own-slot-values (frame slot)
  (declare (type frame frame)
           (type slot slot))
  (let ((it (frame-ref-own-slot-values frame slot)))
    (if it
        (slot-value%-values it)
        nil)))

(defun frame-own-slot-value (frame slot)
  (declare (type frame frame)
           (type slot slot))
  (car (frame-own-slot-values frame slot)))

(defun (setf frame-own-slot-values) (frame slot vals)
  (declare (type frame frame)
           (type slot slot))
  (let ((it (frame-ref-own-slot-values frame slot)))
    (if it
        (setf (slot-value%-values it) vals)
        (frame-add-own-slot-value frame slot vals))))

(defun (setf frame-own-slot-value) (frame slot val)
  (setf (frame-own-slot-values frame slot) `(,val)))

(defun frame-add-own-slot-value (frame slot vals)
  (declare (type frame frame)
           (type slot slot))
  (push (make-slot-value% :slot slot :values (if (listp vals) vals (list vals)))  
        (slot-value frame 'own-slot-values)))


;
; instance functions
;

(defun instance-add-direct-type (inst type)
  (declare (type instance inst)
           (type cls type))
  (push type (instance-direct-types inst)))

(defun instance-remove-direct-type (inst type)
  (declare (type instance inst)
           (type cls type))
  (setf (instance-direct-types inst)
        (delete type (instance-direct-types inst))))

(defun instance-has-direct-type (inst type)
  (declare (type instance inst)
           (type cls type))
  (find type (instance-direct-types inst)))

(defun instance-has-type (inst type)
  (declare (type instance inst)
           (type cls type))
  (labels ((check (ty)
             (or (eq type ty)
                 (some #'check (cls-direct-superclses ty)))))
    (some #'check (instance-direct-types inst))))
  
(defun instance-direct-type (inst)
  (car (instance-direct-types inst)))

(defun (setf instance-direct-type) (inst type)
  (setf (instance-direct-types inst) (list type)))


;
; cls functions
;

(defun cls-add-direct-supercls (cls sc)
  (declare (type cls cls)
           (type cls sc))
  (push sc (cls-direct-superclses cls)))

(defun cls-remove-direct-supercls (cls sc)
  (declare (type cls cls)
           (type cls sc))
  (setf (cls-direct-superclses cls)
        (delete sc (cls-direct-superclses cls))))

(defun cls-has-direct-supercls (cls sc)
  (declare (type cls cls)
           (type cls sc))
  (find sc (cls-direct-superclses cls)))

(defun cls-has-supercls (cls sc)
  (declare (type cls cls)
           (type cls sc))
  (or (cls-has-direct-supercls cls sc)
      (labels ((check (c)
                 (or (cls-has-direct-supercls c sc)
                     (some #'check (cls-direct-superclses c)))))
        (some #'check (cls-direct-superclses cls)))))
  
(defun cls-direct-supercls (cls)
  (car (cls-direct-superclses cls)))

(defun (setf cls-direct-supercls) (cls sc)
  (setf (cls-direct-superclses cls) (list sc)))

(defun cls-add-direct-template-slot (cls tempslot)
  (declare (type cls cls)
           (type slot tempslot))
  (push tempslot (cls-direct-template-slots cls)))

(defun cls-remove-direct-template-slot (cls slot)
  (declare (type cls cls)
           (type slot slot))
  (setf (cls-direct-template-slots cls)
        (delete slot (cls-direct-template-slots cls))))

(defun cls-has-direct-template-slot (cls slot)
  (declare (type cls cls)
           (type slot slot))
  (find slot (cls-direct-template-slots cls)))

(defun cls-has-template-slot (cls slot)
  (declare (type cls cls)
           (type slot slot))
  (or (cls-has-direct-template-slot cls slot)
      (labels ((check (c)
                 (or (cls-has-direct-template-slot c slot)
                     (some #'check (cls-direct-superclses c)))))
        (some #'check (cls-direct-superclses cls)))))

(defun cls-ref-direct-template-facet-values (cls slot facet)
  (declare (type cls cls)
           (type slot slot)
           (type facet facet))
  (find-if #'(lambda (x) 
               (and
                (eq (facet-value%-slot x) slot)
                (eq (facet-value%-facet x) facet)))
           (slot-value cls 'direct-template-facet-values)))

(defun cls-direct-template-facet-values (cls slot facet)
  (declare (type cls cls)
           (type slot slot)
           (type facet facet))
  (let ((it (cls-ref-direct-template-facet-values cls slot facet)))
    (if it
        (facet-value%-values it)
        nil)))

(defun cls-direct-template-facet-value (cls slot facet)
  (declare (type cls cls)
           (type slot slot)
           (type facet facet))
  (car (cls-direct-template-facet-values cls slot facet)))

(defun (setf cls-direct-template-facet-values) (cls slot facet vals)
  (declare (type cls cls)
           (type slot slot)
           (type facet facet))
  (let ((it (cls-ref-direct-template-facet-values cls slot facet)))
    (if it
        (setf (facet-value%-values it) vals)
        (cls-add-direct-template-facet-value cls slot facet vals))))

(defun (setf cls-direct-template-facet-value) (cls slot facet val)
  (setf (cls-direct-template-facet-values cls slot facet) `(,val)))

(defun cls-add-direct-template-facet-value (cls slot facet vals)
  (declare (type cls cls)
           (type slot slot)
           (type facet facet))
  (push (make-facet-value% :slot slot :facet facet :values (if (listp vals) vals (list vals)))  
        (slot-value cls 'direct-template-facet-values)))

(defun cls-ref-template-facet-values (cls slot facet)
  (declare (type cls cls)
           (type slot slot)
           (type facet facet))
  (or (cls-ref-direct-template-facet-values cls slot facet)
      (labels ((check (c)
                 (or (cls-ref-direct-template-facet-values c slot facet)
                     (some #'check (cls-direct-superclses c)))))
        (some #'check (cls-direct-superclses cls)))))

(defun cls-template-facet-values (cls slot facet)
  (declare (type cls cls)
           (type slot slot)
           (type facet facet))
  (let ((it (cls-ref-template-facet-values cls slot facet)))
    (if it
        (facet-value%-values it)
        nil)))

(defun cls-template-facet-value (cls slot facet)
  (declare (type cls cls)
           (type slot slot)
           (type facet facet))
  (car (cls-template-facet-values cls slot facet)))

(defun (setf cls-template-facet-values) (cls slot facet vals)
  (declare (type cls cls)
           (type slot slot)
           (type facet facet))
  (let ((it (cls-ref-template-facet-values cls slot facet)))
    (if it
        (setf (facet-value%-values it) vals)
        (cls-add-direct-template-facet-value cls slot facet vals))))

(defun (setf cls-template-facet-value) (cls slot facet val)
  (setf (cls-template-facet-values cls slot facet) `(,val)))


;
; slot functions
;

(defun slot-add-direct-superslot (slot sc)
  (declare (type slot slot)
           (type slot sc))
  (push sc (slot-direct-superslots slot)))

(defun slot-remove-direct-superslot (slot sc)
  (declare (type slot slot)
           (type slot sc))
  (setf (slot-direct-superslots slot)
        (delete sc (slot-direct-superslots slot))))

(defun slot-has-direct-superslot (slot sc)
  (declare (type slot slot)
           (type slot sc))
  (find sc (slot-direct-superslots slot)))

(defun slot-has-superslot (slot sc)
  (declare (type slot slot)
           (type slot sc))
  (labels ((check (c)
             (or (eq sc c)
                 (some #'check (slot-direct-superslots c)))))
    (some #'check (slot-direct-superslots slot))))
  
(defun slot-direct-superslot (slot)
  (car (slot-direct-superslots slot)))

(defun (setf slot-direct-superslot) (slot sc)
  (setf (slot-direct-superslots slot) (list sc)))


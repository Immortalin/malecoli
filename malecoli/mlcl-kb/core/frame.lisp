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
   (own-slot-values-list
    :ACCESSOR frame-own-slot-values-list
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
    :ACCESSOR cls-direct-template-facet-values-list
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
  (vals))


;
; template facet value
; 

(defstruct facet-value%
  (slot nil :type slot)
  (facet nil :type facet)
  (vals))


;
; frame designator
;

(deftype frame-designator ()
  `(or frame symbol))

(defun find-frame (frame-des &optional (errorp nil))
  (check-type frame-des frame-designator)
  "Return frame called NAME. If there is no such frame NIL is returned
if ERRORP is false, otherwise an error is signalled."
  (etypecase frame-des
             (symbol 
              (if (boundp frame-des)
                  (symbol-value frame-des)
                  (if errorp (error "Frame named ~S does not exist." frame-des) nil)))
             (frame frame-des)))


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
                         (slot-value frame 'own-slot-values-list)))

(defun frame-own-slot-values (frame slot-des)
  (let ((it (frame-ref-own-slot-values frame (find-frame slot-des))))
    (if it
        (slot-value%-vals it)
        nil)))

(defun frame-own-slot-value (frame slot-des)
  (car (frame-own-slot-values frame slot-des)))

(defun (setf frame-own-slot-values) (vals frame slot-des)
  (let ((slot (find-frame slot-des)))
    (let ((it (frame-ref-own-slot-values frame slot)))
      (if it
          (setf (slot-value%-vals it) (convert-values vals))
          (frame-add-own-slot-value frame slot vals)))))

(defun (setf frame-own-slot-value) (val frame slot-des)
  (setf (frame-own-slot-values frame slot-des) `(,val)))

(defun frame-add-own-slot-value (frame slot-des vals)
  (push (make-slot-value% :slot (find-frame slot-des) :vals (convert-values vals))  
        (slot-value frame 'own-slot-values-list)))

(defun convert-values (vals)
  (let ((vs (if (listp vals) vals (list vals))))
    (mapcar #'(lambda (v) (if (symbolp v) (symbol-value v) v)) vs)))

;
; instance functions
;

(defun instance-add-direct-type (inst cls-des)
  (push (find-frame cls-des) (instance-direct-types inst)))

(defun instance-remove-direct-type (inst cls-des)
  (setf (instance-direct-types inst)
        (delete (find-frame cls-des) (instance-direct-types inst))))

(defun instance-has-direct-type (inst cls-des)
  (find (find-frame cls-des) (instance-direct-types inst)))

(defun instance-has-type (inst cls-des)
  (let ((type (find-frame cls-des)))
    (labels ((check (ty)
               (or (eq type ty)
                   (some #'check (cls-direct-superclses ty)))))
      (some #'check (instance-direct-types inst)))))
  
(defun instance-direct-type (inst)
  (car (instance-direct-types inst)))

(defun (setf instance-direct-type) (cls-des inst)
  (setf (instance-direct-types inst) (list (find-frame cls-des))))


;
; cls functions
;

(defun cls-add-direct-supercls (cls cls-des)
  (push (find-frame cls-des) (cls-direct-superclses cls)))

(defun cls-remove-direct-supercls (cls cls-des)
  (setf (cls-direct-superclses cls)
        (delete (find-frame cls-des) (cls-direct-superclses cls))))

(defun cls-has-direct-supercls (cls cls-des)
  (find (find-frame cls-des) (cls-direct-superclses cls)))

(defun cls-has-supercls (cls cls-des)
  (or (cls-has-direct-supercls cls cls-des)
      (labels ((check (c)
                 (or (cls-has-direct-supercls c cls-des)
                     (some #'check (cls-direct-superclses c)))))
        (some #'check (cls-direct-superclses cls)))))
  
(defun cls-direct-supercls (cls)
  (car (cls-direct-superclses cls)))

(defun (setf cls-direct-supercls) (cls-des cls)
  (setf (cls-direct-superclses cls) (list (find-frame cls-des))))

(defun cls-add-direct-template-slot (cls slot-des)
  (push (find-frame slot-des) (cls-direct-template-slots cls)))

(defun cls-remove-direct-template-slot (cls slot-des)
  (setf (cls-direct-template-slots cls)
        (delete (find-frame slot-des) (cls-direct-template-slots cls))))

(defun cls-has-direct-template-slot (cls slot-des)
  (find (find-frame slot-des) (cls-direct-template-slots cls)))

(defun cls-has-template-slot (cls slot-des)
  (or (cls-has-direct-template-slot cls slot-des)
      (labels ((check (c)
                 (or (cls-has-direct-template-slot c slot-des)
                     (some #'check (cls-direct-superclses c)))))
        (some #'check (cls-direct-superclses cls)))))

(defun cls-ref-direct-template-facet-values (cls slot-des facet-des)
  (find-if #'(lambda (x) 
               (and
                (eq (facet-value%-slot x) (find-frame slot-des))
                (eq (facet-value%-facet x) (find-frame facet-des))))
           (slot-value cls 'direct-template-facet-values-list)))

(defun cls-direct-template-facet-values (cls slot-des facet-des)
  (let ((it (cls-ref-direct-template-facet-values cls slot-des facet-des)))
    (if it
        (facet-value%-vals it)
        nil)))

(defun cls-direct-template-facet-value (cls slot-des facet-des)
  (car (cls-direct-template-facet-values cls slot-des facet-des)))

(defun (setf cls-direct-template-facet-values) (vals cls slot-des facet-des)
  (let ((it (cls-ref-direct-template-facet-values cls slot-des facet-des)))
    (if it
        (setf (facet-value%-vals it) vals)
        (cls-add-direct-template-facet-value cls slot-des facet-des vals))))

(defun (setf cls-direct-template-facet-value) (val cls slot-des facet-des)
  (setf (cls-direct-template-facet-values cls slot-des facet-des) `(,val)))

(defun cls-add-direct-template-facet-value (cls slot-des facet-des vals)
  (push (make-facet-value% :slot (find-frame slot-des) :facet (find-frame facet-des) :vals (if (listp vals) vals (list vals)))  
        (slot-value cls 'direct-template-facet-values-list)))

(defun cls-ref-template-facet-values (cls slot-des facet-des)
  (or (cls-ref-direct-template-facet-values cls slot-des facet-des)
      (labels ((check (c)
                 (or (cls-ref-direct-template-facet-values c slot-des facet-des)
                     (some #'check (cls-direct-superclses c)))))
        (some #'check (cls-direct-superclses cls)))))

(defun cls-template-facet-values (cls slot-des facet-des)
  (let ((it (cls-ref-template-facet-values cls slot-des facet-des)))
    (if it
        (facet-value%-vals it)
        nil)))

(defun cls-template-facet-value (cls slot-des facet-des)
  (car (cls-template-facet-values cls slot-des facet-des)))

(defun (setf cls-template-facet-values) (vals cls slot-des facet-des)
  (let ((it (cls-ref-template-facet-values cls slot-des facet-des)))
    (if it
        (setf (facet-value%-vals it) vals)
        (cls-add-direct-template-facet-value cls slot-des facet-des vals))))

(defun (setf cls-template-facet-value) (val cls slot-des facet-des)
  (setf (cls-template-facet-values cls slot-des facet-des) `(,val)))


;
; slot functions
;

(defun slot-add-direct-superslot (slot slot-des)
  (push (find-frame slot-des) (slot-direct-superslots slot)))

(defun slot-remove-direct-superslot (slot slot-des)
  (setf (slot-direct-superslots slot)
        (delete (find-frame slot-des) (slot-direct-superslots slot))))

(defun slot-has-direct-superslot (slot slot-des)
  (find (find-frame slot-des) (slot-direct-superslots slot)))

(defun slot-has-superslot (slot slot-des)
  (let ((sc (find-frame slot-des)))
    (labels ((check (c)
               (or (eq sc c)
                   (some #'check (slot-direct-superslots c)))))
      (some #'check (slot-direct-superslots slot)))))
  
(defun slot-direct-superslot (slot)
  (car (slot-direct-superslots slot)))

(defun (setf slot-direct-superslot) (slot-des slot)
  (setf (slot-direct-superslots slot) (list (find-frame slot-des))))


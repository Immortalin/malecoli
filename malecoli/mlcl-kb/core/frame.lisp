;;;
;;; MaLeCoLi
;;; Copyright (C) 2008 Alessandro Serra
;;; 
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;

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
    :READER instance-direct-types
    :INITFORM nil)))


;
; A class of a kb
;

(defclass cls (instance)
  ((direct-superclses
    :READER cls-direct-superclses
    :INITFORM nil)
   (direct-template-slots
    :READER cls-direct-template-slots
    :INITFORM nil)
   (direct-template-facet-values
    :ACCESSOR cls-direct-template-facet-values-list
    :INITFORM nil)))


;
; A slot of an instance
;

(defclass slot (instance)
  ((direct-superslots
   :READER slot-direct-superslots
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
  `(or frame symbol string))

(defun find-frame (frame-des &optional (errorp nil))
  (check-type frame-des frame-designator)
  "Return frame designated by FRAME-DES. If there is no such frame NIL is returned
if ERRORP is false, otherwise an error is signalled."
  (etypecase frame-des
             (frame frame-des)
             (symbol 
              (if (and (boundp frame-des) (typep (symbol-value frame-des) 'frame))
                  (symbol-value frame-des)
                  (if errorp (error "Frame designated by ~S does not exist." frame-des) nil)))
             (string 
              (let ((fr (element-name->symbol frame-des)))
                (if (boundp fr)
                    (symbol-value fr)
                    (if errorp (error "Frame designated by ~S does not exist." frame-des) nil))))))

(defun find-cls (frame-des &optional (errorp nil))
  (let ((it (find-frame frame-des errorp)))
    (if (or (null it) (not (typep it 'cls)))
        (if errorp (error "Cls designated by ~S does not exist." frame-des) nil))
    it))

(defun find-slot (frame-des &optional (errorp nil))
  (format t "&&& ~A~%" frame-des)
  (let ((it (find-frame frame-des errorp)))
    (if (or (null it) (not (typep it 'slot)))
        (if errorp (error "Slot designated by ~S does not exist." frame-des) nil))
    it))

(defun find-facet (frame-des &optional (errorp nil))
  (let ((it (find-frame frame-des errorp)))
    (if (or (null it) (not (typep it 'facet)))
        (if errorp (error "Facet designated by ~S does not exist." frame-des) nil))
    it))
  
(defun find-simple-instance (frame-des &optional (errorp nil))
  (let ((it (find-frame frame-des errorp)))
    (if (or (null it) (not (typep it 'simple-instance)))
        (if errorp (error "Simple Instance designated by ~S does not exist." frame-des) nil))
    it))
  

;
; values converter
;

(defun convert-values% (vals)
  (let ((vs (if (listp vals) vals (list vals))))
    (labels ((conv (val)
               (etypecase val
                          (symbol 
                           (symbol-value val))
                          (list 
                           (convert-values% val))
                          (t
                           val))))
      (mapcar #'conv vs))))


;
; frame functions
;

(defmethod initialize-instance :after ((fr frame) &rest initargs)
  (declare (ignore initargs))
  (kb-intern fr (frame-kb fr)))

(defun frame-name (frame)
  (check-type frame frame)
  (kb-element-name frame))

; own slot values
(defun frame-ref-own-slot-values (frame slot-des)
  (check-type frame frame)
  (check-type slot-des frame-designator)
  (find-if #'(lambda (x) (eq (slot-value%-slot x) (find-slot slot-des))) 
                         (slot-value frame 'own-slot-values-list)))

(defun frame-own-slot-values (frame slot-des)
  (check-type frame frame)
  (check-type slot-des frame-designator)
  (let ((it (frame-ref-own-slot-values frame slot-des)))
    (and it (slot-value%-vals it))))

(defun frame-own-slot-value (frame slot-des)
  (check-type frame frame)
  (check-type slot-des frame-designator)
  (car (frame-own-slot-values frame slot-des)))

(defun (setf frame-own-slot-values) (vals frame slot-des)
  (check-type frame frame)
  (check-type slot-des frame-designator)
  (let ((it (frame-ref-own-slot-values frame slot-des)))
    (if it
        (setf (slot-value%-vals it) (convert-values% vals))
        (frame-add-own-slot-value frame slot-des vals))))

(defun (setf frame-own-slot-value) (val frame slot-des)
  (check-type frame frame)
  (check-type slot-des frame-designator)  
  (setf (frame-own-slot-values frame slot-des) `(,val)))

(defun frame-add-own-slot-value (frame slot-des vals)
  (check-type frame frame)
  (check-type slot-des frame-designator)
  (push (make-slot-value% :slot (find-slot slot-des) :vals (convert-values% vals))  
        (slot-value frame 'own-slot-values-list)))


;
; instance functions
;

; direct type
(defun instance-direct-type (inst)
  (check-type inst instance)
  (car (instance-direct-types inst)))

(defun (setf instance-direct-types) (cls-des-list inst)
  (check-type inst instance)
  (setf (slot-value inst 'direct-types) (mapcar #'find-cls cls-des-list)))

(defun (setf instance-direct-type) (cls-des inst)
  (check-type inst instance)
  (check-type cls-des frame-designator)  
  (setf (instance-direct-types inst) (list cls-des)))

(defun instance-add-direct-type (inst cls-des)
  (check-type inst instance)
  (check-type cls-des frame-designator)  
  (push (find-cls cls-des) (instance-direct-types inst)))

(defun instance-remove-direct-type (inst cls-des)
  (check-type inst instance)
  (check-type cls-des frame-designator)  
  (setf (instance-direct-types inst)
        (delete (find-cls cls-des) (instance-direct-types inst))))

(defun instance-has-direct-type (inst cls-des)
  (check-type inst instance)
  (check-type cls-des frame-designator)  
  (find (find-cls cls-des) (instance-direct-types inst)))

(defun instance-has-type (inst cls-des)
  (check-type inst instance)
  (check-type cls-des frame-designator) 
  (let ((type (find-cls cls-des)))
    (labels ((check (ty)
               (or (eq type ty)
                   (some #'check (cls-direct-superclses ty)))))
      (some #'check (instance-direct-types inst)))))
  

;
; cls functions
;

; direct supercls
(defun cls-direct-supercls (cls)
  (check-type cls cls)
  (car (cls-direct-superclses cls)))

(defun (setf cls-direct-superclses) (cls-des-list cls)
  (check-type cls cls)
  (setf (slot-value cls 'direct-superclses) (mapcar #'find-cls cls-des-list)))

(defun (setf cls-direct-supercls) (cls-des cls)
  (check-type cls cls)
  (check-type cls-des frame-designator)    
  (setf (cls-direct-superclses cls) (list cls-des)))

(defun cls-add-direct-supercls (cls cls-des)
  (check-type cls cls)
  (check-type cls-des frame-designator)    
  (push (find-cls cls-des) (cls-direct-superclses cls)))

(defun cls-remove-direct-supercls (cls cls-des)
  (check-type cls cls)
  (check-type cls-des frame-designator)    
  (setf (cls-direct-superclses cls)
        (delete (find-cls cls-des) (cls-direct-superclses cls))))

(defun cls-has-direct-supercls (cls cls-des)
  (check-type cls cls)
  (check-type cls-des frame-designator)    
  (find (find-cls cls-des) (cls-direct-superclses cls)))

(defun cls-has-supercls (cls cls-des)
  (check-type cls cls)
  (check-type cls-des frame-designator)    
  (let ((class (find-cls cls-des)))
    (labels ((check (c)
               (or (eq class c)
                   (some #'check (cls-direct-superclses c)))))
      (some #'check (cls-direct-superclses cls)))))
  
; direct template slot
(defun (setf cls-direct-template-slots) (slot-des-list cls)
  (check-type cls cls)
  (setf (slot-value cls 'direct-template-slots) (mapcar #'find-slot slot-des-list)))

(defun cls-add-direct-template-slot (cls slot-des)
  (check-type cls cls)
  (check-type slot-des frame-designator)      
  (push (find-slot slot-des) (cls-direct-template-slots cls)))

(defun cls-remove-direct-template-slot (cls slot-des)
  (check-type cls cls)
  (check-type slot-des frame-designator)        
  (setf (cls-direct-template-slots cls)
        (delete (find-slot slot-des) (cls-direct-template-slots cls))))

(defun cls-has-direct-template-slot (cls slot-des)
  (check-type cls cls)
  (check-type slot-des frame-designator)      
  (find (find-slot slot-des) (cls-direct-template-slots cls)))

(defun cls-has-template-slot (cls slot-des)
  (check-type cls cls)
  (check-type slot-des frame-designator)      
  (or (cls-has-direct-template-slot cls slot-des)
      (labels ((check (c)
                 (or (cls-has-direct-template-slot c slot-des)
                     (some #'check (cls-direct-superclses c)))))
        (some #'check (cls-direct-superclses cls)))))

; direct template facet values
(defun cls-ref-direct-template-facet-values (cls slot-des facet-des)
  (check-type cls cls)
  (check-type slot-des frame-designator)        
  (check-type facet-des frame-designator)        
  (find-if #'(lambda (x) 
               (and
                (eq (facet-value%-slot x) (find-slot slot-des))
                (eq (facet-value%-facet x) (find-facet facet-des))))
           (slot-value cls 'direct-template-facet-values-list)))

(defun cls-direct-template-facet-values (cls slot-des facet-des)
  (check-type cls cls)
  (check-type slot-des frame-designator)        
  (check-type facet-des frame-designator)        
  (let ((it (cls-ref-direct-template-facet-values cls slot-des facet-des)))
    (and it (facet-value%-vals it))))

(defun cls-direct-template-facet-value (cls slot-des facet-des)
  (check-type cls cls)
  (check-type slot-des frame-designator)        
  (check-type facet-des frame-designator)        
  (car (cls-direct-template-facet-values cls slot-des facet-des)))

(defun (setf cls-direct-template-facet-values) (vals cls slot-des facet-des)
  (check-type cls cls)
  (check-type slot-des frame-designator)        
  (check-type facet-des frame-designator)        
  (let ((it (cls-ref-direct-template-facet-values cls slot-des facet-des)))
    (if it
        (setf (facet-value%-vals it) (convert-values% vals))
        (cls-add-direct-template-facet-value cls slot-des facet-des vals))))

(defun (setf cls-direct-template-facet-value) (val cls slot-des facet-des)
  (check-type cls cls)
  (check-type slot-des frame-designator)        
  (check-type facet-des frame-designator)
  (setf (cls-direct-template-facet-values cls slot-des facet-des) `(,val)))

(defun cls-add-direct-template-facet-value (cls slot-des facet-des vals)
  (check-type cls cls)
  (check-type slot-des frame-designator)        
  (check-type facet-des frame-designator)        
  (push (make-facet-value% :slot (find-slot slot-des) 
                           :facet (find-facet facet-des) 
                           :vals (convert-values% vals))
        (slot-value cls 'direct-template-facet-values-list)))

; template facet values
(defun cls-ref-template-facet-values (cls slot-des facet-des)
  (check-type cls cls)
  (check-type slot-des frame-designator)        
  (check-type facet-des frame-designator)
  (or (cls-ref-direct-template-facet-values cls slot-des facet-des)
      (labels ((check (c)
                 (or (cls-ref-direct-template-facet-values c slot-des facet-des)
                     (some #'check (cls-direct-superclses c)))))
        (some #'check (cls-direct-superclses cls)))))

(defun cls-template-facet-values (cls slot-des facet-des)
  (check-type cls cls)
  (check-type slot-des frame-designator)        
  (check-type facet-des frame-designator)        
  (let ((it (cls-ref-template-facet-values cls slot-des facet-des)))
    (and it (facet-value%-vals it))))

(defun cls-template-facet-value (cls slot-des facet-des)
  (check-type cls cls)
  (check-type slot-des frame-designator)        
  (check-type facet-des frame-designator)        
  (car (cls-template-facet-values cls slot-des facet-des)))

(defun (setf cls-template-facet-values) (vals cls slot-des facet-des)
  (check-type cls cls)
  (check-type slot-des frame-designator)        
  (check-type facet-des frame-designator)        
  (let ((it (cls-ref-template-facet-values cls slot-des facet-des)))
    (if it
        (setf (facet-value%-vals it) vals)
        (cls-add-direct-template-facet-value cls slot-des facet-des vals))))

(defun (setf cls-template-facet-value) (val cls slot-des facet-des)
  (check-type cls cls)
  (check-type slot-des frame-designator)        
  (check-type facet-des frame-designator)
  (setf (cls-template-facet-values cls slot-des facet-des) `(,val)))


;
; slot functions
;

; direct superslot
(defun slot-direct-superslot (slot)
  (check-type slot slot)
  (car (slot-direct-superslots slot)))

(defun (setf slot-direct-superslots) (slot-des-list slot)
  (check-type slot slot)
  (setf (slot-value slot 'direct-superslots) (mapcar #'find-slot slot-des-list)))

(defun (setf slot-direct-superslot) (slot-des slot)
  (check-type slot slot)
  (check-type slot-des frame-designator)  
  (setf (slot-direct-superslots slot) (list slot-des)))

(defun slot-add-direct-superslot (slot slot-des)
  (check-type slot slot)
  (check-type slot-des frame-designator)  
  (push (find-slot slot-des) (slot-direct-superslots slot)))

(defun slot-remove-direct-superslot (slot slot-des)
  (check-type slot slot)
  (check-type slot-des frame-designator)  
  (setf (slot-direct-superslots slot)
        (delete (find-slot slot-des) (slot-direct-superslots slot))))

(defun slot-has-direct-superslot (slot slot-des)
  (check-type slot slot)
  (check-type slot-des frame-designator)  
  (find (find-slot slot-des) (slot-direct-superslots slot)))

(defun slot-has-superslot (slot slot-des)
  (check-type slot slot)
  (check-type slot-des frame-designator)  
  (let ((sc (find-slot slot-des)))
    (labels ((check (c)
               (or (eq sc c)
                   (some #'check (slot-direct-superslots c)))))
      (some #'check (slot-direct-superslots slot)))))
  
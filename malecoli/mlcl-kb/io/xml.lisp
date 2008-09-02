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

;;;; Created on 2008-08-18 11:41:45

(in-package :mlcl-kb)
      
;
; import a protege kb 
;

(defun kb-import-from-protege-xml (pathname kb)
  (check-type pathname pathname)
  (check-type kb kb)
  (if (probe-file pathname)
      (let ((*kb* kb))
        (with-open-file (strm pathname :direction :input)
                        (s-xml:start-parse-xml strm
                                               (make-instance 's-xml:xml-parser-state
                                                              :seed (make-seed)
                                                              :new-element-hook #'kb-import-new-element-hook
                                                              :finish-element-hook #'kb-import-finish-element-hook
                                                              :text-hook #'kb-import-text-hook))))
      (error "XML protege file ~S does not exist." pathname)))


;
; export a kb into a protege kb
;

(defun kb-export-to-protege-xml (pathname kb &key (supersedep t))
  (check-type pathname pathname)
  (check-type kb kb)
  (if (or supersedep (not (probe-file pathname)))
      (let ((s-xml:*local-namespace* :protege-ns))
        (with-open-file (strm pathname :direction :output :if-exists :supersede)
                        (format strm "~a ~a ~a~%" 
                                "<knowledge_base xmlns=\"http://protege.stanford.edu/xml\""
                                "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\""
                                "xsi:schemaLocation=\"http://protege.stanford.edu/xml http://protege.stanford.edu/xml/schema/protege.xsd\">")
                        (dolist (el (reverse (kb-interned-elements kb)))
                          ;(format t "~a~%" (s-xml:print-xml-string (kb-element-export-to-lxml kb el) :pretty t))
                          (format strm "~a~%" (s-xml:print-xml-string (kb-element-export-to-lxml kb el) :pretty t)))
                        (format strm "~a~%" "</knowledge_base>")))))


;
; namespaces
;

(in-package :mlcl-kb)

(defpackage :protege-ns
  (:export 
    |class|
    |slot|
    |facet|
    |simple_instance|
    |name|
    |type|
    |superclass|
    |value|
    |template_facet_value|
    |template_slot|
    |slot_reference|
    |facet_reference|
    |own_slot_value|
    |knowledge_base|
    |superslot|
    |value_type|
   ))

(s-xml:register-namespace "http://protege.stanford.edu/xml" "" :protege-ns)

(in-package :mlcl-kb)

;
; import functions
;

(defgeneric kb-import-element (el seed)
  (:method ((el instance) seed)
    (setf (instance-direct-types el) (seed-types seed))
    (setf (frame-own-slot-values-list el) (seed-own-slot-values seed))
    (setf (kb-element-definedp el) t))
  (:method ((el slot) seed)
    (call-next-method)
    (setf (slot-direct-superslots el) (seed-superslots seed)))
  (:method ((el cls) seed)
    (call-next-method)
    (setf (cls-direct-superclses el) (seed-superclasses seed))
    (setf (cls-direct-template-slots el) (seed-template-slots seed))
    (setf (cls-direct-template-facet-values-list el) (seed-template-facet-values seed))))

; seed

(defstruct seed
  (vals nil)
  (slot-reference nil)
  (facet-reference nil)
  (own-slot-values nil)
  (template-facet-values nil)
  (superclasses nil)
  (superslots nil)
  (template-slots nil)
  (name nil)
  (types nil)
  (text nil))

; hooks

(defun kb-import-new-element-hook (name attributes seed)
  (let ((new-seed (make-seed)))
    (cond 
     ((eq name 'protege-ns:|knowledge_base|)
      nil)
     ((eq name 'protege-ns:|class|)
      nil)
     ((eq name 'protege-ns:|slot|)
      nil)
     ((eq name 'protege-ns:|facet|)
      nil)
     ((eq name 'protege-ns:|simple_instance|)
      nil)
     ((eq name 'protege-ns:|name|)
      nil)
     ((eq name 'protege-ns:|type|)
      nil)
     ((eq name 'protege-ns:|own_slot_value|)
      nil)
     ((eq name 'protege-ns:|superclass|)
      nil)
     ((eq name 'protege-ns:|template_slot|)
      nil)
     ((eq name 'protege-ns:|template_facet_value|)
      nil)
     ((eq name 'protege-ns:|superslot|)
      nil)
     ((eq name 'protege-ns:|slot_reference|)
      nil)
     ((eq name 'protege-ns:|facet_reference|)
      nil)
     ((eq name 'protege-ns:|value|)
      nil)
     (t
      (format t "==> ~A  || ~A  || ~A  \\  ~A~%" seed name attributes seed)
      (error 'error :message "Error!!")))
    new-seed))

(defun kb-import-finish-element-hook (name attributes parent-seed seed)
  (cond 
   ((eq name 'protege-ns:|knowledge_base|)
    nil)
   ((eq name 'protege-ns:|class|)
    (let ((el (get-cls (seed-name seed))))
      (if (not (kb-element-definedp el))
          (kb-import-element el seed))))
   ((eq name 'protege-ns:|slot|)
    (let ((el (get-slot (seed-name seed))))
      (if (not (kb-element-definedp el))
          (kb-import-element el seed))))
   ((eq name 'protege-ns:|facet|)
    (let ((el (get-facet (seed-name seed))))
      (if (not (kb-element-definedp el))
          (kb-import-element el seed))))
   ((eq name 'protege-ns:|simple_instance|)
    (let ((el (get-simple-instance (seed-name seed))))
      (if (not (kb-element-definedp el))
          (kb-import-element el seed))))
   ((eq name 'protege-ns:|name|)
    (setf (seed-name parent-seed) (seed-text seed)))
   ((eq name 'protege-ns:|type|)
    (push (get-cls (seed-text seed)) (seed-types parent-seed)))
   ((eq name 'protege-ns:|own_slot_value|)
    (push (make-slot-value% :slot (seed-slot-reference seed) :vals (nreverse (seed-vals seed))) (seed-own-slot-values parent-seed)))
   ((eq name 'protege-ns:|superclass|)
    (push (get-cls (seed-text seed)) (seed-superclasses parent-seed)))
   ((eq name 'protege-ns:|template_slot|)
    (let ((slot (get-slot (seed-text seed))))
      (if (not (find slot (seed-template-slots parent-seed)))
          (push slot (seed-template-slots parent-seed)))))
   ((eq name 'protege-ns:|template_facet_value|)
    (push (make-facet-value% :slot (seed-slot-reference seed) :facet (seed-facet-reference seed) 
                                         :vals (nreverse (seed-vals seed))) (seed-template-facet-values parent-seed)))
   ((eq name 'protege-ns:|superslot|)
    (push (get-slot (seed-text seed)) (seed-superslots parent-seed)))
   ((eq name 'protege-ns:|slot_reference|)
    (setf (seed-slot-reference parent-seed) (get-slot (seed-text seed))))
   ((eq name 'protege-ns:|facet_reference|)
    (setf (seed-facet-reference parent-seed) (get-facet (seed-text seed))))
   ((eq name 'protege-ns:|value|)
    (push  (make-value-from-string (cdr (assoc :|value_type| attributes)) (seed-text seed))
           (seed-vals parent-seed)))
   (t
    (format t "<== ~A  || ~A  || ~A ~%" name attributes seed)
    (error 'error :message "Error!!")))
  parent-seed)

(defun kb-import-text-hook (string seed)
  ;(format t "@@@  ~A~%" string)
  (setf (seed-text seed) string)
  seed)


;
; export protege elements
;

(defgeneric kb-element-export-to-lxml (kb el))

(defmethod kb-element-export-to-lxml (kb (element kb-element))
  (declare (ignore kb)
           (ignore element))
  (let ((lxml nil))
    lxml))

(defmethod kb-element-export-to-lxml (kb (element instance))
  (declare (ignore kb))
  (let ((name (list 'protege-ns:|name| (frame-name element)))
        (types (mapcar #'(lambda (x) (list 'protege-ns:|type| (frame-name x))) (instance-direct-types element)))
        (ownsv (mapcar #'(lambda (x) (cons 'protege-ns:|own_slot_value| 
                                           (cons 
                                            (list 'protege-ns:|slot_reference| (frame-name (slot-value%-slot x)))
                                            (make-lxml-from-values (slot-value%-vals x)))))
                       (frame-own-slot-values-list element))))
    (cons name
          (append types ownsv))))
      
(defmethod kb-element-export-to-lxml (kb (element cls))
  (declare (ignore kb))
  (let ((lxml (call-next-method))
        (supcl (mapcar #'(lambda (x) (list 'protege-ns:|superclass| (frame-name x))) (cls-direct-superclses element)))
        (ts (mapcar #'(lambda (x) (list 'protege-ns:|template_slot| (frame-name x))) (cls-direct-template-slots element)))
        (tfv (mapcar #'(lambda (x) (cons 'protege-ns:|template_facet_value|
                                           (cons 
                                            (list 'protege-ns:|slot_reference| (frame-name (facet-value%-slot x)))
                                            (cons 
                                             (list 'protege-ns:|facet_reference| (frame-name (facet-value%-facet x)))
                                             (make-lxml-from-values (facet-value%-vals x))))))
                       (cls-direct-template-facet-values-list element))))
    (setf lxml (append
                lxml
                supcl
                ts
                tfv))
    (cons 'protege-ns:|class| lxml)))

(defmethod kb-element-export-to-lxml (kb (element slot))
  (declare (ignore kb))
  (let ((lxml (call-next-method))
        (supcl (mapcar #'(lambda (x) (list 'protege-ns:|superslot| (frame-name x))) (slot-direct-superslots element))))
    (setf lxml (append
                lxml
                supcl))
    (cons 'protege-ns:|slot| lxml)))


(defmethod kb-element-export-to-lxml (kb (element facet))
  (declare (ignore kb)
           (ignore element))  
  (let ((lxml (call-next-method)))
    (cons 'protege-ns:|facet| lxml)))

(defmethod kb-element-export-to-lxml (kb (element simple-instance))
  (declare (ignore kb)
           (ignore element))
  (let ((lxml (call-next-method)))
    (cons 'protege-ns:|simple_instance| lxml)))

;
; values
;

(defun make-value-from-string (ty v)
  (cond
   ((string= ty "boolean")
    (string= v "true"))
   ((string= ty "float")
    (float (read-from-string v)))
   ((string= ty "integer")
    (parse-integer v))
   ((string= ty "string")
    v)
   ((string= ty "class")
    (get-cls v))
   ((string= ty "slot")
    (get-slot v))
   ((string= ty "facet")
    (get-facet v))
   ((string= ty "simple_instance")
    (get-simple-instance v))))

(defun value-type-as-string (val)
  (cond
   ((eq val t)
    "boolean")
   ((eq val nil)
    "boolean")
   (t (etypecase val
                 (float
                  "float")
                 (integer 
                  "integer")
                 (string
                  "string")
                 (cls 
                  "class")
                 (slot
                  "slot")
                 (facet
                  "facet")
                 (simple-instance
                  "simple_instance")))))

(defun value-val-as-string (val)
  (cond
   ((eq val t)
    "true")
   ((eq val nil)
    "false")
   (t (etypecase val
                 (float
                  (format nil "~A" val))
                 (integer 
                  (format nil "~A" val))
                 (string
                  val)
                 (cls 
                  (frame-name val))
                 (slot
                  (frame-name val))
                 (facet
                  (frame-name val))
                 (simple-instance
                  (frame-name val))))))

(defun make-lxml-from-values (vals)
  (mapcar #'(lambda (val) (cons (list 'protege-ns:|value| 'protege-ns:|value_type| 
                                    (value-type-as-string val))
                              (list (value-val-as-string val))))
          vals))


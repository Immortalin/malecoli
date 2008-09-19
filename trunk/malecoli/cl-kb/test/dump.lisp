;;;; Created on 2008-04-23 16:33:28

(in-package :mlcl-kb)

;
; dump of a knowledge base
;

(defun kb-dump (strm &optional (kb *kb*))
  (dolist (el (kb-interned-elements kb))
    (if (typep el 'slot)
        (kb-element-dump el strm)))
  nil)


;
; dump of protege objects
;

(defgeneric kb-element-dump (element strm))


; methods
(defmethod kb-element-dump ((element instance) strm)
  (format strm "  id: ~A   name: ~A  types: ~A  definedp: ~A~%  own-slot-values: ~A~%" 
          element
          (frame-name element)
          (instance-direct-types element)
          (frame-definedp element)
          (frame-own-slot-values-list element)))

(defmethod kb-element-dump ((element cls) strm)
  (format strm "## class~%")
  (call-next-method)
  (format strm "  super-classes: ~A~%  template-slots: ~A~%  template-facet-values: ~A~%" 
          (cls-direct-superclses element)
          (cls-direct-template-slots element)
          (cls-direct-template-facet-values-list element))
  (format strm "  ~A ~A ~A ~A ~A ~%" 
          (cls-role element)
          (cls-abstractp element)
          (cls-concretep element)
          (cls-documentation element)
          (cls-constraints element)))

(defmethod kb-element-dump ((element slot) strm)
  (format strm "## slot~%")
  (call-next-method)
  (format strm "  super-slots: ~A ~%" 
          (slot-direct-superslots element))
  (format strm "  ~A  ~A ~A ~A ~A ~A ~A ~A ~A ~A ~A ~A~%" 
          (slot-value-type element)
          (slot-minimum-cardinality element)
          (slot-maximum-cardinality element)
          (slot-minimum-value element)
          (slot-maximum-value element)
          (slot-allowed-clses element)
          (slot-allowed-parents element)
          (slot-allowed-values element)
          (slot-defaults element)
          (slot-inverse-slot element)
          (slot-values element)
          (slot-associated-facet element)
          ))

(defmethod kb-element-dump ((element facet) strm)
  (format strm "## facet~%")
  (call-next-method)
  (format strm "  ~A  ~%" 
          (facet-documentation element)))

(defmethod kb-element-dump ((element simple-instance) strm)
  (format strm "## simple instance~%")
  (call-next-method))


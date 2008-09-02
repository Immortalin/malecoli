
;;;; Created on 2008-04-22 16:34:12

(in-package :mlcl-kb)

;
; protege knowledge base
;

(defvar *protege-kb* nil 
  "protege kb")             

(defpackage "PROTEGE-KB")


;
; protege constants
;

(in-package "PROTEGE-KB")

(cl:eval-when (:compile-toplevel :load-toplevel)
  (cl:when (cl:not (cl:boundp 'concrete-value))
           (cl:defconstant concrete-value "Concrete")
           (cl:defconstant abstract-value "Abstract")
           (cl:defconstant any-type-value "Any")
           (cl:defconstant boolean-type-value "Boolean")
           (cl:defconstant float-type-value "Float")                       
           (cl:defconstant integer-type-value "Integer")
           (cl:defconstant string-type-value "String")
           (cl:defconstant symbol-type-value "Symbol")
           (cl:defconstant instance-type-value "Instance")
           (cl:defconstant cls-type-value "Class")
           (cl:export '(concrete-value abstract-value any-type-value boolean-type-value float-type-value 
                                       integer-type-value string-type-value 
                                       symbol-type-value instance-type-value cls-type-value))))


;
; conversion functions
;

(cl:in-package :mlcl-kb)
  
(defun string->type-value (str)
          (cond
           ((string-equal str "Any")
            'protege-kb:any-type-value)
           ((string-equal str "Boolean")
            'protege-kb:boolean-type-value)
           ((string-equal str "Float")
            'protege-kb:float-type-value)
           ((string-equal str "Integer")
            'protege-kb:integer-type-value)
           ((string-equal str "String")
            'protege-kb:string-type-value)
           ((string-equal str "Symbol")
            'protege-kb:symbol-type-value)
           ((string-equal str "Instance")
            'protege-kb:instance-type-value)
           ((string-equal str "Class")
            'protege-kb:cls-type-value)))

(defun string->role (str)
          (cond
           ((string-equal str "Abstract")
            'protege-kb:abstract-value)
           ((string-equal str "Concrete")
            'protege-kb:concrete-value)))


;
; protege initialization
;

(defun initialize-protege-kb (kb)
  (let ((*kb* kb))
    (mlcl-kb:kb-get-slot ":ANNOTATED-INSTANCE")
    (mlcl-kb:kb-get-slot ":ANNOTATION-TEXT")
    (mlcl-kb:kb-get-slot ":ASSOCIATED-FACET")
    (mlcl-kb:kb-get-slot ":ASSOCIATED-SLOT")
    (mlcl-kb:kb-get-slot ":CREATION-TIMESTAMP")
    (mlcl-kb:kb-get-slot ":CREATOR")
    (mlcl-kb:kb-get-slot ":DIRECT-DOMAIN")
    (mlcl-kb:kb-get-slot ":DIRECT-INSTANCES")
    (mlcl-kb:kb-get-slot ":DIRECT-SUBCLASSES")
    (mlcl-kb:kb-get-slot ":DIRECT-SUBSLOTS")
    (mlcl-kb:kb-get-slot ":DIRECT-SUPERCLASSES")
    (mlcl-kb:kb-get-slot ":DIRECT-SUPERSLOTS")
    (mlcl-kb:kb-get-slot ":DIRECT-TEMPLATE-SLOTS")
    (mlcl-kb:kb-get-slot ":DIRECT-TYPE")
    (mlcl-kb:kb-get-slot ":DOCUMENTATION")
    (mlcl-kb:kb-get-slot ":FROM")
    (mlcl-kb:kb-get-slot ":MODIFICATION-TIMESTAMP")
    (mlcl-kb:kb-get-slot ":MODIFIER")
    (mlcl-kb:kb-get-slot ":NAME")
    (mlcl-kb:kb-get-slot ":PAL-DESCRIPTION")
    (mlcl-kb:kb-get-slot ":PAL-NAME")
    (mlcl-kb:kb-get-slot ":PAL-RANGE")
    (mlcl-kb:kb-get-slot ":PAL-STATEMENT")
    (mlcl-kb:kb-get-slot ":ROLE")
    (mlcl-kb:kb-get-slot ":SLOT-CONSTRAINTS")
    (mlcl-kb:kb-get-slot ":SLOT-DEFAULTS")
    (mlcl-kb:kb-get-slot ":SLOT-INVERSE")
    (mlcl-kb:kb-get-slot ":SLOT-MAXIMUM-CARDINALITY")
    (mlcl-kb:kb-get-slot ":SLOT-MINIMUM-CARDINALITY")
    (mlcl-kb:kb-get-slot ":SLOT-NUMERIC-MAXIMUM")
    (mlcl-kb:kb-get-slot ":SLOT-NUMERIC-MINIMUM")
    (mlcl-kb:kb-get-slot ":SLOT-VALUE-TYPE")
    (mlcl-kb:kb-get-slot ":SLOT-VALUES")
    (mlcl-kb:kb-get-slot ":TO")
  
    (mlcl-kb:kb-get-cls ":THING")
    (mlcl-kb:kb-get-cls ":SYSTEM-CLASS")
    (mlcl-kb:kb-get-cls ":META-CLASS")
    (mlcl-kb:kb-get-cls ":CLASS")
    (mlcl-kb:kb-get-cls ":STANDARD-CLASS")
    (mlcl-kb:kb-get-cls ":SLOT")
    (mlcl-kb:kb-get-cls ":STANDARD-SLOT")
    (mlcl-kb:kb-get-cls ":FACET")
    (mlcl-kb:kb-get-cls ":STANDARD-FACET")                 
    (mlcl-kb:kb-get-cls ":CONSTRAINT")
    (mlcl-kb:kb-get-cls ":PAL-CONSTRAINT")
    (mlcl-kb:kb-get-cls ":ANNOTATION")
    (mlcl-kb:kb-get-cls ":INSTANCE-ANNOTATION")
    (mlcl-kb:kb-get-cls ":RELATION")
    (mlcl-kb:kb-get-cls ":DIRECTED-BINARY-RELATION")
  
    (mlcl-kb:kb-get-facet ":CONSTRAINTS")
    (mlcl-kb:kb-get-facet ":DEFAULTS")
    (mlcl-kb:kb-get-facet ":DOCUMENTATION-IN-FRAME")
    (mlcl-kb:kb-get-facet ":INVERSE")
    (mlcl-kb:kb-get-facet ":MAXIMUM-CARDINALITY")
    (mlcl-kb:kb-get-facet ":MINIMUM-CARDINALITY")
    (mlcl-kb:kb-get-facet ":NUMERIC-MAXIMUM")
    (mlcl-kb:kb-get-facet ":NUMERIC-MINIMUM")
    (mlcl-kb:kb-get-facet ":VALUE-TYPE")
    (mlcl-kb:kb-get-facet ":VALUES")))
  
    
(defun make-protege-kb ()
  (let ((kb (make-instance 'kb :name "PROTEGE-KB" :protege-file nil)))
    (initialize-protege-kb kb)
    kb))

(eval-when (:LOAD-TOPLEVEL :EXECUTE)
  (if (null (mlcl-kb:find-kb "PROTEGE-KB"))
      (setf *protege-kb* (make-protege-kb))))

;
; cls
;
  
(defun cls-documentation (cls)
  (frame-own-slot-value cls 'PROTEGE-KB::|:DOCUMENTATION|))

(defun (setf cls-documentation) (v cls)
  (setf (frame-own-slot-value cls 'PROTEGE-KB::|:DOCUMENTATION|) v))

(defun cls-role (cls)
  (string->role (frame-own-slot-value cls 'PROTEGE-KB::|:ROLE|)))

(defun (setf cls-role) (v cls)
  (setf (frame-own-slot-value cls 'PROTEGE-KB::|:ROLE|) v))

(defun cls-abstractp (cls)
  (eq (cls-role cls) 'protege-kb:abstract-value))

(defun (setf cls-abstractp) (v cls)
  (setf (cls-role cls) (if v protege-kb:abstract-value protege-kb:concrete-value)))

(defun cls-concretep (cls)
  (eq (cls-role cls) 'protege-kb:concrete-value))

(defun (setf cls-concretep) (v cls)
  (setf (cls-role cls) (if v protege-kb:concrete-value protege-kb:abstract-value)))

(defun cls-constraints (cls)
  (frame-own-slot-values cls 'PROTEGE-KB::|:SLOT-CONSTRAINTS|))

(defun (setf cls-constraints) (vs cls)
  (setf (frame-own-slot-values cls 'PROTEGE-KB::|:SLOT-CONSTRAINTS|) vs))


;
; slot
;

(defun slot-documentation (slot)
  (frame-own-slot-value slot 'PROTEGE-KB::|:DOCUMENTATION|))

(defun (setf slot-documentation) (v slot)
  (setf (frame-own-slot-value slot 'PROTEGE-KB::|:DOCUMENTATION|) v))

(defun slot-value-type (slot)
  (string->type-value (frame-own-slot-value slot 'PROTEGE-KB::|:SLOT-VALUE-TYPE|)))

(defun (setf slot-value-type) (val slot)
  (setf (frame-own-slot-value slot 'PROTEGE-KB::|:SLOT-VALUE-TYPE|) val))

(defun slot-minimum-cardinality (slot)
  (frame-own-slot-value slot 'PROTEGE-KB::|:SLOT-MINIMUM-CARDINALITY|))

(defun (setf slot-minimum-cardinality) (v slot)
  (setf (frame-own-slot-value slot 'PROTEGE-KB::|:SLOT-MINIMUM-CARDINALITY|) v))

(defun slot-maximum-cardinality (slot)
  (frame-own-slot-value slot 'PROTEGE-KB::|:SLOT-MAXIMUM-CARDINALITY|))

(defun (setf slot-maximum-cardinality) (v slot)
  (setf (frame-own-slot-value slot 'PROTEGE-KB::|:SLOT-MAXIMUM-CARDINALITY|) v))

(defun slot-minimum-value (slot)
  (frame-own-slot-value slot 'PROTEGE-KB::|:SLOT-NUMERIC-MINIMUM|))

(defun (setf slot-minimum-value) (v slot)
  (setf (frame-own-slot-value slot 'PROTEGE-KB::|:SLOT-NUMERIC-MINIMUM|) v))

(defun slot-maximum-value (slot)
  (frame-own-slot-value slot 'PROTEGE-KB::|:SLOT-NUMERIC-MAXIMUM|))

(defun (setf slot-maximum-value) (v slot)
  (setf (frame-own-slot-value slot 'PROTEGE-KB::|:SLOT-NUMERIC-MAXIMUM|) v))

(defun slot-defaults (slot)
  (frame-own-slot-values slot 'PROTEGE-KB::|:SLOT-DEFAULTS|))

(defun (setf slot-defaults) (vs slot)
  (setf (frame-own-slot-values slot 'PROTEGE-KB::|:SLOT-DEFAULTS|) vs))

(defun slot-values (slot)
  (frame-own-slot-values slot 'PROTEGE-KB::|:SLOT-VALUES|))

(defun (setf slot-values) (vs slot)
  (setf (frame-own-slot-values slot 'PROTEGE-KB::|:SLOT-VALUES|) vs))

(defun slot-inverse-slot (slot)
  (frame-own-slot-value slot 'PROTEGE-KB::|:SLOT-INVERSE|))

(defun (setf slot-inverse-slot) (v slot)
  (setf (frame-own-slot-value slot 'PROTEGE-KB::|:SLOT-INVERSE|) v))

(defun slot-allowed-clses (slot)
  (and (eq (slot-value-type slot) 'protege-kb:instance-type-value)
       (cdr (frame-own-slot-values slot 'PROTEGE-KB::|:SLOT-VALUE-TYPE|))))

(defun (setf slot-allowed-clses) (vs slot)
  (setf (frame-own-slot-values slot 'PROTEGE-KB::|:SLOT-VALUE-TYPE|)
        (cons protege-kb:instance-type-value vs)))

(defun slot-allowed-parents (slot)
  (and (eq (slot-value-type slot) 'protege-kb:cls-type-value)
       (cdr (frame-own-slot-values slot 'PROTEGE-KB::|:SLOT-VALUE-TYPE|))))

(defun (setf slot-allowed-parents) (vs slot)
  (setf (frame-own-slot-values slot 'PROTEGE-KB::|:SLOT-VALUE-TYPE|)
        (cons protege-kb:cls-type-value vs)))

(defun slot-allowed-values (slot)
  (and (eq (slot-value-type slot) 'protege-kb:symbol-type-value)
       (cdr (frame-own-slot-values slot 'PROTEGE-KB::|:SLOT-VALUE-TYPE|))))

(defun (setf slot-allowed-values) (vs slot)
  (setf (frame-own-slot-values slot 'PROTEGE-KB::|:SLOT-VALUE-TYPE|)
        (cons protege-kb:symbol-type-value vs)))

(defun slot-constraints (cls)
  (frame-own-slot-values cls 'PROTEGE-KB::|:SLOT-CONSTRAINTS|))

(defun (setf slot-constraints) (vs cls)
  (setf (frame-own-slot-values cls 'PROTEGE-KB::|:SLOT-CONSTRAINTS|) vs))

(defun slot-associated-facet (slot)
  (frame-own-slot-value slot 'PROTEGE-KB::|:ASSOCIATED-FACET|))

(defun (setf slot-associated-facet) (vs slot)
  (setf (frame-own-slot-value slot 'PROTEGE-KB::|:ASSOCIATED-FACET|) vs))

;
; facet
;

(defun facet-documentation (facet)
  (frame-own-slot-value facet 'PROTEGE-KB::|:DOCUMENTATION|))

(defun (setf facet-documentation) (v facet)
  (setf (frame-own-slot-value facet 'PROTEGE-KB::|:DOCUMENTATION|) v))

(defun facet-associated-slot (facet)
  (frame-own-slot-value facet 'PROTEGE-KB::|:ASSOCIATED-SLOT|))

(defun (setf facet-associated-slot) (v facet)
  (setf (frame-own-slot-value facet 'PROTEGE-KB::|:ASSOCIATED-SLOT|) v))


;
; simple instance
;


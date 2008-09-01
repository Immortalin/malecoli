
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


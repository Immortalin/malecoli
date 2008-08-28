;;;; Created on 2008-04-22 16:34:12

(in-package :mlcl-kb)


(progn
  (defvar *protege-pathname*)
  (eval-when (:COMPILE-TOPLEVEL)
    (if (null (boundp '*protege-pathname*))
        (setq *protege-pathname*            
              #-sbcl (merge-pathnames
                      (make-pathname
                       :directory '(:relative ".." "resources")
                       :name "protege" :type "xml" :case :local)
                      *compile-file-truename*)
              #+sbcl #p"/hardmnt/tharpe0/sra/serra/Software/Developing/MaLeCoLi/code.google.com/workspace/malecoli-trunk/malecoli/mlcl-kb/resources/protege.xml")))
  (eval-when (:LOAD-TOPLEVEL :EXECUTE)
    (if (null (boundp '*protege-pathname*))
        (setq *protege-pathname*            
              #-sbcl (merge-pathnames
                      (make-pathname
                       :directory '(:relative ".." "resources")
                       :name "protege" :type "xml" :case :local)
                      *load-truename*)
              #+sbcl #p"/hardmnt/tharpe0/sra/serra/Software/Developing/MaLeCoLi/code.google.com/workspace/malecoli-trunk/malecoli/mlcl-kb/resources/protege.xml"))))

;
; protege knowledge base
;

(eval-when (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
  (if (null (find-kb "PROTEGE-KB"))
      (let ((*kb* (make-kb "PROTEGE-KB" :protege-file *protege-pathname*))))))

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
                       (cl:defconstant cls-type-value "Class"))
              (cl:export '(
                           concrete-value abstract-value any-type-value boolean-type-value float-type-value 
                           integer-type-value string-type-value 
                           symbol-type-value instance-type-value cls-type-value)))

(cl:eval-when (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
  (cl:let ((mlcl-kb:*kb* (mlcl-kb:find-kb "PROTEGE-KB")))
    (cl:if (cl:null (mlcl-kb::kb-loadedp mlcl-kb:*kb*))
           (cl:progn
#|
             (mlcl-kb:def-cls-ref ":::::STANDARD-CLASS")

             (mlcl-kb:def-slot-ref ":::::ANNOTATED-INSTANCE")
             (mlcl-kb:def-slot-ref ":::::ANNOTATION-TEXT")
             (mlcl-kb:def-slot-ref ":::::ASSOCIATED-FACET")
             (mlcl-kb:def-slot-ref ":::::ASSOCIATED-SLOT")
             (mlcl-kb:def-slot-ref ":::::CREATION-TIMESTAMP")
             (mlcl-kb:def-slot-ref ":::::CREATOR")
             (mlcl-kb:def-slot-ref ":::::DIRECT-DOMAIN")
             (mlcl-kb:def-slot-ref ":::::DIRECT-INSTANCES")
             (mlcl-kb:def-slot-ref ":::::DIRECT-SUBCLASSES")
             (mlcl-kb:def-slot-ref ":::::DIRECT-SUBSLOTS")
             (mlcl-kb:def-slot-ref ":::::DIRECT-SUPERCLASSES")
             (mlcl-kb:def-slot-ref ":::::DIRECT-SUPERSLOTS")
             (mlcl-kb:def-slot-ref ":::::DIRECT-TEMPLATE-SLOTS")
             (mlcl-kb:def-slot-ref ":::::DIRECT-TYPE")
             (mlcl-kb:def-slot-ref ":::::DOCUMENTATION")
             (mlcl-kb:def-slot-ref ":::::FROM")
             (mlcl-kb:def-slot-ref ":::::MODIFICATION-TIMESTAMP")
             (mlcl-kb:def-slot-ref ":::::MODIFIER")
             (mlcl-kb:def-slot-ref ":::::NAME")
             (mlcl-kb:def-slot-ref ":::::PAL-DESCRIPTION")
             (mlcl-kb:def-slot-ref ":::::PAL-NAME")
             (mlcl-kb:def-slot-ref ":::::PAL-RANGE")
             (mlcl-kb:def-slot-ref ":::::PAL-STATEMENT")
             (mlcl-kb:def-slot-ref ":::::ROLE")
             (mlcl-kb:def-slot-ref ":::::SLOT-CONSTRAINTS")
             (mlcl-kb:def-slot-ref ":::::SLOT-DEFAULTS")
             (mlcl-kb:def-slot-ref ":::::SLOT-INVERSE")
             (mlcl-kb:def-slot-ref ":::::SLOT-MAXIMUM-CARDINALITY")
             (mlcl-kb:def-slot-ref ":::::SLOT-MINIMUM-CARDINALITY")
             (mlcl-kb:def-slot-ref ":::::SLOT-NUMERIC-MAXIMUM")
             (mlcl-kb:def-slot-ref ":::::SLOT-NUMERIC-MINIMUM")
             (mlcl-kb:def-slot-ref ":::::SLOT-VALUE-TYPE")
             (mlcl-kb:def-slot-ref ":::::SLOT-VALUES")
             (mlcl-kb:def-slot-ref ":::::TO")
  |#        
            (mlcl-kb:def-cls-ref ":::::STANDARD-CLASS")
            (mlcl-kb:def-cls-ref ":THING")
            (mlcl-kb:def-cls-ref ":STANDARD-CLASS")
            (mlcl-kb:def-cls-ref ":STANDARD-SLOT")
            (mlcl-kb:def-cls-ref ":STANDARD-FACET")
            (mlcl-kb:def-slot-ref ":SLOT-VALUE-TYPE")
            
            (mlcl-kb:def-slot-ref ":::::ROLE")
            
            #|
            (mlcl-kb:def-cls ":::::THING" 
                             :superclses '(|:THING|)
                             :types '|:::::STANDARD-CLASS|)
             (mlcl-kb:def-cls ":::::SYSTEM-CLASS" 
                              :types '|:::::STANDARD-CLASS| 
                              :superclses '|:::::THING|)
             (mlcl-kb:def-cls ":::::META-CLASS" 
                              :types '|:::::STANDARD-CLASS| 
                              :superclses '|:::::SYSTEM-CLASS|)
             (mlcl-kb:def-cls ":::::CLASS" 
                              :types '|:::::STANDARD-CLASS| 
                              :superclses '|:::::META-CLASS|)
             (mlcl-kb:def-cls ":::::STANDARD-CLASS" 
                              :types '|:STANDARD-CLASS| 
                              :superclses '(|:STANDARD-CLASS| |:::::CLASS|))
            
            ))))
            |#
            
   
            (mlcl-kb:def-cls ":::::THING" 
                             :superclses '(|:THING|)
                             :types '|:::::STANDARD-CLASS|
                             ;:own-slot-values-list (cl:list (cl:cons '|:::::ROLE| abstract-value)))
                             )
         
            (mlcl-kb:def-cls ":::::SYSTEM-CLASS" 
                             :types '|:::::STANDARD-CLASS| 
                             :superclses '|:::::THING|
                             ;:own-slot-values-list (cl:list (cl:cons '|:::::ROLE| abstract-value)))
                             )

            (mlcl-kb:def-cls ":::::META-CLASS" 
                             :types '|:::::STANDARD-CLASS| 
                             :superclses '|:::::SYSTEM-CLASS| 
                             ;:template-slots (cl:list '|:::::NAME| '|:::::DIRECT-TYPE|)
                             ; :own-slot-values-list (cl:list (cl:cons '|:::::ROLE| abstract-value)))
                             )
            (mlcl-kb:def-cls ":::::CLASS" 
                             :types '|:::::STANDARD-CLASS| 
                             :superclses '|:::::META-CLASS|
                             ;:template-slots (cl:list '|:::::DIRECT-INSTANCES| '|:::::DIRECT-SUBCLASSES| '|:::::DIRECT-SUPERCLASSES| '|:::::DIRECT-TEMPLATE-SLOTS|)
                             ;:own-slot-values-list (cl:list (cl:cons '|:::::ROLE| abstract-value)))
                             )
            (mlcl-kb:def-cls ":::::STANDARD-CLASS" 
                             :types '(|:::::STANDARD-CLASS| |:STANDARD-CLASS|)
                             :superclses '(|:STANDARD-CLASS| |:::::CLASS|)
                             :template-slots (cl:list '|:::::ROLE|)
                             ;:template-slots (cl:list '|:::::DOCUMENTATION| '|:::::ROLE| '|:::::SLOT-CONSTRAINTS|)
                             ;:own-slot-values-list (cl:list (cl:cons '|:::::ROLE| concrete-value)))
                             )

            (mlcl-kb:def-cls ":::::SLOT" 
                             :types '|:::::STANDARD-CLASS| 
                             :superclses '|:::::META-CLASS|
                             ;:template-slots (cl:list '|:::::DIRECT-DOMAIN| '|:::::SLOT-VALUE-TYPE|)
                             ;:own-slot-values-list (cl:list (cl:cons '|:::::ROLE| abstract-value)))
                             )
            (mlcl-kb:def-cls ":::::STANDARD-SLOT" 
                             :types '|:::::STANDARD-CLASS| 
                             :superclses '(|:STANDARD-SLOT| |:::::SLOT|)
                             ;:template-slots (cl:list '|:::::ASSOCIATED-FACET| '|:::::DIRECT-SUBSLOTS| '|:::::DIRECT-SUPERSLOTS| '|:::::DOCUMENTATION| 
                             ;                         '|:::::SLOT-CONSTRAINTS| '|:::::SLOT-DEFAULTS| '|:::::SLOT-INVERSE| '|:::::SLOT-MAXIMUM-CARDINALITY| 
                             ;                         '|:::::SLOT-MINIMUM-CARDINALITY| '|:::::SLOT-NUMERIC-MAXIMUM| '|:::::SLOT-NUMERIC-MINIMUM| '|:::::SLOT-VALUE-TYPE| 
                             ;                         '|:::::SLOT-VALUES|)
                             ;:own-slot-values-list (cl:list (cl:cons '|:::::ROLE| concrete-value)))
                             )
            (mlcl-kb:def-cls ":::::FACET" 
                             :types '|:::::STANDARD-CLASS| 
                             :superclses '|:::::META-CLASS|
                             ;:template-slots (cl:list '|:::::ASSOCIATED-SLOT| '|:::::DOCUMENTATION|)
                             ;:own-slot-values-list (cl:list (cl:cons '|:::::ROLE| abstract-value)))
                             )
            (mlcl-kb:def-cls ":::::STANDARD-FACET" 
                             :types '|:::::STANDARD-CLASS| 
                             :superclses '(|:STANDARD-FACET| |:::::FACET|)
                             ;:own-slot-values-list (cl:list (cl:cons '|:::::ROLE| concrete-value)))
                             )
          
          (mlcl-kb:def-slot ":::::ROLE" 
                            :types '|:::::STANDARD-SLOT|
                            :own-slot-values-list (cl:list (cl:cons '|:SLOT-VALUE-TYPE| 
                                                                    (cl:list symbol-type-value 
                                                                             abstract-value concrete-value))))
                            
            #|
            (mlcl-kb:def-cls ":::::CONSTRAINT" 
                             :types '|:::::STANDARD-CLASS| 
                             :superclses '|:::::SYSTEM-CLASS|
                             :own-slot-values-list (cl:list (cl:cons '|:::::ROLE| abstract-value)))
            (mlcl-kb:def-cls ":::::PAL-CONSTRAINT" 
                             :types '|:::::STANDARD-CLASS| 
                             :superclses '|:::::CONSTRAINT|
                             :template-slots (cl:list '|:::::PAL-DESCRIPTION| '|:::::PAL-NAME| '|:::::PAL-RANGE| '|:::::PAL-STATEMENT|)
                             :own-slot-values-list (cl:list (cl:cons '|:::::ROLE| concrete-value)))
            (mlcl-kb:def-cls ":::::ANNOTATION" 
                             :types '|:::::STANDARD-CLASS| 
                             :superclses '|:::::SYSTEM-CLASS|
                             :own-slot-values-list (cl:list (cl:cons '|:::::ROLE| abstract-value)))
            (mlcl-kb:def-cls ":::::INSTANCE-ANNOTATION" 
                             :types '|:::::STANDARD-CLASS| 
                             :superclses '|:::::ANNOTATION|
                             :template-slots (cl:list '|:::::ANNOTATED-INSTANCE| '|:::::ANNOTATION-TEXT| '|:::::CREATION-TIMESTAMP| '|:::::CREATOR|)
                             :own-slot-values-list (cl:list (cl:cons '|:::::ROLE| concrete-value)))
            (mlcl-kb:def-cls ":::::RELATION" 
                             :types '|:::::STANDARD-CLASS| 
                             :superclses '|:::::SYSTEM-CLASS|
                             :own-slot-values-list (cl:list (cl:cons '|:::::ROLE| abstract-value)))
            (mlcl-kb:def-cls ":::::DIRECTED-BINARY-RELATION" 
                             :types '|:::::STANDARD-CLASS| 
                             :superclses '|:::::RELATION|
                             :template-slots (cl:list '|:::::FROM| '|:::::TO|)
                             :own-slot-values-list (cl:list (cl:cons '|:::::ROLE| concrete-value)))
            |#
            
            #|
        (mlcl-kb:def-slot ":::::ANNOTATED-INSTANCE" :types '|:::::STANDARD-SLOT|
                          :own-slot-values-list (cl:list (cl:cons '|:::::SLOT-VALUE-TYPE| (cl:list instance-type-value (mlcl-kb:find-frame '|:::::THING|)))))
        (mlcl-kb:def-slot ":::::ANNOTATION-TEXT" :types '|:::::STANDARD-SLOT|
                          :own-slot-values-list (cl:list (cl:cons '|:::::SLOT-VALUE-TYPE| (cl:list string-type-value))))
        (mlcl-kb:def-slot ":::::ASSOCIATED-FACET" :types '|:::::STANDARD-SLOT|
                          :own-slot-values-list (cl:list (cl:cons '|:::::SLOT-INVERSE| (mlcl-kb:find-frame '|:::::ASSOCIATED-SLOT|))
                                                         (cl:cons '|:::::SLOT-VALUE-TYPE| (cl:list  instance-type-value (mlcl-kb:find-frame '|:::::FACET|)))))
        (mlcl-kb:def-slot ":::::ASSOCIATED-SLOT" :types '|:::::STANDARD-SLOT|
                          :own-slot-values-list (cl:list (cl:cons '|:::::SLOT-INVERSE| (mlcl-kb:find-frame '|:::::ASSOCIATED-FACET|))
                                                         (cl:cons '|:::::SLOT-VALUE-TYPE| (cl:list instance-type-value (mlcl-kb:find-frame '|:::::SLOT|)))))
        (mlcl-kb:def-slot ":::::CREATION-TIMESTAMP" :types '|:::::STANDARD-SLOT|
                        :own-slot-values-list (cl:list (cl:cons '|:::::SLOT-VALUE-TYPE| (cl:list string-type-value))))
        (mlcl-kb:def-slot ":::::CREATOR" :types '|:::::STANDARD-SLOT|
                          :own-slot-values-list (cl:list (cl:cons '|:::::SLOT-VALUE-TYPE| (cl:list string-type-value))))
        (mlcl-kb:def-slot ":::::DIRECT-DOMAIN" :types '|:::::STANDARD-SLOT|
                          :own-slot-values-list (cl:list (cl:cons '|:::::SLOT-INVERSE| '|:::::DIRECT-TEMPLATE-SLOTS|)
                                                         (cl:cons '|:::::SLOT-VALUE-TYPE| (cl:list instance-type-value (mlcl-kb:find-frame '|:::::CLASS|)))))
        (mlcl-kb:def-slot ":::::DIRECT-INSTANCES" :types '|:::::STANDARD-SLOT|
                  :own-slot-values-list (cl:list (cl:cons '|:::::SLOT-INVERSE| '|:::::DIRECT-TYPE|)
                                                 (cl:cons '|:::::SLOT-VALUE-TYPE| (cl:list instance-type-value (mlcl-kb:find-frame '|:::::THING|)))))
        (mlcl-kb:def-slot ":::::DIRECT-SUBCLASSES" :types '|:::::STANDARD-SLOT|
                          :own-slot-values-list (cl:list (cl:cons '|:::::SLOT-INVERSE| '|:::::DIRECT-SUPERCLASSES|)
                                                         (cl:cons '|:::::SLOT-VALUE-TYPE| (cl:list cls-type-value (mlcl-kb:find-frame '|:::::THING|)))))
        (mlcl-kb:def-slot ":::::DIRECT-SUBSLOTS" :types '|:::::STANDARD-SLOT|
                          :own-slot-values-list (cl:list (cl:cons '|:::::SLOT-INVERSE| '|:::::DIRECT-SUPERSLOTS|)
                                                       (cl:cons '|:::::SLOT-VALUE-TYPE| (cl:list instance-type-value (mlcl-kb:find-frame '|:::::SLOT|)))))
        (mlcl-kb:def-slot ":::::DIRECT-SUPERCLASSES" :types '|:::::STANDARD-SLOT|
                          :own-slot-values-list (cl:list (cl:cons '|:::::SLOT-INVERSE| '|:::::DIRECT-SUBCLASSES|)
                                                         (cl:cons '|:::::SLOT-VALUE-TYPE| (cl:list cls-type-value (mlcl-kb:find-frame '|:::::THING|)))))
        (mlcl-kb:def-slot ":::::DIRECT-SUPERSLOTS" :types '|:::::STANDARD-SLOT|
                          :own-slot-values-list (cl:list (cl:cons '|:::::SLOT-INVERSE| '|:::::DIRECT-SUBSLOTS|)
                                                         (cl:cons '|:::::SLOT-VALUE-TYPE| (cl:list instance-type-value (mlcl-kb:find-frame '|:::::SLOT|)))))
        (mlcl-kb:def-slot ":::::DIRECT-TEMPLATE-SLOTS" :types '|:::::STANDARD-SLOT|
                          :own-slot-values-list (cl:list (cl:cons '|:::::SLOT-INVERSE| '|:::::DIRECT-DOMAIN|)
                                                         (cl:cons '|:::::SLOT-VALUE-TYPE| (cl:list instance-type-value '|:::::SLOT|))))
        (mlcl-kb:def-slot ":::::DIRECT-TYPE" :types '|:::::STANDARD-SLOT|
                          :own-slot-values-list (cl:list (cl:cons '|:::::SLOT-INVERSE| '|:::::DIRECT-INSTANCES|)
                                                         (cl:cons '|:::::SLOT-VALUE-TYPE| (cl:list cls-type-value '|:::::THING|))))
        (mlcl-kb:def-slot ":::::DOCUMENTATION" :types '|:::::STANDARD-SLOT|
                          :own-slot-values-list (cl:list (cl:cons '|:::::SLOT-VALUE-TYPE| (cl:list string-type-value))))
        (mlcl-kb:def-slot ":::::FROM" :types '|:::::STANDARD-SLOT|
                          :own-slot-values-list (cl:list (cl:cons '|:::::SLOT-VALUE-TYPE| (cl:list instance-type-value '|:::::THING|))))
        (mlcl-kb:def-slot ":::::MODIFICATION-TIMESTAMP" :types '|:::::STANDARD-SLOT|
                          :own-slot-values-list (cl:list (cl:cons '|:::::SLOT-VALUE-TYPE| (cl:list string-type-value))))
        (mlcl-kb:def-slot ":::::MODIFIER" :types '|:::::STANDARD-SLOT|
                          :own-slot-values-list (cl:list (cl:cons '|:::::SLOT-VALUE-TYPE| (cl:list string-type-value))))
        (mlcl-kb:def-slot ":::::NAME" :types '|:::::STANDARD-SLOT|
                          :own-slot-values-list (cl:list (cl:cons '|:::::SLOT-VALUE-TYPE| (cl:list string-type-value))))
        (mlcl-kb:def-slot ":::::PAL-DESCRIPTION" :types '|:::::STANDARD-SLOT|
                          :own-slot-values-list (cl:list (cl:cons '|:::::SLOT-VALUE-TYPE| (cl:list string-type-value))))
        (mlcl-kb:def-slot ":::::PAL-NAME" :types '|:::::STANDARD-SLOT|
                          :own-slot-values-list (cl:list (cl:cons '|:::::SLOT-VALUE-TYPE| (cl:list string-type-value))))
        (mlcl-kb:def-slot ":::::PAL-RANGE" :types '|:::::STANDARD-SLOT|
                          :own-slot-values-list (cl:list (cl:cons '|:::::SLOT-VALUE-TYPE| (cl:list string-type-value))))
        (mlcl-kb:def-slot ":::::PAL-STATEMENT" :types '|:::::STANDARD-SLOT|
                          :own-slot-values-list (cl:list (cl:cons '|:::::SLOT-VALUE-TYPE| (cl:list string-type-value))))
        (mlcl-kb:def-slot ":::::ROLE" :types '|:::::STANDARD-SLOT|
                          :own-slot-values-list (cl:list (cl:cons '|:::::SLOT-VALUE-TYPE| (cl:list symbol-type-value 
                                                                                               abstract-value concrete-value))))
        (mlcl-kb:def-slot ":::::SLOT-CONSTRAINTS" :types '|:::::STANDARD-SLOT|
                          :own-slot-values-list (cl:list (cl:cons '|:::::SLOT-VALUE-TYPE| (cl:list instance-type-value (mlcl-kb:find-frame '|:::::CONSTRAINT|)))))
        (mlcl-kb:def-slot ":::::SLOT-DEFAULTS" :types '|:::::STANDARD-SLOT|
                          :own-slot-values-list (cl:list (cl:cons '|:::::SLOT-VALUE-TYPE| (cl:list any-type-value))))
        (mlcl-kb:def-slot ":::::SLOT-INVERSE" :types '|:::::STANDARD-SLOT|
                          :own-slot-values-list (cl:list (cl:cons '|:::::SLOT-INVERSE| '|:::::SLOT-INVERSE|)
                                                         (cl:cons '|:::::SLOT-VALUE-TYPE| (cl:list instance-type-value (mlcl-kb:find-frame '|:::::SLOT|)))))
        (mlcl-kb:def-slot ":::::SLOT-MAXIMUM-CARDINALITY" :types '|:::::STANDARD-SLOT|
                          :own-slot-values-list (cl:list (cl:cons '|:::::SLOT-VALUE-TYPE| (cl:list integer-type-value))))
        (mlcl-kb:def-slot ":::::SLOT-MINIMUM-CARDINALITY" :types '|:::::STANDARD-SLOT|
                          :own-slot-values-list (cl:list (cl:cons '|:::::SLOT-VALUE-TYPE| (cl:list integer-type-value))))
        (mlcl-kb:def-slot ":::::SLOT-NUMERIC-MAXIMUM" :types '|:::::STANDARD-SLOT|
                          :own-slot-values-list (cl:list (cl:cons '|:::::SLOT-VALUE-TYPE| (cl:list float-type-value))))
        (mlcl-kb:def-slot ":::::SLOT-NUMERIC-MINIMUM" :types '|:::::STANDARD-SLOT|
                          :own-slot-values-list (cl:list (cl:cons '|:::::SLOT-VALUE-TYPE| (cl:list float-type-value))))
        (mlcl-kb:def-slot ":::::SLOT-VALUE-TYPE" :types '|:::::STANDARD-SLOT|
                          :own-slot-values-list (cl:list (cl:cons '|:::::SLOT-VALUE-TYPE| (cl:list any-type-value))))
        (mlcl-kb:def-slot ":::::SLOT-VALUES" :types '|:::::STANDARD-SLOT|
                          :own-slot-values-list (cl:list (cl:cons '|:::::SLOT-VALUE-TYPE| (cl:list any-type-value))))
        (mlcl-kb:def-slot ":::::TO" :types '|:::::STANDARD-SLOT|
                          :own-slot-values-list (cl:list (cl:cons '|:::::SLOT-VALUE-TYPE| (cl:list instance-type-value (mlcl-kb:find-frame '|:::::THING|)))))
        
        (mlcl-kb:def-facet ":::::CONSTRAINTS" :types '|:::::STANDARD-FACET|
                           :own-slot-values-list (cl:list (cl:cons '|:::::ASSOCIATED-SLOT| '|:::::SLOT-CONSTRAINTS|) ))
        (mlcl-kb:def-facet ":::::DEFAULTS" :types '|:::::STANDARD-FACET|
                           :own-slot-values-list (cl:list (cl:cons '|:::::ASSOCIATED-SLOT| '|:::::SLOT-DEFAULTS|) ))
        (mlcl-kb:def-facet ":::::DOCUMENTATION-IN-FRAME" :types '|:::::STANDARD-FACET|
                           :own-slot-values-list (cl:list (cl:cons '|:::::ASSOCIATED-SLOT| '|:::::DOCUMENTATION|)))
        (mlcl-kb:def-facet ":::::INVERSE" :types '|:::::STANDARD-FACET|)
        (mlcl-kb:def-facet ":::::MAXIMUM-CARDINALITY" :types '|:::::STANDARD-FACET|
                           :own-slot-values-list (cl:list (cl:cons '|:::::ASSOCIATED-SLOT| '|:::::SLOT-MAXIMUM-CARDINALITY|) ))
        (mlcl-kb:def-facet ":::::MINIMUM-CARDINALITY" :types '|:::::STANDARD-FACET|
                           :own-slot-values-list (cl:list (cl:cons '|:::::ASSOCIATED-SLOT| '|:::::SLOT-MINIMUM-CARDINALITY|) ))
        (mlcl-kb:def-facet ":::::NUMERIC-MAXIMUM" :types '|:::::STANDARD-FACET|
                           :own-slot-values-list (cl:list (cl:cons '|:::::ASSOCIATED-SLOT| '|:::::SLOT-NUMERIC-MAXIMUM|) ))
        (mlcl-kb:def-facet ":::::NUMERIC-MINIMUM" :types '|:::::STANDARD-FACET|
                           :own-slot-values-list (cl:list (cl:cons '|:::::ASSOCIATED-SLOT| '|:::::SLOT-NUMERIC-MINIMUM|) ))
        (mlcl-kb:def-facet ":::::VALUE-TYPE" :types '|:::::STANDARD-FACET|
                           :own-slot-values-list (cl:list (cl:cons '|:::::ASSOCIATED-SLOT| '|:::::SLOT-VALUE-TYPE|) ))
        (mlcl-kb:def-facet ":::::VALUES" :types '|:::::STANDARD-FACET|
                           :own-slot-values-list (cl:list (cl:cons '|:::::ASSOCIATED-SLOT| '|:::::SLOT-VALUES|)))))))

|#
))))
;
; conversion functions
;

(cl:in-package :mlcl-kb)
  
(defun string->type-value (str)
          (cond
           ((string-equal str "Any")
            'protege-kb::any-type-value)
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

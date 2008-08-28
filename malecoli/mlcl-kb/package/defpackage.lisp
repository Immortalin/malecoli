;;;; 2008-08-05 15:59:48


(in-package :common-lisp-user)

(defpackage :mlcl-kb
  (:nicknames :mlcl-kb :ml-kb)
  (:use :cl)
  (:export
    #|
    KB's Elements
    |#
    kb-element
    kb-element-name
    kb-element-definedp
    
    frame
    frame-kb
    frame-own-slot-values%
    frame-name
    frame-symbol
    frame-own-slot-values
    frame-own-slot-value
    frame-add-own-slot-value
    
    instance
    instance-direct-types
    instance-add-direct-type
    instance-remove-direct-type
    instance-has-direct-type
    instance-has-type
    instance-direct-type
    
    cls
    cls-direct-superclses
    cls-direct-template-slots
    cls-direct-template-facet-values%
    cls-add-direct-supercls
    cls-remove-direct-supercls
    cls-has-direct-supercls
    cls-has-supercls
    cls-direct-supercls
    cls-add-direct-template-slot
    cls-direct-template-facet-values
    cls-direct-template-facet-value
    cls-add-direct-template-facet-value  
    
    slot
    slot-direct-superslots
    slot-add-direct-superslot
    slot-remove-direct-superslot
    slot-has-direct-superslot
    slot-has-superslot
    slot-direct-superslot
    
    facet
    
    simple-instance
    
    make-slot-value%
    slot-value%-slot
    slot-value%-values
    
    make-facet-value%
    facet-value%-slot
    facet-value%-facet
    facet-value%-values
        
    find-frame 
    #|
    KB
    |#
    *kb*
    kb
    kb-name
    kb-package
    kb-interned-elements
    kb-use-list
    kb-protege-file
    kb-load
    kb-save
    
    make-kb
    delete-kb
    kb-clear
    
    list-all-kbs
    find-kb
    use-kb
    unuse-kb

    kb-find-element    
    kb-intern
    kb-unintern
    
    #|
    |#
    kb-make-cls
    kb-make-slot
    kb-make-facet
    kb-make-simple-instance
    kb-get-cls
    kb-get-slot
    kb-get-facet
    kb-get-simple-instance

    #|
    Defs
    |#    
    def-kb
    in-kb
    def-cls
    def-slot
    def-facet
    def-simple-instance
    
    def-cls-ref
    def-slot-ref
    def-facet-ref
    def-simple-instance-ref
    
    #|
    Protege
    |# 
    cls-documentation
    cls-role
    cls-abstractp
    cls-concretep
    cls-constraints
    slot-documentation
    slot-value-type
    slot-minimum-cardinality
    slot-maximum-cardinality
    slot-minimum-value
    slot-maximum-value
    slot-defaults
    slot-values
    slot-inverse-slot
    slot-allowed-clses
    slot-allowed-parents
    slot-allowed-values
    slot-constraints
    slot-associated-facet
    facet-documentation
    facet-associated-slot
    
    string->type-value
    string->role
   ))

(defpackage :mlcl-kbs)


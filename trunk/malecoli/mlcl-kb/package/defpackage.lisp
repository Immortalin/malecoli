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
    frame-own-slot-values-list
    frame-name
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
    cls-direct-template-facet-values-list
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

    kb-create
    kb-save
    kb-open
    kb-close
    kb-openedp
        
    kb-import-from-protege
    kb-export-to-protege
    
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
    make-cls
    make-slot
    make-facet
    make-simple-instance
    get-cls
    get-slot
    get-facet
    get-simple-instance

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

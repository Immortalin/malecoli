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
; protege initialization
;

(defun initialize-protege-kb (kb)
  (let ((*kb* kb))
    (mlcl-kb:get-slot ":ANNOTATED-INSTANCE")
    (mlcl-kb:get-slot ":ANNOTATION-TEXT")
    (mlcl-kb:get-slot ":ASSOCIATED-FACET")
    (mlcl-kb:get-slot ":ASSOCIATED-SLOT")
    (mlcl-kb:get-slot ":CREATION-TIMESTAMP")
    (mlcl-kb:get-slot ":CREATOR")
    (mlcl-kb:get-slot ":DIRECT-DOMAIN")
    (mlcl-kb:get-slot ":DIRECT-INSTANCES")
    (mlcl-kb:get-slot ":DIRECT-SUBCLASSES")
    (mlcl-kb:get-slot ":DIRECT-SUBSLOTS")
    (mlcl-kb:get-slot ":DIRECT-SUPERCLASSES")
    (mlcl-kb:get-slot ":DIRECT-SUPERSLOTS")
    (mlcl-kb:get-slot ":DIRECT-TEMPLATE-SLOTS")
    (mlcl-kb:get-slot ":DIRECT-TYPE")
    (mlcl-kb:get-slot ":DOCUMENTATION")
    (mlcl-kb:get-slot ":FROM")
    (mlcl-kb:get-slot ":MODIFICATION-TIMESTAMP")
    (mlcl-kb:get-slot ":MODIFIER")
    (mlcl-kb:get-slot ":NAME")
    (mlcl-kb:get-slot ":PAL-DESCRIPTION")
    (mlcl-kb:get-slot ":PAL-NAME")
    (mlcl-kb:get-slot ":PAL-RANGE")
    (mlcl-kb:get-slot ":PAL-STATEMENT")
    (mlcl-kb:get-slot ":ROLE")
    (mlcl-kb:get-slot ":SLOT-CONSTRAINTS")
    (mlcl-kb:get-slot ":SLOT-DEFAULTS")
    (mlcl-kb:get-slot ":SLOT-INVERSE")
    (mlcl-kb:get-slot ":SLOT-MAXIMUM-CARDINALITY")
    (mlcl-kb:get-slot ":SLOT-MINIMUM-CARDINALITY")
    (mlcl-kb:get-slot ":SLOT-NUMERIC-MAXIMUM")
    (mlcl-kb:get-slot ":SLOT-NUMERIC-MINIMUM")
    (mlcl-kb:get-slot ":SLOT-VALUE-TYPE")
    (mlcl-kb:get-slot ":SLOT-VALUES")
    (mlcl-kb:get-slot ":TO")
  
    (mlcl-kb:get-cls ":THING")
    (mlcl-kb:get-cls ":SYSTEM-CLASS")
    (mlcl-kb:get-cls ":META-CLASS")
    (mlcl-kb:get-cls ":CLASS")
    (mlcl-kb:get-cls ":STANDARD-CLASS")
    (mlcl-kb:get-cls ":SLOT")
    (mlcl-kb:get-cls ":STANDARD-SLOT")
    (mlcl-kb:get-cls ":FACET")
    (mlcl-kb:get-cls ":STANDARD-FACET")                 
    (mlcl-kb:get-cls ":CONSTRAINT")
    (mlcl-kb:get-cls ":PAL-CONSTRAINT")
    (mlcl-kb:get-cls ":ANNOTATION")
    (mlcl-kb:get-cls ":INSTANCE-ANNOTATION")
    (mlcl-kb:get-cls ":RELATION")
    (mlcl-kb:get-cls ":DIRECTED-BINARY-RELATION")
  
    (mlcl-kb:get-facet ":CONSTRAINTS")
    (mlcl-kb:get-facet ":DEFAULTS")
    (mlcl-kb:get-facet ":DOCUMENTATION-IN-FRAME")
    (mlcl-kb:get-facet ":INVERSE")
    (mlcl-kb:get-facet ":MAXIMUM-CARDINALITY")
    (mlcl-kb:get-facet ":MINIMUM-CARDINALITY")
    (mlcl-kb:get-facet ":NUMERIC-MAXIMUM")
    (mlcl-kb:get-facet ":NUMERIC-MINIMUM")
    (mlcl-kb:get-facet ":VALUE-TYPE")
    (mlcl-kb:get-facet ":VALUES")))
  
    
(defun make-protege-kb ()
  (let ((kb (make-instance 'kb :name "PROTEGE-KB" :protege-file nil)))
    (initialize-protege-kb kb)
    kb))

(eval-when (:LOAD-TOPLEVEL :EXECUTE)
  (if (null (mlcl-kb:find-kb "PROTEGE-KB"))
      (setf *protege-kb* (make-protege-kb))))

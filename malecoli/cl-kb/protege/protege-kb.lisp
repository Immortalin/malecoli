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

(in-package :cl-kb)

;
; protege knowledge base
;

(defvar *protege-kb* nil 
  "protege kb")      


;
; protege initialization
;

(defun initialize-protege-kb (kb)
  (let ((*kb* kb))
    (make-system-slot ":ANNOTATED-INSTANCE")
    (make-system-slot ":ANNOTATION-TEXT")
    (make-system-slot ":ASSOCIATED-FACET")
    (make-system-slot ":ASSOCIATED-SLOT")
    (make-system-slot ":CREATION-TIMESTAMP")
    (make-system-slot ":CREATOR")
    (make-system-slot ":DIRECT-DOMAIN")
    (make-system-slot ":DIRECT-INSTANCES")
    (make-system-slot ":DIRECT-SUBCLASSES")
    (make-system-slot ":DIRECT-SUBSLOTS")
    (make-system-slot ":DIRECT-SUPERCLASSES")
    (make-system-slot ":DIRECT-SUPERSLOTS")
    (make-system-slot ":DIRECT-TEMPLATE-SLOTS")
    (make-system-slot ":DIRECT-TYPE")
    (make-system-slot ":DOCUMENTATION")
    (make-system-slot ":FROM")
    (make-system-slot ":MODIFICATION-TIMESTAMP")
    (make-system-slot ":MODIFIER")
    (make-system-slot ":NAME")
    (make-system-slot ":PAL-DESCRIPTION")
    (make-system-slot ":PAL-NAME")
    (make-system-slot ":PAL-RANGE")
    (make-system-slot ":PAL-STATEMENT")
    (make-system-slot ":ROLE")
    (make-system-slot ":SLOT-CONSTRAINTS")
    (make-system-slot ":SLOT-DEFAULTS")
    (make-system-slot ":SLOT-INVERSE")
    (make-system-slot ":SLOT-MAXIMUM-CARDINALITY")
    (make-system-slot ":SLOT-MINIMUM-CARDINALITY")
    (make-system-slot ":SLOT-NUMERIC-MAXIMUM")
    (make-system-slot ":SLOT-NUMERIC-MINIMUM")
    (make-system-slot ":SLOT-VALUE-TYPE")
    (make-system-slot ":SLOT-VALUES")
    (make-system-slot ":TO")
  
    (make-system-cls ":THING")
    (make-system-cls ":SYSTEM-CLASS")
    (make-system-cls ":META-CLASS")
    (make-system-cls ":CLASS")
    (make-system-cls ":STANDARD-CLASS")
    (make-system-cls ":SLOT")
    (make-system-cls ":STANDARD-SLOT")
    (make-system-cls ":FACET")
    (make-system-cls ":STANDARD-FACET")                 
    (make-system-cls ":CONSTRAINT")
    (make-system-cls ":PAL-CONSTRAINT")
    (make-system-cls ":ANNOTATION")
    (make-system-cls ":INSTANCE-ANNOTATION")
    (make-system-cls ":RELATION")
    (make-system-cls ":DIRECTED-BINARY-RELATION")
  
    (make-system-facet ":CONSTRAINTS")
    (make-system-facet ":DEFAULTS")
    (make-system-facet ":DOCUMENTATION-IN-FRAME")
    (make-system-facet ":INVERSE")
    (make-system-facet ":MAXIMUM-CARDINALITY")
    (make-system-facet ":MINIMUM-CARDINALITY")
    (make-system-facet ":NUMERIC-MAXIMUM")
    (make-system-facet ":NUMERIC-MINIMUM")
    (make-system-facet ":VALUE-TYPE")
    (make-system-facet ":VALUES")))
      
(defun make-protege-kb ()
  (let ((kb (make-instance 'kb :protege-pprj-file (find-kb-file "protege"))))
    (initialize-protege-kb kb)
    kb))

(eval-when (:LOAD-TOPLEVEL :EXECUTE)
  (if (null (find-kb "protege" nil))
      (setf *protege-kb* (make-protege-kb))))

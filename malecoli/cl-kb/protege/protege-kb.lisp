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
    (get-slot ":ANNOTATED-INSTANCE")
    (get-slot ":ANNOTATION-TEXT")
    (get-slot ":ASSOCIATED-FACET")
    (get-slot ":ASSOCIATED-SLOT")
    (get-slot ":CREATION-TIMESTAMP")
    (get-slot ":CREATOR")
    (get-slot ":DIRECT-DOMAIN")
    (get-slot ":DIRECT-INSTANCES")
    (get-slot ":DIRECT-SUBCLASSES")
    (get-slot ":DIRECT-SUBSLOTS")
    (get-slot ":DIRECT-SUPERCLASSES")
    (get-slot ":DIRECT-SUPERSLOTS")
    (get-slot ":DIRECT-TEMPLATE-SLOTS")
    (get-slot ":DIRECT-TYPE")
    (get-slot ":DOCUMENTATION")
    (get-slot ":FROM")
    (get-slot ":MODIFICATION-TIMESTAMP")
    (get-slot ":MODIFIER")
    (get-slot ":NAME")
    (get-slot ":PAL-DESCRIPTION")
    (get-slot ":PAL-NAME")
    (get-slot ":PAL-RANGE")
    (get-slot ":PAL-STATEMENT")
    (get-slot ":ROLE")
    (get-slot ":SLOT-CONSTRAINTS")
    (get-slot ":SLOT-DEFAULTS")
    (get-slot ":SLOT-INVERSE")
    (get-slot ":SLOT-MAXIMUM-CARDINALITY")
    (get-slot ":SLOT-MINIMUM-CARDINALITY")
    (get-slot ":SLOT-NUMERIC-MAXIMUM")
    (get-slot ":SLOT-NUMERIC-MINIMUM")
    (get-slot ":SLOT-VALUE-TYPE")
    (get-slot ":SLOT-VALUES")
    (get-slot ":TO")
  
    (get-cls ":THING")
    (get-cls ":SYSTEM-CLASS")
    (get-cls ":META-CLASS")
    (get-cls ":CLASS")
    (get-cls ":STANDARD-CLASS")
    (get-cls ":SLOT")
    (get-cls ":STANDARD-SLOT")
    (get-cls ":FACET")
    (get-cls ":STANDARD-FACET")                 
    (get-cls ":CONSTRAINT")
    (get-cls ":PAL-CONSTRAINT")
    (get-cls ":ANNOTATION")
    (get-cls ":INSTANCE-ANNOTATION")
    (get-cls ":RELATION")
    (get-cls ":DIRECTED-BINARY-RELATION")
  
    (get-facet ":CONSTRAINTS")
    (get-facet ":DEFAULTS")
    (get-facet ":DOCUMENTATION-IN-FRAME")
    (get-facet ":INVERSE")
    (get-facet ":MAXIMUM-CARDINALITY")
    (get-facet ":MINIMUM-CARDINALITY")
    (get-facet ":NUMERIC-MAXIMUM")
    (get-facet ":NUMERIC-MINIMUM")
    (get-facet ":VALUE-TYPE")
    (get-facet ":VALUES")))
      
(defun make-protege-kb ()
  (let ((kb (make-instance 'kb :protege-pprj-file (find-kb-file "protege"))))
    (initialize-protege-kb kb)
    kb))

(eval-when (:LOAD-TOPLEVEL :EXECUTE)
  (if (null (find-kb "protege" nil))
      (setf *protege-kb* (make-protege-kb))))

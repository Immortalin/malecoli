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

;;;; 2008-08-21 09:30:59

(in-package :common-lisp-user)

(defpackage :mlcl-dataset
  (:nicknames :mlcl-dataset :ml-dataset)
  (:use :cl :mlcl-kb)
  (:export  
    #|
    case
    |#
    dataset-thing
    dataset-thing-name-id
    dataset-case
    dataset-case-id

    #|
    dataset
    |#
    dataset
    dataset-name
    dataset-schema
    dataset-storage
    dataset-cases  
    dataset-add-case
    
    #|
    schema
    |#
    schema
    schema-pathname
    schema-package
    schema-kb
    
    schema-name
    schema-load
    
    #|
    storage
    |#
    storage
    storage-pathname
    storage-cases
    storage-load
    storage-save
    storage-add-case
    storage-add-cases
    
    #|
    workspace
    |#
    workspace
    workspace-pathname
    workspace-storage
    workspace-schema
    workspace-datasets
    
    workspace-load
    workspace-save
    
    workspace-make-dataset
    workspace-cases-import
    
    
    #|
    arff
    |#
    
    arff->dataset-kb
   ))


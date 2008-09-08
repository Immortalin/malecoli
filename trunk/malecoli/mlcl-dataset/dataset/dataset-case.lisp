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

;;;; Created on 2008-09-03 17:34:24

(in-package :mlcl-dataset)

(clsql:def-view-class |DatasetThing| ()
                      ((name-id
                        :db-kind :key
                        :db-constraints :not-null
                        :reader |DatasetThing-NAME-ID|
                        :initarg :name-id
                        :type string))
                      (:base-table |DatasetThing|))

(clsql:def-view-class |DatasetCase| (|DatasetThing|)
                      ()
                      (:base-table |DatasetCase|))


(clsql:def-view-class enumeration ()
                      ((id
                        :db-kind :key
                        :db-constraints :not-null
                        :reader enumeration-id
                        :initarg :name-id
                        :type integer)
                       (name
                        :reader enumeration-name
                        :initarg :name
                        :type string))
                      (:base-table enumeration))


#|
(defun find-dataset-case (case-cls name-id)
  (clsql:locally-enable-sql-reader-syntax)
  (clsql:select case-cls :refresh t :where [= [slot-value '|DatasetThing| 'name-id] name-id))

(defun find-dataset-thing (name-id)
  (clsql:locally-enable-sql-reader-syntax)
  (clsql:select '|DatasetThing| :refresh t :where [= [slot-value '|DatasetThing| 'name-id] name-id))
|#

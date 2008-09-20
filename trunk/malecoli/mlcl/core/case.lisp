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

(in-package :mlcl)

;
; dataset thing
;

(defclass dataset-thing ()
  ((name-id
    :reader dataset-thing-name-id
    :initarg :name-id
    :type string)))


;
; dataset case
;
                 
(defclass dataset-case (dataset-thing)
  ((id
    :reader dataset-case-id
    :type fixnum
    :initarg :id)))


;
; dataset date
;

(defclass dataset-date ()
  ((|time_year|
    :ACCESSOR |time_year|
    :INITFORM 0 
    :type fixnum)
   (|time_month|
    :ACCESSOR |time_month|
    :INITFORM 0 
    :type fixnum)
   (|time_day| 
    :ACCESSOR |time_day|
    :INITFORM 0 
    :type fixnum)))

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

;;;; Created on 2008-09-12 11:06:01

(in-package :cl-kb)

(setf *kb-paths* '(#p"/hardmnt/tharpe0/sra/serra/Software/Developing/MaLeCoLi/runtime_ws/kbs/"))
(setf *kb-default-path* #p"/hardmnt/tharpe0/sra/serra/Software/Developing/MaLeCoLi/runtime_ws/kbs/")

(defmacro init-variable (var code)
  `(eval-when (:LOAD-TOPLEVEL :EXECUTE)
     (if (null (boundp ',var))
      (setq ,var ,code))))

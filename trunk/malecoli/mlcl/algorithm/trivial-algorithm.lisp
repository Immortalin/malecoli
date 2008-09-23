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

;;;; Created on 2008-09-18 16:55:34

(in-package :mlcl)

;
; trivial algorithm
;

(defclass trivial-algorithm (algorithm)
  ())


;
; trivial algorithm compiler
;

(defclass trivial-algorithm-compiler (algorithm-compiler)
  ())

(defmethod algorithm-compiler-compile ((algorithm-compiler trivial-algorithm-compiler) algo-frame schema-kb strm)
  (declare (ignore schema-kb))
  (let ((code (cl-kb:frame-own-slot-value algo-frame '|algorithm|::|trivial_algorithm_code|)))
    (format strm "~A~%~%" code))
  (list (cl-kb:frame-name algo-frame) `(make-instance 'trivial-algorithm :name ,(cl-kb:frame-name algo-frame))))

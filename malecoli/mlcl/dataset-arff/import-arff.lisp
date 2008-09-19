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

;;;; Created on 2008-08-26 09:57:12

(in-package :mlcl)

;
; convert an arff file into two dataset kbes
;

(defun arff->dataset-kb (arff-pathname dest-dir)
  (let* ((fn (pathname-name arff-pathname))
         (fnd (format nil "~A-data" fn))
         (kb (cl-kb:find-kb fn nil))
         (kbd (cl-kb:find-kb fnd nil)))
    (if (null kb)
        (progn
          (setf kb (cl-kb:make-kb (merge-pathnames
                                     (make-pathname 
                                      :name fn
                                      :type "pprj")
                                     dest-dir)
                                    :use '(cl-kbs::|dataset|)))
          (cl-kb:kb-create kb))
        (progn 
          (cl-kb:kb-open kb)
          (cl-kb:kb-clear kb)))
    (if (null kbd)
        (progn
          (setf kbd (cl-kb:make-kb (merge-pathnames
                                                   (make-pathname 
                                                    :name fnd
                                                    :type "pprj")
                                                   dest-dir)
                                    :use (list kb)))
          (cl-kb:kb-create kbd))
        (progn
          (cl-kb:kb-open kbd)
          (cl-kb:kb-clear kbd)))
    (arff-import arff-pathname kb kbd)
    (cl-kb:kb-save kb)
    (cl-kb:kb-close kb)
    (cl-kb:kb-save kbd)
    (cl-kb:kb-close kbd)
    (values kb kbd)))


;
; import
;

(defun arff-import (pathname kb kbd)
  (with-open-file (strm pathname)
                  (multiple-value-bind (relation-name attributes comments) (arff-read-header strm)
                    (arff-import-header relation-name attributes comments kb)
                    (arff-import-data relation-name attributes strm kbd))))


;
; import arff header
;

(defun arff-import-header (relation-name attributes comments kb)
  (let ((cl-kb:*kb* kb))
    (let ((insi (cl-kb:mk-simple-instance (format nil "this") '|dataset|::|DatasetDescription|))
          (cacl (cl-kb:mk-cls (format nil "~ACase" relation-name) :supercls '|dataset|::|DatasetCase|))
          (slots nil))
      (dolist (attr attributes)
        (let ((slot (cl-kb:mk-slot (format nil "~A_~A" relation-name (car attr)))))
          (cl-kb:cls-add-direct-template-slot cacl slot)
          (cond 
           ((eq (cdr attr) 'real)
            (setf (cl-kb:slot-value-type slot) cl-kb::float-type-value))
           ((eq (cdr attr) 'numeric)
            (setf (cl-kb:slot-value-type slot) cl-kb:float-type-value))
           ((eq (cdr attr) 'integer)
            (setf (cl-kb:slot-value-type slot) cl-kb:integer-type-value))
           ((eq (cdr attr) 'string)
            (setf (cl-kb:slot-value-type slot) cl-kb:string-type-value))
           ((eq (cdr attr) 'date)
            (setf (cl-kb:slot-value-type slot) (list cl-kb:instance-type-value 
                                                       '|dataset|::|time|)))
           ((and (listp (cdr attr)) (eq (second attr) 'nominal))
            (setf (cl-kb:slot-value-type slot) (cons cl-kb:symbol-type-value
                                                (caddr attr)))))
          (setf (cl-kb:slot-maximum-cardinality slot) 1)
          (push slot slots)))
      (setf (cl-kb:frame-own-slot-value insi '|dataset|::|dataset_name|) relation-name)
      (setf (cl-kb:frame-own-slot-value insi '|dataset|::|dataset_version|) "0.0.1")
      (setf (cl-kb:frame-own-slot-value insi '|dataset|::|dataset_comment|) (format nil "~{~a~%~}" comments))
      (setf (cl-kb:frame-own-slot-value insi '|dataset|::|dataset_default_target_slot|) (first slots)))))


;      
; import arff data
;

(defun arff-import-data (relation-name attributes strm kbd)
  (let ((cl-kb:*kb* kbd)
        (seed nil)
        (num 0)
        (dssi nil))
    (setf dssi (cl-kb:mk-simple-instance (format nil "~A-ds" relation-name) '|dataset|::|Dataset|))
    (arff-read-data attributes strm 
                    seed
                    #'(lambda (seed) 
                        (declare (ignore seed))
                        (let ((si (cl-kb:mk-simple-instance (format nil "case-~9,'0d" num) (format nil "~ACase" relation-name))))
                          (setf num (+ 1 num))
                          (push si (cl-kb:frame-own-slot-values 
                                    dssi '|dataset|::|dataset_case|))
                          si))
                    #'(lambda (seed) 
                        (declare (ignore seed))
                        nil)
                    #'(lambda (seed attr val) 
                        (if (not (string-equal val "?"))
                            (let ((valc val))
                              (cond 
                               ((eq (cdr attr) 'real)
                                (setf valc (float (with-input-from-string (strm val)
                                                                   (read strm)))))
                               ((eq (cdr attr) 'numeric)
                                (setf valc (with-input-from-string (strm val)
                                                                   (read strm))))
                               ((eq (cdr attr) 'integer)
                                (setf valc (parse-integer val)))
                               ((eq (cdr attr) 'date)
                                (setf valc nil)))
                              (setf (cl-kb:frame-own-slot-value seed (format nil "~A_~A" relation-name (car attr))) valc)))))))


;
; read arff header
;
  
(defun arff-read-header (strm)
  (let ((relation-name nil)
        (attributes nil)
        (comments nil))
    (do ((line (arff-read-header-line strm)
               (arff-read-header-line strm)))
        ((or (null line)
             (string-equal "data" (first line))))
      (cond 
       ((string-equal (first line) "%")
        (push (second line) comments))
       ((string-equal (first line) "relation")
        (setf relation-name (cl-ppcre:register-groups-bind (nil v1 v2) 
                                                ("\\s*((\\w+)|\"(.*)\")\\s*" (second line)) 
                                                (or v1 v2))))
       ((string-equal (first line) "attribute")
        (push (cl-ppcre:register-groups-bind (nil v1 v2 v3 nil v4 v5) 
                                             ("\\s*('([^']+)'|([\\w-']+)|\"([^\"]+)\")\\s*((\\w+)|{([^}]*)})" (second line)) 
                                             (cons (or v1 v2 v3) (arff-read-type v4 v5)))
              attributes))))      
    (setf comments (nreverse comments))
    (setf attributes (nreverse attributes))
    (values relation-name attributes comments)))

(defun arff-read-type (v3 v4)
  (if v3
      (cond 
       ((string-equal v3 "numeric") 'numeric)
       ((string-equal v3 "real") 'real)
       ((string-equal v3 "integer") 'integer)
       ((string-equal v3 "string") 'string)
       ((string-equal v3 "date") 'date))
      (progn
        (let ((vals (cl-ppcre:split "(\\s*,\\s*)" v4)))
          (setf (car vals) (string-left-trim " " (car vals)))
          (do ((v vals (cdr v)))
              ((null v) nil)
            (setf (car v) (string-trim " " (car v)))
            (setf (car v) (string-trim "'" (car v))))
          (list 'nominal vals)))))

(defun arff-read-header-line (strm)
  (let ((line (read-line strm nil))
        (it nil))
    (setf it (cl-ppcre:register-groups-bind (v1) ("((^%(.*)$)|(^\\s*$))" line) v1))
    (if it 
        (list "%" it)
        (progn
          (setf it (cl-ppcre:register-groups-bind (v1 v2) ("@(\\w+)\\s+(.*)$" line) (list v1 v2)))
          (if it
              it
              nil)))))


;
; read arff data
;

(defun arff-read-data (attributes strm seed new-record-fn finish-record-fn field-fn)
  (do ((line (arff-read-data-line strm)
               (arff-read-data-line strm)))
        ((null line))
    (if (eq (first line) 'values)
        (progn
          (setf seed (funcall new-record-fn seed))
          (do ((v (rest line) (cdr v)) (attr attributes (cdr attr)))
              ((and (null attr) (null v)))
            (funcall field-fn seed (car attr) (car v)))
          (setf seed (funcall finish-record-fn seed))))))

(defun arff-read-data-line (strm)
  (let ((line (read-line strm nil))
        (it nil))
    (if (null line)
        nil
        (progn
          (setf it (cl-ppcre:register-groups-bind (v1) ("((%.*$)|(^\\s*$))" line) v1))
          (if it 
              (list 'comment it)
              (let ((vals (cl-ppcre:split #\, line)))
                (do ((v vals (cdr v)))
                    ((null v) nil)
                  (setf (car v) (string-trim "'" (car v))))  
                (cons 'values vals)))))))
              
              

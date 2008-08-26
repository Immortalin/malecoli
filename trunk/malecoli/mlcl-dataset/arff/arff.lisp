;;;; Created on 2008-08-26 09:57:12

(in-package :mlcl-dataset)

(defun arff-import (pathname)
  (let ((kb nil))
    (with-open-file (strm pathname)
                    (multiple-value-bind (relation-name attributes comments) (arff-import-header strm)
                      (arff-import-data kb attributes strm)))
    kb))


(defun arff-import-read-type (v3 v4)
  (if v3
      (cond 
       ((string-equal v3 "numeric") 'numeric)
       ((string-equal v3 "real") 'real)
       ((string-equal v3 "integer") 'integer)
       ((string-equal v3 "string") 'string)
       ((string-equal v3 "date") 'date))
      (progn
        (format t "$$ ~A~%" v4)
        (list 'nominal (cl-ppcre:split "[#\,\\s]" v4)))))

(defun arff-import-read-line (strm)
  (let ((line (read-line strm nil))
        (it nil))
    (setf it (cl-ppcre:register-groups-bind (v1) ("%(.+)$" line) v1))
    (if it 
        (list "%" it)
        (progn
          (setf it (cl-ppcre:register-groups-bind (v1 v2) ("@(\\w+)\\s+(.*)$" line) (list v1 v2)))
          (if it
              it
              nil)))))

(defun arff-import-read-data-line (strm)
  (let ((line (read-line strm nil))
        (it nil))
    (if (null line)
        nil
        (progn
          (setf it (cl-ppcre:register-groups-bind (v1) ("%(.*)$" line) v1))
          (if it 
              (list "%" it)              
              (cons 'values (cl-ppcre:split "[#\,\\s]" line)))))))
  
(defun arff-import-header (strm)
  (let ((relation-name nil)
        (attributes nil)
        (comments nil))
    (do ((line (arff-import-read-line strm)
               (arff-import-read-line strm)))
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
        (push (cl-ppcre:register-groups-bind (nil v1 v2 nil v3 v4) 
                                             ("\\s*(([\\w-]+)|\"(.*)\")\\s*((\\w+)|{(.*)})" (second line)) 
                                             (cons (or v1 v2) (arff-import-read-type v3 v4)))
              attributes))))      
    (setf comments (nreverse comments))
    (setf attributes (nreverse attributes))
    ;(format t "############### ~A ########### ~%" relation-name)
    ;(format t "!! ~A~%" comments)
    ;(format t "!! ~A~%" attributes)
    ;(format t "########################## ~%")
    (values relation-name attributes comments)))

(defun arff-import-data (kb attributes strm)
  (do ((line (arff-import-read-data-line strm)
               (arff-import-read-data-line strm)))
        ((null line))
    (if (eq (first line) 'values)
        (progn
          (do ((v (rest line) (cdr v)) (attr attributes (cdr attr)))
              ((and (null attr) (null v)))
            (format t "~A=~A " (car (car attr)) (car v)))
          (format t "~%"))))
  (dolist (attr attributes) 
    (format t "~A~%" attr)))

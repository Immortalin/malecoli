;;;; Created on 2008-11-17 17:15:29

(in-package :clone-kb)

;
; model import 
;

(defun xml-message-import (pathname)
  (let ((seed (make-message-seed))
        (message nil))
    (setf message (message-seed-message seed))
    (with-open-file (strm pathname :direction :input)
                    (s-xml:start-parse-xml strm
                                           (make-instance 's-xml:xml-parser-state
                                                          :seed seed
                                                          :new-element-hook #'message-import-new-element-hook
                                                          :finish-element-hook #'message-import-finish-element-hook
                                                          :text-hook #'message-import-text-hook)))
    message))


;
; namespaces
;


;
; model seed
;

(defstruct message-seed
  (message (make-message))
  (item nil)
  (attributes)
  (text nil))


;
; xml hooks
;

  
(defun message-import-new-element-hook (name attributes seed)
  (let ((new-seed (make-message-seed :message (message-seed-message seed)
                             :item (message-seed-item seed)
                             :inim (message-seed-inim seed))))
    ;
    ;
     new-seed))

(defun message-import-finish-element-hook (name attributes parent-seed seed)
  ;
  ;
  parent-seed)

(defun message-import-text-hook (string seed)
  (setf (message-seed-text seed) string)
  seed)

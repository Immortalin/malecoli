;;;; Created on 2008-11-19 15:06:42

(in-package :clone-kb)

;
; negotiation import 
;

(defun xml-negotiation-import (pathname)
  (let ((negotiation (make-negotiation)))
    (setf (negotiation-model negotiation) 
          (xml-model-import ;(merge-pathnames 
                            ; (make-pathname
                            ;  :type "nmi")
                             pathname)) ;)
    (setf (negotiation-messages negotiation) 
          (xml-messages-import (merge-pathnames 
                                (make-pathname :type "xml" 
                                               :name (format nil "~A-messages" (pathname-name pathname)))
                                   pathname)))
    (setf (negotiation-state negotiation)
          (state-import (merge-pathnames 
                         (make-pathname :type "txt" 
                                        :name (format nil "~A-state" (pathname-name pathname)))
                         pathname)))
    (let ((outpathname (merge-pathnames 
                        (make-pathname
                         :name (format nil "~A-data" (pathname-name pathname))
                         :type "lisp")
                        pathname)))
      (with-open-file (strm outpathname :direction :output :if-exists :supersede)
                      (format strm "~S~%" negotiation)))
    negotiation))

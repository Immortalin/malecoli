;;;; Created on 2008-09-18 16:28:13

(in-package :mlcl)

(defclass makefile ()
  ((file 
    :READER makefile-pprj-file
    :INITARG :file
    :TYPE pathname)
   (schema
    :READER makefile-dataset-schema
    :INITARG :schema
    :TYPE schema)
   (package
    :READER makefile-package
    :INITARG :package
    :TYPE package)
   (kb
    :TYPE cl-kb:kb
    :INITARG :kb
    :INITFORM nil
    :READER makefile-kb)))


(defmethod initialize-instance :after ((makefile makefile) &rest initargs)
  (declare (ignore initargs))
  (setf (slot-value makefile 'package) 
        (or (find-package (format nil "~A-algo" (makefile-name makefile))) 
            (make-package (format nil "~A-algo" (makefile-name makefile)) 
                          :use '(:cl :cl-kb :mlcl ))))
  (if (null (makefile-kb makefile))
      (setf (slot-value makefile 'kb) 
            (or (cl-kb:find-kb (makefile-pprj-file makefile) nil)
                (cl-kb:make-kb (makefile-pprj-file makefile)
                                 :use '(cl-kbs::|algorithm|)))))
  (makefile-load makefile))

(defun makefile-name (makefile)
  (format nil "~A-~A" 
          (pathname-name (makefile-pprj-file makefile))
          (schema-name (makefile-dataset-schema makefile))))

(defun makefile-source-list-file (makefile)
  (merge-pathnames
   (make-pathname :type "lisp")
   (makefile-pprj-file makefile)))

(defun makefile-compiled-list-file (makefile)
  (merge-pathnames
   (make-pathname :type nil)
   (makefile-source-list-file makefile)))

(defun makefile-xml-kb-file (makefile)
  (merge-pathnames
   (make-pathname :type "xml")
   (makefile-pprj-file makefile)))

(defun makefile-load (makefile)
  (let ((lispfile (makefile-source-list-file makefile)))
    (if (or (not (probe-file lispfile)) (< (file-write-date lispfile) (file-write-date (makefile-xml-kb-file makefile))))
        (progn
          (cl-kb:kb-open (makefile-kb makefile))
          (with-open-file (strm (makefile-source-list-file makefile) :direction :output :if-exists :supersede)
                          (makefile-compile (makefile-package makefile) (makefile-kb makefile) strm))
          (cl-kb:kb-close (makefile-kb makefile))
          (compile-file lispfile)
          (load (makefile-compiled-list-file makefile))
          (funcall (find-symbol "INIT-MAKEFILE" (makefile-package makefile))))
        (progn
          (load (makefile-compiled-list-file makefile))))))

;
;
;

(defun makefile-compile (package kb strm)
  (cl-kb:kb-open kb)
  (let ((compinfo (make-algocomp-info)))
    (makefile-compile-header package kb strm)
     (let ((algo-list nil))
       (dolist (el (cl-kb:kb-interned-elements kb))
         (if (and (typep el 'cl-kb:simple-instance) 
                  (cl-kb:instance-has-type el '|algorithm|::|Algorithm|))
             (push el algo-list)))
       (dolist (algo algo-list)
         (makefile-compile-algorithm package algo compinfo strm)))
    (makefile-compile-trailer package strm))
  (cl-kb:kb-close kb))

(defstruct algocomp-info
  (symbols nil))

;
; compile header/trailer
;

(defun makefile-compile-header (package kb strm)
  (declare (ignore kb))
  (format strm ";;;; Created on ~A~%~%" (get-universal-time))
  (format strm "(in-package \"~A\")~%~%" (package-name package))
  (format strm "~%~%"))

(defun makefile-compile-trailer (package strm)
  (format strm "(defun init-makefile () ")
  (format strm "nil) ~%~%")
  (format strm "(format t \"!! loaded ~A !!~A\")" (package-name package) "~%~%")
  (format strm "~%~%")
  (format strm ";;;; Created on ~A~%" (get-universal-time)))

;
;
;

(defun makefile-compile-algorithm (package algo compinfo strm)
  (declare (ignore package)
           (ignore compinfo))
  (let* ((compiler-frame 
          (cl-kb:frame-own-slot-value algo '|algorithm|::|algorithm_compiler|))
         (class-name
          (cl-kb:frame-own-slot-value compiler-frame '|algorithm|::|algorithm_compiler_class|))
         (class-package
          (cl-kb:frame-own-slot-value compiler-frame '|algorithm|::|algorithm_compiler_package|))
         (class-asdf
          (cl-kb:frame-own-slot-value compiler-frame '|algorithm|::|algorithm_compiler_asdf_package|)))
    (format strm ";;;; Created algorithm ~A ~%~%" (cl-kb:frame-name algo))
    (format strm "(asdf:operate 'asdf:load-op '~A)~%~%" class-asdf)
    (asdf:operate 'asdf:load-op (make-symbol class-asdf))
    (let ((symb (find-symbol class-name (find-package class-package))))
      (let ((algorithm-compiler
             (make-instance symb)))
        (algorithm-compiler-compile algorithm-compiler algo)))))

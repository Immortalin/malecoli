;;;; Created on 2008-09-17 15:45:10

(in-package :mlcl-algorithm)

(defclass algorithm ()
  ((file 
    :READER algorithm-pprj-file
    :INITARG :file
    :TYPE pathname)
   (package
    :READER algorithm-package
    :INITARG :package
    :TYPE package)
   (kb
    :TYPE kb
    :INITARG :kb
    :INITFORM nil
    :READER algorithm-kb)))

(defmethod initialize-instance :after ((algorithm algorithm) &rest initargs)
  (declare (ignore initargs))
  (setf (slot-value algorithm 'package) 
        (or (find-package (format nil "~A-algo" (algorithm-name algorithm))) 
            (make-package (format nil "~A-algo" (algorithm-name algorithm)) 
                          :use '(:cl :mlcl-kb :mlcl-dataset))))
  (if (null (algorithm-kb algorithm))
      (setf (slot-value algorithm 'kb) 
            (or (mlcl-kb:find-kb (algorithm-name algorithm) nil)
                (mlcl-kb:make-kb (algorithm-pprj-file algorithm)
                                 :use '(mlcl-kbs::|algorithm|)))))
  (dolist (ukb (mlcl-kb:kb-use-list (algorithm-kb algorithm)))
    (if (not (member ukb (list (mlcl-kb:find-kb 'mlcl-kbs::|algorithm|) (mlcl-kb:find-kb 'mlcl-kbs::|protege|))))
        (let ((ns (make-instance 'algorithm :file (mlcl-kb:kb-protege-pprj-file ukb) :kb ukb)))
          (use-package (algorithm-package ns) (algorithm-package algorithm))))) 
  (algorithm-load algorithm))

(defun algorithm-name (algorithm)
  (pathname-name (algorithm-pprj-file algorithm)))

(defun algorithm-source-list-file (algorithm)
  (merge-pathnames
   (make-pathname :type "lisp")
   (algorithm-pprj-file algorithm)))

(defun algorithm-compiled-list-file (algorithm)
  (merge-pathnames
   (make-pathname :type nil)
   (algorithm-source-list-file algorithm)))

(defun algorithm-xml-kb-file (algorithm)
  (merge-pathnames
   (make-pathname :type "xml")
   (algorithm-pprj-file algorithm)))

(defun algorithm-load (algorithm)
  (let ((lispfile (algorithm-source-list-file algorithm)))
    (if (or (not (probe-file lispfile)) (< (file-write-date lispfile) (file-write-date (algorithm-xml-kb-file algorithm))))
        (progn
          (mlcl-kb:kb-open (algorithm-kb algorithm))
          (with-open-file (strm (algorithm-source-list-file algorithm) :direction :output :if-exists :supersede)
                          (algorithm-compile (algorithm-package algorithm) (algorithm-kb algorithm) strm))
          (mlcl-kb:kb-close (algorithm-kb algorithm))
          (compile-file lispfile)
          (load (algorithm-compiled-list-file algorithm))
          (funcall (find-symbol "INIT-ALGORITHM" (algorithm-package algorithm))))
        (progn
          (load (algorithm-compiled-list-file algorithm))))))



;;;; Created on 2008-04-23 15:28:45

(in-package :mlcl-kb)


;
; A generic element of a kb
;

(defclass kb-element ()
  ((name
    :TYPE string
    :READER kb-element-name
    :INITARG :name
    :documentation "the name of the element")
   (definedp
    :ACCESSOR kb-element-definedp
    :INITARG :definedp
    :INITFORM nil
    :documentation "true iff the elemente is completely defined"))
  (:documentation "A generic element of a kb."))


;
; global variables
;

(defvar *kb* nil 
  "default kb")

(defvar *all-kbs* nil
  "list of all kbs")


;
; knowledge base
;

(defclass kb ()
  ((name
    :READER kb-name
    :INITARG :name
    :TYPE string)
   (package
    :READER kb-package
    :INITARG :package
    :TYPE package)
   (interned-elements
    :READER kb-interned-elements
    :INITFORM nil)
   (use-list
    :READER kb-use-list
    :INITARG :use-list
    :INITFORM nil)
   (protege-file
    :TYPE (or nil pathname)
    :INITARG :protege-file
    :ACCESSOR kb-protege-file)
   (loadedp
    :INITFORM nil
    :ACCESSOR kb-loadedp)
   (openedp
    :INITFORM nil
    :ACCESSOR kb-openedp))
  (:documentation "A kb"))

; kb designator

(deftype kb-designator ()
  `(or string kb symbol))

(defun find-kb (kb-des &optional (errorp nil))
  (check-type kb-des kb-designator)
  "Return kb called NAME. If there is no such kb NIL is returned
if ERRORP is false, otherwise an error is signalled."
  (etypecase kb-des
             (kb kb-des)
             (symbol
              (if (and (boundp kb-des) (typep (symbol-value kb-des) 'kb))
                  (symbol-value kb-des)
                  (if errorp (error "Kb named ~S does not exist." kb-des) nil)))
             (string 
              (or (find-if #'(lambda (x) (string= (kb-name x) kb-des)) *all-kbs*)
                  (if errorp (error "Kb named ~S does not exist." kb-des) nil)))))

; make and delete

(defmethod initialize-instance :after ((kb kb) &rest initargs)
  (declare (ignore initargs))
  (if (find-kb (kb-name kb))
      (error "Kb named ~s already exists." (kb-name kb)))
  (setf (slot-value kb 'package) (or (find-package (kb-name kb)) (make-package (kb-name kb) :use nil)))
  (let ((ul (kb-use-list kb)))
    (setf (slot-value kb 'use-list) nil)
    (dolist (u ul) (use-kb u kb)))
  (let ((kbsym (intern (kb-name kb) (find-package :mlcl-kbs))))
    (setf (symbol-value kbsym) kb))
  (push kb *all-kbs*))
  
(defun make-kb (name &key (use-list nil) (protege-file nil))
  "make a new kb"
  (check-type name string)
  (if (find-kb name)
      (error "Kb named ~s already exists." name))
  (make-instance 'kb :name name :protege-file protege-file :use-list use-list))

(defun delete-kb (kb-des)
  (check-type kb-des kb-designator)
  (let ((kb (find-kb kb-des)))
    (setf *all-kbs* (delete kb *all-kbs*))
    (delete-package (slot-value kb 'package))
    (let ((kbsym (find-symbol (kb-name kb) (find-package :mlcl-kbs))))
      (setf (symbol-value kbsym) nil)
      (unintern kbsym (find-package :mlcl-kbs)))))

(defun kb-clear (kb-des)
  (check-type kb-des kb-designator)
  (let ((kb (find-kb kb-des)))
    (dolist (el (kb-interned-elements kb))
      (let ((it (kb-find-element-symbol (kb-element-name el) kb)))
        (if it
            (progn
              (setf (symbol-value it) nil)
              (unintern it (slot-value kb 'package))))))
    (setf (slot-value kb 'interned-elements) nil)))

; using

(defun use-kb (kb-to-use-des &optional (kb-des *kb*))
  (check-type kb-des kb-designator)
  (let ((kb (find-kb kb-des))
        (kb-to-use (find-kb kb-to-use-des)))
    (push kb-to-use (slot-value kb 'use-list))
    (use-package (kb-package kb-to-use) (kb-package kb))))

(defun unuse-kb (kb-to-use-des &optional (kb-des *kb*))
  (check-type kb-des kb-designator)
  (let ((kb (find-kb kb-des))
        (kb-to-use (find-kb kb-to-use-des)))
    (setf (slot-value kb 'use-list) (delete kb-to-use (slot-value kb 'use-list)))
    (unuse-package (kb-package kb-to-use) (kb-package kb))))

; kb and interned elements

(defun kb-find-element-symbol (name &optional (kb-des *kb*))
  (check-type kb-des kb-designator)
  (check-type name string)
  (let* ((kb (find-kb kb-des)))
    (find-symbol name (kb-package kb))))

(defun kb-find-element (name &optional (kb-des *kb*))
  (check-type kb-des kb-designator)
  (check-type name string)
  (let* ((kb (find-kb kb-des))
         (it (kb-find-element-symbol name kb)))
    (if (boundp it)
        (symbol-value it)
        nil)))

(defun kb-intern (el &optional (kb-des *kb*))
  (check-type kb-des kb-designator)
  (check-type el kb-element)
  (let* ((kb (find-kb kb-des))
         (it (kb-find-element-symbol (kb-element-name el) kb)))
    (if it
        (progn
          (setf (slot-value kb 'interned-elements) (delete el (slot-value kb 'interned-elements))))
        (progn
          (setf it (intern (kb-element-name el) (kb-package kb)))))
    (setf (symbol-value it) el)
    (push (symbol-value it) (slot-value kb 'interned-elements))
    (symbol-value it)))

(defun kb-unintern (el &optional (kb-des *kb*))
  (check-type kb-des kb-designator)
  (check-type el kb-element)
  (let* ((kb (find-kb kb-des))
         (it (kb-find-element-symbol (kb-element-name el) kb)))
    (setf (symbol-value it) nil)
    (unintern it (kb-package kb))
    (setf (slot-value kb 'interned-elements) (delete el (slot-value kb 'interned-elements)))))


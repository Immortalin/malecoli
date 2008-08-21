

(defclass technical-object ()
  ((price
    :TYPE (or float nil)
    :INITFORM nil
    :INITARG price
    :ACCESSOR technical-object-price)))

(defclass pc (technical-object)
  ((floppy-disk
    :TYPE (or floppy-disk  nil)
    :INITFORM nil
    :INITARG floppy-disk
    :ACCESSOR pc-floppy-disk)))

(defclass storage-device (technical-object)
  ())

(defclass magnetic-storage-device (storage-device)
  ())

(defclass floppy-disk (magnetic-storage-device)
  ())


(defun test-get-pc01 ()
  (let ((pc (make-instance 'pc))
	(floppy (make-instance 'floppy-disk)))
    (setf (pc-floppy-disk pc) floppy)
    (setf (technical-object-price pc) 120.00)
    (setf (technical-object-price floppy) 12.0)
    pc))

(defun test-get-pc02 ()
  (let ((pc (make-instance 'pc))
	(floppy (make-instance 'floppy-disk)))
    (setf (pc-floppy-disk pc) floppy)
    (setf (technical-object-price pc) 110.00)
    (setf (technical-object-price floppy) 7.0)
    pc))

(defun test-get-pc03 ()
  (let ((pc (make-instance 'pc))
	(floppy (make-instance 'floppy-disk)))
    (setf (pc-floppy-disk pc) floppy)
    (setf (technical-object-price pc) 130.00)
    (setf (technical-object-price floppy) 10.0)
    pc))

(defun test-get-pc04 ()
  (let ((pc (make-instance 'pc))
	(floppy (make-instance 'floppy-disk)))
    (setf (pc-floppy-disk pc) floppy)
    (setf (technical-object-price pc) 110.00)
    (setf (technical-object-price floppy) 40.0)
    pc))

(defmethod similarity ((x pc) (q pc))
  (let ((s0 (similarity (pc-floppy-disk x) (pc-floppy-disk q)))
	(s1 (similarity (technical-object-price x)
			(technical-object-price q))))
    (+ s0 s1)))

(defmethod similarity ((x floppy-disk) (q floppy-disk))
  (let ((s1 (similarity (technical-object-price x)
			(technical-object-price q))))
    s1))
  

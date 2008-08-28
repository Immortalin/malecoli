;;;; Created on 2008-08-28 12:09:50

(in-package :mlcl-kb)

;
; protege load/save
;

(defun kb-load (&optional (kb *kb*))
  (if (and (kb-protege-file kb) (not (kb-loadedp kb)))
      (progn
        (kb-import-from-protege-xml (kb-protege-file kb) kb)
        (setf (kb-loadedp kb) t))))

(defun kb-save (&optional (kb *kb*))
  (if (kb-protege-file kb)
      (progn
        (kb-export-to-protege-xml (kb-protege-file kb) kb)
        (save-new-pprj kb))))

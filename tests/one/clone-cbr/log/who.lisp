;;;; Created on 2009-02-05 12:13:32


(in-package :clone-cbr)

(defmethod cl-who:convert-tag-to-string-list ((tag (eql :ele)) args-l body body-fn)
  (nconc (cons "<"
               (cons (car body)
                     (cons ">"
                           (funcall body-fn (cdr body)))))
         (list "</"
               (car body)
               ">")))



(defpackage #:cbr
  (:use :cl))
(in-package #:cbr)
(defgeneric similarity (cas query))
(defmethod similarity (cas query)
  (declare (ignore cas query))
  0.0)


(defpackage #:cbr-user
  (:use #:cbr :cl))
(in-package #:cbr-user)
(defgeneric similarity (cas query))
(defmethod similarity (cas query)
  (cbr::similarity cas query))


(defpackage #:myA
    (:use #:cbr-user :cl))
(in-package :myA)
(defgeneric similarity (cas query))
(defmethod similarity (cas query)
  (cbr-user::similarity cas query))



(defpackage #:myB
    (:use #:cbr-user :cl))
(in-package #:myB)
(defgeneric similarity (cas query))
(defmethod similarity (cas query)
  (cbr-user::similarity cas query))

(in-package :cl-user)

;;;; Created on 2008-09-03 17:34:24

(in-package :mlcl-dataset)

(clsql:def-view-class |DatasetThing| ()
                      ((name
                        :db-kind :key
                        :db-constraints :not-null
                        :reader |DatasetThing-NAME|
                        :initarg :name
                        :type string))
                      (:base-table |DatasetThing|))

(clsql:def-view-class |DatasetCase| (|DatasetThing|)
                      ()
                      (:base-table |DatasetCase|))

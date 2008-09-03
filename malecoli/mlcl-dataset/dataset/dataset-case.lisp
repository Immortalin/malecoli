;;;; Created on 2008-09-03 17:34:24

(in-package :mlcl-dataset)

(clsql:def-view-class |DatasetThing| ()
                      ())
(clsql:def-view-class |DatasetCase| (|DatasetThing|)
                      ((case-id
                        :db-kind :key
                        :db-constraints :not-null
                        :type integer)))

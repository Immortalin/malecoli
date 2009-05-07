;;;; Created on 2009-02-09 10:58:28

(in-package :clone-kb)

(defun state-import (pathname)
  (let ((state nil))
    (if (probe-file pathname)
        (with-open-file (strm pathname :direction :input)
                        (setf state (read strm))))
    state))


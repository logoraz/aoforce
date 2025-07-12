(defpackage :aoforce
  (:use :cl
        :utils/base)
  (:export #:simple-test
           #:test-utils)
  (:documentation "Main package of AOFORCE"))

(in-package :aoforce)


(defun simple-test (&optional (n 11))
  "Simple function for testing."
  (loop :for i :from 0 :below n
        :collect (list (format nil "list ~A" i)
                       (/ i n))))

(defun test-utils ()
  (concat "string-1" " " "string-2"))

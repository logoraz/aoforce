(defpackage #:aoforce/core/aoforce
  (:use #:cl
        #:aoforce/core/utils/base)
  (:export #:simple-test
           #:test-utils)
  (:documentation "Main package of Confer."))
(in-package #:aoforce/core/aoforce)


(defun simple-test (&optional (n 11))
  "Simple function for testing."
  (loop :for i :from 0 :below n
        :collect (list (format nil "list ~A" i)
                       (/ i n))))

(defun test-utils ()
  (concat "string-1" " " "string-2"))

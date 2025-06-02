(defpackage #:aoforce/libraries/chembook/chembook
  (:use #:cl)
  (:export #:chem-db))
(in-package #:aoforce/libraries/chembook/chembook)

(defun chem-db (&optional (n 11))
  "Simple 'Mock' database for testing."
  (loop :for i :from 0 :below n
        :collect (list i
                       (format nil "Raw material ~A" i)
                       (/ i n))))


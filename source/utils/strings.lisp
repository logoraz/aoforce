(defpackage :utils/strings
  (:use :cl :uiop)
  #+or
  (:import-from :cl-ppcre)
  (:export #:concat)
  (:documentation "String utilities."))
(in-package :utils/strings)

;; String manipulation
(defun concat (&rest strings)
  "Shorthand for CONCATENATE specialized for strings."
  (apply #'concatenate 'string strings))



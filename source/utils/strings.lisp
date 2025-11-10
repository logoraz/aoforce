(defpackage :utils/strings
  (:use :cl :uiop)
  (:import-from :cl-ppcre)
  (:export #:concat)
  (:documentation "String utilities."))
(in-package :utils/strings)


;;;==============================================================================
;;; String Manipulation
;;;==============================================================================
(defun concat (&rest strings)
  "Shorthand for CONCATENATE specialized for strings."
  (apply #'concatenate 'string strings))



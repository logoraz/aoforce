(defpackage #:core/database
  (:use #:cl)
  (:import-from #:bt2)
  (:export #:config-file)
  (:documentation "An in-memory database -> basic Lisp data structures."))

(in-package #:core/database)

;;; =============================================================================
;;; Database
;;; =============================================================================
;;
;;
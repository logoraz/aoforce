(defpackage #:core/database
  (:use #:cl)
  (:import-from #:bt2)
  (:documentation "An in-memory database -> basic Lisp data structures.")  
  (:export #:config-file))

(in-package #:core/database)

;;; =============================================================================
;;; Database
;;; =============================================================================
;;
;;
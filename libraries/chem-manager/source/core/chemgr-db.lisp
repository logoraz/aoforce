(defpackage #:core/chemgr-db
  (:nicknames #:cmgrdb)
  (:use #:cl)
  (:import-from #:bt2)
  (:local-nicknames (#:db #:dbi))
  (:export )
  (:documentation "A simple database for SQLITE."))

(in-package #:core/chemgr-db)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Database Configuration
;;;

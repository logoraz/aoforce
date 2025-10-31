(defpackage :core/database
  (:nicknames :db)
  (:use :cl)
  (:import-from :bt2)
  (:export )
  (:documentation "An in-memory database -> basic Lisp data structures."))
(in-package :core/database)


;;; =============================================================================
;;; In-Memory Database Ex
;;; =============================================================================
(defstruct surfactant
  "A surfactant data structure."
  (id (gensym "SURF%") :type symbol :read-only t)
  (name "" :type string)
  (hlb nil :type (or null integer))
  (inci "" :type string)
  (class :anionic :type (member :anionic :cationic :non-ionic :amphoteric)))


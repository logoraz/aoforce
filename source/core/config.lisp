(defpackage :core/config
  (:use :cl)
  (:import-from :bt2)
  (:export #:config-object
           #:*config-objects*)
  (:documentation "An in-memory database -> basic Lisp data structures."))
(in-package :core/config)


;;; =============================================================================
;;; Data Types
;;; =============================================================================
(defstruct config-object
  "One entry in the config file list.

   :name        - name of file
   :path        – absolute or relative path (string)
   :type        – :lisp, :shell, :text, :binary, :directory, :symlink
   :required    – T if the file must exist at start-up
   :writable    – T if the program will write to it
   :vc          – T if the file is in version-controlled
   :description – free-form comment"
  (path        "" :type string)
  (type        :lisp :type (member :lisp :shell :text :binary :directory :symlink))
  (required    nil :type boolean)
  (writable    nil :type boolean)
  (vc          nil :type (or null string))
  (description nil :type (or null string)))

(defparameter *config-objects* ()
  "List of CONFIG-OBJECT structures – the whole file-section of the app.")

;;; =============================================================================
;;; Helpers
;;; =============================================================================


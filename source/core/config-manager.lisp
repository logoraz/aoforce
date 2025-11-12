(defpackage :core/config-manager
  (:use :cl :utils/syntax)
  (:import-from :uiop
                #:run-program
                #:copy-file
                #:ensure-pathname
                #:ensure-directory-pathname)
  (:import-from :uiop/filesystem
                #:ensure-directories-exist
                #:directory-exists-p)
  (:import-from :uiop/pathname
                #:directory-namestring)
  (:import-from :osicat
                #:make-link
                #:file-kind)
  (:export #:config-object
           #:config-manager
           #:print-object
           ;; Manager methods
           #:add-config
           #:list-configs
           #:deploy-configs
           #:clear-configs
           #:configs
           ;; Utilities
           #:expand-pathname
           #:symlinkp
           #:create-symlink)
  (:documentation "CLOS-based Configuration Manager"))

(in-package :core/config-manager)

;;; =============================================================================
;;; Config Object & Config Manager (Classes)
;;; =============================================================================
(defclass config-object ()
  ((name    :initarg :name :reader config-name :type string
            :documentation "Name of the config (e.g., 'emacs')")
   (source  :initarg :source :reader config-source  :type pathname
            :documentation "Source path")
   (place   :initarg :place :reader config-place :type pathname
            :documentation "Destination path")
   (spec    :initarg :spec :reader config-spec :initform :symlink
            :type (member :copy :symlink)
            :documentation "Transfer method")
   (type    :initarg :type :reader config-type :initform :file
            :type (member :file :directory)
            :documentation "File or directory"))
  (:documentation "Represents a single configuration entry."))

(defclass config-manager ()
  ((configs :initform () :accessor configs
            :documentation "List of config-object instances"))
  (:documentation "Manages a collection of config-object entries."))

;;; =============================================================================
;;; Interface (Generic Functions)
;;; =============================================================================
(defgeneric add-config (manager name source place &key spec type)
  (:documentation "Add a config entry to the manager."))

(defgeneric list-configs (manager &optional stream)
  (:documentation "Print the deployment plan."))

(defgeneric deploy-configs (manager &optional verbose)
  (:documentation "Deploy all configurations."))

(defgeneric clear-configs (manager)
  (:documentation "Remove all config entries."))

;;; =============================================================================
;;; Core Behavior (Methods)
;;; =============================================================================
(defmethod print-object ((config config-object) stream)
  "Pretty-print a CONFIG-OBJECT in #<> form for REPL and debugging.

See: PRINT-UNREADABLE-OBJECT for default behavior.
Example:
  #<CONFIG-OBJECT sbclrc [FILE → SYMLINK] ~/dotfiles/.sbclrc → ~/.sbclrc>"
  (print-unreadable-object (config stream :type t)
    (format stream "~A [~A → ~A] ~A → ~A"
            (config-name config)
            (string-upcase (symbol-name (config-type config)))
            (string-upcase (symbol-name (config-spec config)))
            (config-source config)
            (config-place config))))

(defmethod add-config ((manager config-manager) name source place
                       &key (spec :symlink) (type :file))
  "Add a config entry with path expansion."
  (push (make-instance 'config-object
                       :name   name
                       :source (expand-pathname source)
                       :place  (expand-pathname place)
                       :spec   spec
                       :type   type)
        (configs manager))
  manager)

(defmethod list-configs ((manager config-manager) &optional (stream t))
  "Pretty-print the deployment plan."
  (format stream "~%**** Config Deployment Plan ****~%")
  (loop :for obj :in (reverse (configs manager))
        :for i :from 1
        :do (format stream "~d. ~A~%" i obj))
  (values))

(defmethod deploy-configs ((manager config-manager) &optional (verbose t))
  "Deploy all configurations in MANAGER: symlink or copy as specified.

Actions are performed in reverse order of addition (LIFO).

Returns a list of performed actions:
  ((:symlink \"/source\" \"/dest\") ...)
  ((:copy \"/source\" \"/dest\") ...)

If VERBOSE is true (default), prints a summary.

Example return:
  ((:symlink \"/home/user/dotfiles/.sbclrc\" \"/home/user/.sbclrc\")
   (:copy \"/home/user/dotfiles/.emacs.d\" \"/home/user/.emacs.d\"))"
  (let ((results '()))
    (dolist (obj (reverse (configs manager)))
      (let ((src (config-source obj))
            (dst (config-place obj))
            (spec (config-spec obj))
            (type (config-type obj)))
        (ensure-directories-exist (directory-namestring dst))
        (cond
          ((eq spec :symlink)
           (if (eq type :directory)
               (create-symlink src dst :dir t)
               (create-symlink src dst))
           (push `(:symlink ,src ,dst) results))
          ((eq spec :copy)
           (if (eq type :directory)
               (run-program `("cp" "-r" ,(namestring src) ,(namestring dst)))
               (copy-file src dst))
           (push `(:copy ,src ,dst) results)))))

    (let ((actions (nreverse results)))
      (when verbose
        (format t "Deployment complete. ~D action~:P performed.~%" (length actions)))
      actions)))

(defmethod clear-configs ((manager config-manager))
  "Clear all config entries."
  (setf (configs manager) ())
  manager)

;;; =============================================================================
;;; Deployment Engine Specifics
;;; =============================================================================
(defun expand-pathname (pathspec)
  "Expand ~ to user's home directory."
  (let* ((str (princ-to-string pathspec))
         (home (namestring (user-homedir-pathname)))
         (expanded
           (if (and (>= (length str) 2)
                    (string= (subseq str 0 2) "~/"))
               (concatenate 'string home (subseq str 1))
               str)))
    (ensure-pathname expanded)))

(defun create-symlink (src link &key (dir nil))
  "Create a symlink."
  (let ((target (if dir (ensure-directory-pathname src) src)))
    (make-link link :target target)))

;;; =============================================================================
;;; Utilities
;;; =============================================================================
(defun ensure-directory (pathspec &key (mode #o700))
  "Ensure directory exists."
  (ensure-directories-exist (ensure-directory-pathname pathspec) :mode mode))

(defun symlinkp (pathspec)
  "Test if path is a symlink."
  (eq (file-kind pathspec) :symbolic-link))


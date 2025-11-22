(defpackage #:core/config-manager
  (:use #:cl 
        #:utils/syntax
        #:trivial-gray-streams)
  (:import-from #:uiop
                #:run-program
                #:copy-file
                #:ensure-pathname
                #:ensure-directory-pathname)
  (:import-from #:uiop/filesystem
                #:ensure-directories-exist
                #:directory-exists-p)
  (:import-from #:uiop/pathname
                #:directory-namestring)
  (:import-from #:osicat
                #:make-link
                #:file-kind)
  (:import-from #:cl-ppcre
                #:regex-replace-all)
  (:export #:config-object
           #:config-manager
           #:colored-stream
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

(in-package #:core/config-manager)

;;; =============================================================================
;;; Classes
;;; =============================================================================
(defclass config-object ()
  ((name    :initarg :name :reader config-name :type string
            :documentation "Name of the config")
   (source  :initarg :source :reader config-source  :type pathname
            :documentation "Source path")
   (place   :initarg :place :reader config-place :type pathname
            :documentation "Destination path")
   (spec    :initarg :spec :reader config-spec :initform :symlink
            :type (member :copy :symlink)
            :documentation "Transfer method")
   (type    :initarg :type :reader config-type :initform :file
            :type (member :file :directory)
            :documentation "Source type: File or directory"))
  (:documentation "Represents a single configuration entry."))

(defclass config-manager ()
  ((configs :initform () :accessor configs
            :documentation "List of config-object instances"))
  (:documentation "Manages a collection of config-object entries."))

(defclass colored-stream (trivial-gray-streams:fundamental-character-output-stream)
  ((target :initarg :target :reader target)
   (use-colors :initarg :use-colors :initform t :accessor use-colors-p))
  (:documentation "A Gray stream that adds ANSI colors to output."))

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
;;; Methods (Core Behavior)
;;; =============================================================================
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
  (format stream "~A~A**** Config Deployment Outline ****~A~%~%"
          (color :bold) (color :green) (color :reset))
  (loop :for obj :in (reverse (configs manager))
        :for i :from 1
        :do (format stream "~A~D. ~A~A ~A~%"
                   (color :green) i
                   (color :grey) obj
                   (color :reset)))
  (values))

(defmethod deploy-configs ((manager config-manager) &optional (verbose t))
  "Deploy all configurations in MANAGER: symlink or copy as specified.

Actions are performed in reverse order of addition (LIFO).
If VERBOSE is true (default), prints a live summary with colors.

Returns a list of performed actions:
  ((:symlink \"/source\" \"/dest\") ...)
  ((:copy \"/source\" \"/dest\") ...)"
  (let ((results '())
        (count 0)
        (total (length (configs manager))))
    (when verbose
      (format verbose "~%~A~ADeploying Configuration ...~A~%~%"
              (color :bold) (color :green) (color :reset)))
    (dolist (obj (reverse (configs manager)))
      (incf count)
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
           (push `(:copy ,src ,dst) results)))
        
        (when verbose
          (format verbose "~A[~A~D/~D~A] ~A~A ~A~A ~A~A ~A~%"
                  (color :grey) (color :green) count total (color :grey)
                  (if (eq type :file) (color :yellow)
                      (color :red))
                  src
                  (color :reset) (color :arrow)
                  (if (eq spec :symlink) (color :cyan)
                      (color :yellow))
                  dst
                  (color :reset)))))
    
    (let ((actions (nreverse results)))
      (when verbose
        (format verbose "~%~A~ADeployment complete: ~A~D action~:P~A performed.~A~%"
                (color :bold) (color :green)
                (color :cyan) (length actions) (color :green)
                (color :reset)))
      (format verbose "~%~A~A~A~%" (color :grey) actions (color :reset))))
  (values))

(defmethod clear-configs ((manager config-manager))
  "Clear all config entries."
  (setf (configs manager) ())
  manager)

(defmethod print-object ((config config-object) stream)
  "Colorized pretty-print a CONFIG-OBJECT in #<> form for REPL and debugging.

Example:
  #<CONFIG-OBJECT sbclrc [FILE → SYMLINK] ~/dotfiles/.sbclrc → ~/.sbclrc>"
  (print-unreadable-object (config stream :type t)
    (format stream "~A~A ~A[~A~A ~A~A ~A~A~A] ~A~A ~A~A ~A~A ~A"
            (color :magenta) (config-name config)
            (color :reset)
            (color :yellow) (string-upcase (symbol-name (config-type config)))
            (color :reset) (color :arrow)
            (color :cyan) (string-upcase (symbol-name (config-spec config))) (color :reset)
            (color :yellow) (config-source config)
            (color :reset) (color :arrow)
            (color :cyan) (config-place config)
            (color :grey))))

;;; =============================================================================
;;; Stream Methods
;;; =============================================================================
(defmethod trivial-gray-streams:stream-write-string
    ((stream colored-stream) string &optional start end)
  "Write STRING to target, stripping ANSI codes if colors disabled."  
  (with-slots (target use-colors) stream
    (write-string (if use-colors string (strip-ansi string))
                  target :start (or start 0) :end end)))

;; Optional: if you want to support ~[color] later
(defmethod trivial-gray-streams:stream-write-char ((stream colored-stream) char)
  "Fallback: write single character directly."  
  (write-char char (target stream)))

;;; =============================================================================
;;; Helper Functions/Utilities (Deployment Engine Specifics & Colors)
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

(defun symlinkp (pathspec)
  "Test if path is a symlink."
  (eq (file-kind pathspec) :symbolic-link))

(defun create-symlink (src link &key (dir nil))
  "Create a symlink."
  (let ((target (if dir (ensure-directory-pathname src) src)))
    (make-link link :target target)))

(defparameter *color-codes*
  '((:reset   . "[0m")
    (:bold    . "[1m")
    (:red     . "[31m")
    (:green   . "[32m")
    (:yellow  . "[33m")
    (:blue    . "[34m")
    (:magenta . "[35m")
    (:cyan    . "[36m")
    (:grey    . "[90m")
    (:arrow   . " → "))
  "Direct keyword → ANSI escape code")

(defparameter *use-unicode-arrows* t
  "Use Unicode arrows (→) when T and terminal supports it.")

(defun color (&rest commands)
  "Return concatenated ANSI codes for COMMANDS."
  (apply #'concatenate 'string
         (mapcar (lambda (c) (or (cdr (assoc c *color-codes*)) "")) commands)))

;; Optional: Unicode arrow fallback
(defun arrow ()
  (if *use-unicode-arrows* (color :arrow) " to "))

;; Optional: strip ANSI codes when disabled
(defun strip-ansi (s)
  (regex-replace-all "\\[[0-9;]*m" s ""))

;;; =============================================================================
;;; Other
;;; =============================================================================
(defun ensure-directory (pathspec &key (mode #o700))
  "Ensure directory exists."
  (ensure-directories-exist (ensure-directory-pathname pathspec) :mode mode))


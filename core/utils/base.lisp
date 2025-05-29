(defpackage #:confer/core/utils/base
  (:use #:cl
        #:uiop)
  (:import-from #:cl-interpol)
  (:import-from #:osicat
                #:make-link
                #:file-kind)
  (:export #:*shell-program*
           #:concat
           #:run-prog
           #:run-prog-collect-output
           #:getuid
           #:dir-pathname
           #:ensure-dir
           #:symlinkp
           #:create-symlink)
  (:documentation "Base utilities."))
(in-package #:confer/core/utils/base)

;; String manipulation
(defun concat (&rest strings)
  "Shorthand for CONCATENATE specialized for strings."
  (apply #'concatenate 'string strings))


;; Environment
(defun getuid ()
  (sb-posix:getuid))

;; Pathnames
(defun dir-pathname (pathspec)
  "Converts the non-wild pathname designator PATHSPEC to directory form."
  (uiop:ensure-directory-pathname pathspec))


;; directory creation/manipulation
;; Create directories (if they do no already exist)
(defun ensure-dir (pathspec &key (mode #o700))
  "Ensure directory in PATHSPEC exists"
  (let ((dir (dir-pathname pathspec)))
    (ensure-directories-exist dir :mode mode)))


;; Symlinks
(defun symlinkp (pathspec)
  "Test whether PATHSPEC is a symlink."
  (eq (osicat:file-kind pathspec) :symbolic-link))

(defun create-symlink (src link &key (dir nil))
  "Create a symlink for SRC to LINK."
  (let ((src-dir (dir-pathname src)))
    (if dir (osicat:make-link link :target src-dir)
        (osicat:make-link link :target src))))


;; Shell utilities (from StumpWM)
(defvar *shell-program* "/bin/sh"
  "The shell program used by @code{run-shell-command}.")

(defun run-prog (prog &rest opts &key args output (wait t) &allow-other-keys)
  "Common interface to shell. Does not return anything useful."
  (remf opts :args)
  (remf opts :output)
  (remf opts :wait)
  (let ((env (sb-ext:posix-environ)))
    (apply #'sb-ext:run-program prog args :output (if output output t)
           :error t :wait wait :environment env opts)))

(defun run-prog-collect-output (prog &rest args)
  "run a command and read its output."
  (with-output-to-string (s)
    (run-prog prog :args args :output s :wait t)))

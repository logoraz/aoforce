(defpackage #:confer/libraries/utils/base
  (:use #:cl
        #:uiop)
  (:import-from #:cl-interpol)
  #+(or)
  (:import-from #:osicat
                #:make-link)
  (:import-from #:sb-posix
                #:s-islnk
                #:stat-mode
                #:lstat
                #:symlink)
  (:export #:*shell-program*
           #:concat
           #:run-prog
           #:run-prog-collect-output
           #:getuid
           #:dir-pathname
           #:symlinkp
           #:create-symlink))
(in-package #:confer/libraries/utils/base)

;; String manipulation
(defun concat (&rest strings)
  "Shorthand for CONCATENATE specialized for strings."
  (apply #'concatenate 'string strings))


;; Symlinks
(defun dir-pathname (pathspec)
  "Converts the non-wild pathname designator PATHSPEC to directory form."
  (uiop:ensure-directory-pathname pathspec))

(defun getuid ()
  (sb-posix:getuid))

(defun symlinkp (pathspec)
  "Test whether PATHSPEC is a symlink."
  (sb-posix:s-islnk (sb-posix:stat-mode (sb-posix:lstat pathspec))))

(defun create-symlink (src lnk)
  "Create a symlink for SRC to LINK."
  (let ((src-dir (dir-pathname src))
        (lnk-dir (dir-pathname lnk)))
    (sb-posix:symlink src-dir lnk-dir)
    lnk-dir))

#+(or)
(defun create-symlink (src link)
  "Create a symlink for SRC to LINK."
  (let ((src-dir (dir-pathname src))
        (osicat:make-link link :target src :hard nil))))


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

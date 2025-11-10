(defpackage :utils/shell
  (:use :cl :uiop)
  (:export #:executable-find
           #:*shell-program*)
  (:documentation "Shell utilities."))
(in-package :utils/shell)


;;;==============================================================================
;;; General
;;;==============================================================================
(defun executable-find (program)
  "Simple function to return path to PROGRAM"
  (remove #\newline 
          (uiop:run-program (list "which" program)
                            :output :string)))

;; Shell utilities (from StumpWM)
(defvar *shell-program* "/bin/sh"
  "The shell program used by @code{run-shell-command}.")


;;;==============================================================================
;;; Reference
;;;==============================================================================
;;; source: stumpwm/wrappers.lisp
;; implementation specific --> sbcl
;; convert to ANSI Common Lisp (ccl & sbcl)
#+ (or)
(defun run-prog (prog &rest opts &key args output (wait t) &allow-other-keys)
  "Common interface to shell. Does not return anything useful."
  (remf opts :args)
  (remf opts :output)
  (remf opts :wait)
  (let ((env (sb-ext:posix-environ)))
    (apply #'sb-ext:run-program prog args :output (if output output t)
           :error t :wait wait :environment env opts)))
#+ (or)
(defun run-prog-collect-output (prog &rest args)
  "run a command and read its output."
  (with-output-to-string (s)
    (run-prog prog :args args :output s :wait t)))

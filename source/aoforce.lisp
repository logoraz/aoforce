(defpackage :aoforce
  (:nicknames :aofr)
  (:use :cl
        :gtk4
        :setup
        :frontends/aofr-adw)
  (:export #:simple-test
           #:main)
  (:documentation "Main package of AOFORCE"))

(in-package :aoforce)

;;; =============================================================================
;;; Tests
;;; =============================================================================
(defun simple-test (&optional (n 11))
  "Simple function for testing."
  (loop :for i :from 0 :below n
        :collect (list (format nil "list ~A" i)
                       (/ i n))))

;;; =============================================================================
;;; Entry Point
;;; =============================================================================
(defun main ()
  "Main entry point for the executable."
  (unless (adw:initialized-p)
    (adw:init))
  (simple-repl))

#+(or)
(defun main ()
  "Main entry point for the executable."
  (format t "Hello from Common Lisp! Arguments: ~A~%" 'no-args)
  #+or
  (progn
    #+clisp (ext:exit)
    #+(and ecl clasp) (ext:quit)
    #+ccl (ccl:quit)
    #+sbcl (sb-ext:quit))
  (uiop:quit))

(defpackage #:aoforce
  (:nicknames #:aofr)
  (:use #:cl
        #:setup
        #:ui/app)
  (:local-nicknames (#:it #:iterate))
  ;; Tests/Play
  (:export #:simple-test
           #:simple-test2)
  ;; UI
  (:export #:ui)
  (:documentation "Main package of AOFORCE"))

(in-package #:aoforce)

;;; =============================================================================
;;; Tests
;;; =============================================================================
(defun simple-test (&optional (n 11))
  "Simple function for testing."
  (loop :for i :from 0 :below n
        :collect (list (format nil "list ~A" i)
                       (/ i n))))

(defun simple-test2 (&optional (n 11))
  "Simple function for testing."
  (it:iter (it:for i from 0 below n)
           (it:collect (list (format nil "list ~A" i)
                             (/ i n)))))

;;; =============================================================================
;;; Entry Point
;;; =============================================================================
(defun ui ()
  "Main entry point for the executable."
  (start-app))

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

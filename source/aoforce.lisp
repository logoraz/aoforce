(defpackage :aoforce
  (:nicknames :aofr)
  (:use :cl :asdf :uiop)
  (:export #:simple-test
           #:test-utils
           #:main)
  (:documentation "Main package of AOFORCE"))
(in-package :aoforce)


(defun simple-test (&optional (n 11))
  "Simple function for testing."
  (loop :for i :from 0 :below n
        :collect (list (format nil "list ~A" i)
                       (/ i n))))

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

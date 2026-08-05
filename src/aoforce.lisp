(defpackage :aoforce
  (:nicknames #:aofr)
  (:use #:cl)
  (:import-from :aoforce/setup
                #:outline
                #:deploy)
  (:import-from :aoforce/ui/app
                #:aoforce-app)
  (:import-from :learn-cl/sdraw
                #:sdraw)
  (:import-from :learn-cl/dtrace
                #:dtrace)
  (:local-nicknames (#:it :iterate))
  ;; Tests/Play
  (:export #:sdraw
           #:dtrace
           #:simple-test
           #:simple-test2)
  ;; Setup
  (:export #:outline
           #:deploy)
  ;; Main Entry
  (:export #:main)
  (:documentation "Main package of AOFORCE"))

(in-package :aoforce)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Entry Point

(defun main ()
  "Main entry point for the executable."
  (aoforce-app))

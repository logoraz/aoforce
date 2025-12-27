(defpackage #:tests/suite
  (:use #:cl
        #:5am
        #:utils/strings
        #:utils/shell)
  (:export ) ;; TODO let's export when we are ready
  (:documentation "Base Test Suite"))
(in-package #:tests/suite)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Define the test suite
;;;

(def-suite :suite :description "confr test suite")
(in-suite :suite)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Let's first define the "easy" tests
;;;
#+nil
(test concat-test
  (is (string= "1 2" (concat "1 " "2"))))



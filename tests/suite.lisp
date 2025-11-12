(defpackage #:tests/suite
  (:use :cl :5am
        :utils/syntax
        :aoforce)
  (:export )
  (:documentation "Base Test Suite"))
(in-package #:tests/suite)

;;; =============================================================================
;;; Define the test suite
;;; =============================================================================
(def-suite :suite :description "AOFORCE test suite")
(in-suite :suite)

;;; =============================================================================
;;; Let's first define the "easy" tests
;;; =============================================================================
(test concat-test
  (is (string= "1 2" (concat "1 " "2"))))



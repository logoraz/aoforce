(defpackage :aoforce/libraries/learn-cl/fcalc
  (:nicknames :fcalc)
  (:use :cl)
  (:export #:make-calculator
           #:add!
           #:subtract!
           #:multiply!
           #:divide!
           #:clear!
           #:get-result)
  (:documentation "A functional calculator example in CL"))
(in-package :aoforce/libraries/learn-cl/fcalc)

;; See: https://gigamonkeys.com/book/functions

(defun make-calculator (current-value)
  "Create Calculator function 'object'"
  (lambda (operation)
    (setf current-value (funcall operation current-value))))

(defun add! (calculator x)
  "Add x to the value stored in calculator."
  (funcall calculator (lambda (value)
                        (+ value x))))

(defun subtract! (calculator x)
  "Subtract x from the value stored in calculator."
  (funcall calculator (lambda (value)
                        (- value x))))

(defun multiply! (calculator x)
  "Multiply x by the value stored in calculator."
  (funcall calculator (lambda (value)
                        (* value x))))

(defun divide! (calculator x)
  "Divide the value stored in calculator by x."
  (funcall calculator (lambda (value)
                        (/ value x))))

(defun clear! (calculator)
  "Clear calculator."
  (funcall calculator (lambda (value) 0)))


(defun get-result (calculator)
  "Get the value stored in calculator."
  (let ((result nil)) ;; nil used instead of () since result contains symbols
    (funcall calculator (lambda (value)
                          (setf result value)
                          value))
    result))

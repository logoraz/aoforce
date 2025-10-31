(defpackage :utils/syntax
  (:use :cl :uiop)
  (:export )
  (:documentation "Syntactic Language Extensions."))
(in-package :utils/syntax)

;; Equivalent to "named let" factorial
#+(or)
(labels ((fact (n acc)
           (if (zerop n)
               acc
               (fact (1- n) (* acc n)))))
  (fact 5 1))  ; => 120

(defmacro nlet (name bindings &body body)
  `(labels ((,name ,(mapcar #'car bindings) ,@body))
     (,name ,@(mapcar #'cadr bindings))))

#+nil
(nlet fact ((n 5) (acc 1))
      (if (zerop n)
          acc
          (fact (1- n) (* acc n))))  ; => 120

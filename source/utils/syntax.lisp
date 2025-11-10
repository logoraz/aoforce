(defpackage :utils/syntax
  (:use :cl :uiop)
  (:export )
  (:documentation "Syntactic Language Extensions."))
(in-package :utils/syntax)


;;;==============================================================================
;;; Examples
;;;==============================================================================
(defmacro nlet (name bindings &body body)
  `(labels ((,name ,(mapcar #'car bindings) ,@body))
     (,name ,@(mapcar #'cadr bindings))))

#+nil ;example
(nlet fact ((n 5) (acc 1))
      (if (zerop n)
          acc
          (fact (1- n) (* acc n))))  ; => 120

#+(or) ;Equivalent to "named let" factorial
(labels ((fact (n acc)
           (if (zerop n)
               acc
               (fact (1- n) (* acc n)))))
  (fact 5 1))  ; => 120


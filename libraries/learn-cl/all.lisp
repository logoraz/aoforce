(uiop:define-package :aoforce/libraries/learn-cl/all
  (:nicknames :lcl)
  (:use :cl)
  (:use-reexport
   #:aoforce/libraries/learn-cl/sdraw
   #:aoforce/libraries/learn-cl/dtrace
   #:aoforce/libraries/learn-cl/fcalc)
  (:documentation "Common Lisp Learning Tools/Suite"))
(in-package :aoforce/libraries/learn-cl/all)

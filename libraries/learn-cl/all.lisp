(uiop:define-package :aoforce/libraries/learn-cl/all
  (:nicknames :learn-cl :lcl)
  (:use :cl)
  (:use-reexport
   #:aoforce/libraries/learn-cl/sdraw
   #:aoforce/libraries/learn-cl/dtrace)
  (:documentation "Public interface for lcl"))
(in-package :aoforce/libraries/learn-cl/all)

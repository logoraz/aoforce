(uiop:define-package :aoforce/core/all
  (:nicknames :aoforce :aofrc)
  (:use :cl)
  (:use-reexport
   #:aoforce/core/utils/base
   #:aoforce/core/database
   #:aoforce/core/aoforce)
  (:documentation "Core interface of AOFORCE."))
(in-package :aoforce/core/all)

;;;; All general interfaces + common core of pure and stateful data structures
(uiop:define-package #:aoforce/core/all
  (:nicknames #:aoforce #:aofrc)
  (:use #:cl)
  (:use-reexport #:aoforce/core/utils/base
                 #:aoforce/core/aoforce))

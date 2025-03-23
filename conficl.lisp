;;;; Common Lisp Environment Setup
;;;
;;;
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;;body...
  ;;use ocicl to load external libraries if needed
  )

(defpackage #:conficl/setup
  (:use #:cl
        #:uiop
        #+sbcl
        #:sb-posix)
  (:import-from #:foo 
                #:bar 
                #:baz
                #:qux)
  (:export #:tbd))
(in-package #:conficl/setup)


(defconstant +cl-path+ (uiop:xdg-config-home #p"common-lisp"))


(defun symlinkp (pathname)
  (sb-posix:s-islnk (sb-posix:stat-mode (sb-posix:lstat pathname))))


(sb-ext:quit)
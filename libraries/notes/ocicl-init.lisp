;;;; OCICL Init/Setup
;;; This file generated via $ ocicl setup > ocicl-init.lisp
(defpackage #:aoforce/notes/ocicl-init
  (:use #:cl))
(in-package #:aoforce/notes/ocicl-init)

;; Preserving existing /home/loraz/.local/share/ocicl/ocicl-registry.cfg
;; Use setup's --force option to override.

;; Present the following code to your LISP system at startup, either
;; by adding it to your implementation's startup file
;; (~/.sbclrc, ~/.eclrc, ~/.abclrc, ~/.clinit.cl, or ~/.roswell/init.lisp)
;; or overriding it completely on the command line
;; (eg. sbcl --userinit init.lisp)

#-ocicl
(when (probe-file #P"/home/loraz/.local/share/ocicl/ocicl-runtime.lisp")
  (load #P"/home/loraz/.local/share/ocicl/ocicl-runtime.lisp"))
(asdf:initialize-source-registry
 (list :source-registry (list :directory (uiop:getcwd)) :inherit-configuration))

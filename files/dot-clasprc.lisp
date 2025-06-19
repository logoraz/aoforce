;;;; dot-clasprc.lisp -> .clasprc - Clasp Initialization File

(ignore-errors (require 'asdf)
               (require 'uiop))

;;; Enable OCICL
;; Preserving existing (uiop:xdg-data-home #P"ocicl/ocicl-registry.cfg")
;; Use setup's --force option to override.

;; Present the following code to your LISP system at startup, either
;; by adding it to your implementation's startup file
;; (~/.sbclrc, ~/.eclrc, ~/.clasprc, ~/.abclrc, ~/.clinit.cl, or ~/.roswell/init.lisp)
;; or overriding it completely on the command line
;; (eg. sbcl --userinit init.lisp)

#-ocicl
(progn
  (when (probe-file (uiop:xdg-data-home #P"ocicl/ocicl-runtime.lisp"))
    (load (uiop:xdg-data-home #P"ocicl/ocicl-runtime.lisp")))
  (asdf:initialize-source-registry
   (list :source-registry
         (list :directory (uiop:getcwd)) :inherit-configuration)))

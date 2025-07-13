;;;; dot-ccl-init.lisp -> .ccl-init.lisp - Clozure CL Initialization File

(ignore-errors (require 'asdf))

;;; Enable OCICL
;; Preserving existing (uiop:xdg-data-home #P"ocicl/ocicl-registry.cfg")
;; Use setup's --force option to override.

;; Present the following code to your LISP system at startup, either
;; by adding it to your implementation's startup file
;; (~/.sbclrc, ~/.ccl-init.lisp ~/.eclrc, ~/.clasprc  ~/.abclrc, ~/.clinit.cl,
;;  ~/.roswell/init.lisp)
;; or overriding it completely on the command line
;; (eg. sbcl --userinit init.lisp)

;; Note: To add other systems not registered in ocicl, simply use the
;; :tree keyword (as opposed to the default :directory) as follows. Also,
;; I wrap this initializing with `ignore-errors` so that the CL implementation
;; fails quietly...

#-ocicl
(ignore-errors
  (when (probe-file (uiop:xdg-data-home #P"ocicl/ocicl-runtime.lisp"))
    (load (uiop:xdg-data-home #P"ocicl/ocicl-runtime.lisp")))
  (asdf:initialize-source-registry
   (list :source-registry
         ;; Needed to store non-available ocicl systems in ocicl/
         (list :tree (uiop:getcwd))
         :inherit-configuration)))

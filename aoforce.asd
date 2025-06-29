(defsystem "aoforce"
  :description "A Common Lisp Configuration Environment Resource"
  :author "Erik P Almaraz <erikalmaraz@fastmail.com>"
  :license "Apache-2.0"
  :version (:read-file-form "version.sexp" :at (0 1))
  :class :package-inferred-system
  ;; `:defsystem-depends-on' is used to declare the dependency on any
  ;; ASDF dependencies defined in their own system (preferred to here).
  ;; see https://asdf.common-lisp.dev/asdf.html
  ;; :defsystem-depends-on (:my-asdf-system)
  :depends-on ("bordeaux-threads"
               "lparallel"
               "closer-mop"
               ;; Dependencies are also detected by ASDF using the
               ;; :import-from clause (i.e. `:import-from` #:system-name)...
               ;; So not necessary to place all here, just the system-wide ones
               ;; when using the package-inferred-system...
               ;; https://asdf.common-lisp.dev/asdf.htmlx
               ;; AOFORCE
               "aoforce/setup"
               "aoforce/core/all"
               ;; Libraries
               "aoforce/libraries/learn-cl/all"
               "aoforce/libraries/websxcl/all"
               "aoforce/libraries/cl-bexp/all")
  :in-order-to ((test-op (test-op "aoforce/test")))
  :long-description "
A collection of Common Lisp development environment configuration resources,
tools, and a playground for building new projects.")


(defsystem "aoforce/test"
  :depends-on ("fiveam"
               "aoforce/tests/all")
  :perform (test-op (op c)
                    (symbol-call :fiveam :run!
                                 (find-symbol* :root-suite :aoforce/test))))

(defsystem "aoforce/docs"
  :depends-on ())

;;; Register Systems
;;; The function `register-system-packages' must be called to register packages
;;; used or provided by your system when the name of the system/file that 
;;; provides the package is not the same as the package name
;;; (converted to lower case).

(asdf:register-system-packages "aoforce/tests/all" '(:aoforce/test))

(register-system-packages "bordeaux-threads" '(:bt :bt2 :bordeaux-threads-2))

(register-system-packages "closer-mop" '(:c2mop :c2cl :c2cl-user))

(register-system-packages "fiveam" '(:5am))

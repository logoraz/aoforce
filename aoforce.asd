(defsystem "aoforce"
  :description "A Common Lisp Configuration Environment Resource"
  :author "Erik P Almaraz <erikalmaraz@fastmail.com>"
  :license "Apache-2.0"
  :version (:read-file-form "version.sexp" :at (0 1))
  :depends-on ("bordeaux-threads"
               "lparallel"
               "closer-mop"
               "osicat"
               "green-threads"
               "mito"
               ;; Local Systems
               "learn-cl"
               "websxcl"
               "cl-bexp")
  :serial t
  :components
  ((:module "source"
    :serial t
    :components
    ((:module "utils"
      :serial t
      :components
      ((:file "base")))
     (:file "aoforce")
     (:file "database")))
   (:file "setup" :depends-on ("source")))

  :in-order-to ((test-op (test-op "aoforce/test")))
  :long-description "
A collection of Common Lisp development environment configuration resources,
tools, and a playground for building new projects.")

;; (defsystem "aoforce/libraries"
;;   :depends-on ("learn-cl"
;;                "websxcl"
;;                "cl-bexp"))

(defsystem "aoforce/test"
  :depends-on ("fiveam")
  :pathname "tests"
  :serial t
  :components
  ((:file "suite"))
  :perform (test-op (op c)
                    (symbol-call :fiveam :run!
                                 (find-symbol* :root-suite :tests/suite))))

(defsystem "aoforce/docs"
  :depends-on ()
  :pathname "docs")

;;; Register Systems
;;; The function `register-system-packages' must be called to register packages
;;; used or provided by your system when the name of the system/file that 
;;; provides the package is not the same as the package name
;;; (converted to lower case).

(register-system-packages "bordeaux-threads" '(:bt :bt2 :bordeaux-threads-2))

(register-system-packages "closer-mop" '(:c2mop :c2cl :c2cl-user))

(register-system-packages "fiveam" '(:5am))

(asdf:register-system-packages "learn-cl" '(:sdraw :dtrace :fcalc))

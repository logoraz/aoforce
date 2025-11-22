(defsystem "confr"
  :description "Common Lisp Project/Libary Templating Toolset"
  :author "Erik P Almaraz <erikalmaraz@fastmail.com>"
  :license "Apache-2.0"
  :version (:read-file-form "version.sexp" :at (0 1))
  :depends-on ("iterate"
               "bordeaux-threads"
               "lparallel"
               "cl-ppcre"
               "osicat"
               "trivial-gray-streams")
  :components ;; Map of System
  ((:module "source"
    :components
    ((:module "utils" ;; Establish first our toolbox
      :components
      ((:file "base")))
     ;; Build out the core of confr
     (:module "core"
      :depends-on ("utils")
      :components
      ((:file "database")))
     ;; Finally scaffold confr
     (:file "confr"  :depends-on ("utils" "core")))))
  :in-order-to ((test-op (test-op "confr/tests")))
  :long-description "")

;; ==============================================================================
;; Register Systems
;; ==============================================================================
;; The function `register-system-packages' must be called to register packages
;; used or provided by your system when the name of the system/file that 
;; provides the package is not the same as the package name
;; (converted to lower case).
(register-system-packages "bordeaux-threads" '(:bt :bt2 :bordeaux-threads-2))
(register-system-packages "closer-mop" '(:c2mop :c2cl :c2cl-user))
(register-system-packages "fiveam" '(:5am))

;; ==============================================================================
;; Secondary Systems
;; ==============================================================================
(defsystem "confr/tests"
  :description "Unit tests"
  :depends-on ("confr" "fiveam")
  :components
  ((:module "tests"
    :components
    ((:file "suite"))))
  :perform (test-op (o c)
                    (symbol-call :fiveam :run! :suite)))

(defsystem "confr/docs"
  :description "Documentation framework"
  :depends-on ("confr" "3bmd" "print-licenses")
  :components
  ((:module "docs"
    :components
    ((:file "confr-docs")))))

(defsystem "confr/executable"
  :description "Build executable"
  :depends-on ("confr")
  :build-operation "program-op"
  :build-pathname "confr-preexe"
  :entry-point "confr:main")

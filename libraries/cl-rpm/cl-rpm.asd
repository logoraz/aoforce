(defsystem "cl-rpm"
  :description "Interface to the RedHat Package Management System Common Lisp"
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
     ;; Build out the core of cl-rpm
     (:module "core"
      :depends-on ("utils")
      :components
      ((:file "database")))
     ;; Finally scaffold cl-rpm
     (:file "rpm"  :depends-on ("utils" "core")))))
  :in-order-to ((test-op (test-op "cl-rpm/tests")))
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
(defsystem "cl-rpm/tests"
  :description "Unit tests"
  :depends-on ("cl-rpm" "fiveam")
  :components
  ((:module "tests"
    :components
    ((:file "suite"))))
  :perform (test-op (o c)
                    (symbol-call :fiveam :run! :suite)))

(defsystem "cl-rpm/docs"
  :description "Documentation framework"
  :depends-on ("cl-rpm" "3bmd" "print-licenses")
  :components
  ((:module "docs"
    :components
    ((:file "rpm-docs")))))

(defsystem "cl-rpm/executable"
  :description "Build executable"
  :depends-on ("cl-rpm")
  :build-operation "program-op"
  :build-pathname "rpm-preexe"
  :entry-point "rpm:main")

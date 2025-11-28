(defsystem "chem-manager"
  :description "Chemical inventory Manager written in Common Lisp"
  :author "Erik P Almaraz <erikalmaraz@fastmail.com>"
  :license "Apache-2.0"
  :version (:read-file-form "version.sexp" :at (0 1))
  :depends-on
  ("iterate"
   "bordeaux-threads"
   "lparallel"
   "cl-ppcre"
   "osicat"
   "trivial-gray-streams"
   "cl-dbi")
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
      ((:file "chemgr-db")))
     ;; Finally scaffold cl-rpm
     (:file "chem-manager"  :depends-on ("utils" "core")))))
  :in-order-to ((test-op (test-op "chem-manager/tests")))
  :long-description "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Register Systems
;;;
;; The function `register-system-packages' must be called to register packages
;; used or provided by your system when the name of the system/file that 
;; provides the package is not the same as the package name
;; (converted to lower case).
(register-system-packages "iterate" '(:iter))
(register-system-packages "bordeaux-threads" '(:bt :bt2 :bordeaux-threads-2))
(register-system-packages "fiveam" '(:5am))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Secondary Systems
;;;
(defsystem "chem-manager/tests"
  :description "Unit tests"
  :depends-on ("chem-manager" "fiveam")
  :components
  ((:module "tests"
    :components
    ((:file "suite"))))
  :perform (test-op (o c)
                    (symbol-call :fiveam :run! :suite)))

(defsystem "chem-manager/docs"
  :description "Documentation framework"
  :depends-on ("chem-manager" "3bmd" "print-licenses")
  :components
  ((:module "docs"
    :components
    ((:file "chem-manager-docs")))))

(defsystem "chem-manager/executable"
  :description "Build executable"
  :depends-on ("chem-manager")
  :build-operation "program-op"
  :build-pathname "chem-manager-preexe"
  :entry-point "chem-manager:main")

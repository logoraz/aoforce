(defsystem "aoforce"
  :description "A Common Lisp Configuration Environment Resource"
  :author "Erik P Almaraz <erikalmaraz@fastmail.com>"
  :license "Apache-2.0"
  :version (:read-file-form "version.sexp" :at (0 1))
  :depends-on ; External Dependencies
  ("iterate"
   "bordeaux-threads"
   "lparallel"
   "cl-ppcre"
   "osicat"
   "trivial-gray-streams"
   "cl-gtk4"
   "cl-gtk4.adw"
   "cl-gdk4"
   ;; Local Systems (aka libraries)
   "confr")
  :components ; Map of System (Internals)
  ((:module "source"
    :components
    ((:module "utils" ; Establish utils/toolbox
      :components
      ((:file "syntax")))
     (:module "core" ; Build out the core of aoforce
      :depends-on ("utils")
      :components
      ((:file "database")
       (:file "config-manager")))
     (:module "renderer" ; UI/X Frontends
      :serial t
      :components
      ((:file "widgets")
       (:file "layouts")
       (:file "controller")
       (:file "builder")
       (:file "app")
       #+nil (:file "adw")))
     ;; Finally scaffold aoforce
     (:file "setup"   :depends-on ("utils" "core"))
     (:file "aoforce" :depends-on ("utils" "core" "renderer")))))
  :in-order-to ((test-op (test-op "aoforce/tests")))
  :long-description "A collection of Common Lisp development environment
configuration resources, tools, and a playground for building new projects.")

;;; =============================================================================
;;; Register Systems
;;; =============================================================================
;; The function `register-system-packages' must be called to register packages
;; used or provided by your system when the name of the system/file that
;; provides the package is not the same as the package name
;; (converted to lower case).
(register-system-packages "iterate" '(:iter))
(register-system-packages "bordeaux-threads" '(:bt :bt2 :bordeaux-threads-2))
(register-system-packages "fiveam" '(:5am))

;;; =============================================================================
;;; Secondary Systems
;;; =============================================================================
(defsystem "aoforce/tests"
  :description "Unit tests"
  :depends-on ("aoforce"
               "fiveam")
  :components
  ((:module "tests"
    :components
    ((:file "suite"))))
  :perform (test-op (o c)
                    (symbol-call :fiveam :run! :suite)))

(defsystem "aoforce/docs"
  :description "Documentation framework"
  :depends-on ("aoforce"
               "3bmd"
               "colorize"
               "print-licenses")
  :components
  ((:module "docs"
    :components
    ((:file "aoforce-docs")))))

(defsystem "aoforce/libraries"
  :description "Extra libraries to bring in if needed"
  :depends-on ("confr"
               "cl-rpm"
               "learn-cl"))

(defsystem "aoforce/executable"
  :description "Build executable"
  :depends-on ("aoforce")
  :build-operation "program-op"
  :build-pathname "aoforce-preexe"
  :entry-point "aoforce:main")

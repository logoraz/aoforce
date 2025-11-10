(defsystem "aoforce"
  :description "A Common Lisp Configuration Environment Resource"
  :author "Erik P Almaraz <erikalmaraz@fastmail.com>"
  :license "Apache-2.0"
  :version (:read-file-form "version.sexp" :at (0 1))
  :depends-on ;; External Dependencies 
  ("bordeaux-threads"
   "closer-mop"
   "fast-generic-functions"
   "osicat"
   "cl-ppcre"
   "trivial-gray-streams"
   "micros"
   "slynk"
   #+sbcl "cl-gtk4"
   #+sbcl "cl-gtk4.adw"
   #+sbcl "cl-gdk4"
   ;; Local Systems (aka libraries)
   "confr")
  :components ;; Map of System (Internals)
  ((:module "source"
    :components
    (;; Establish utils/toolbox
     (:module "utils"
      :components
      ((:file "syntax")
       (:file "files")
       (:file "strings")
       (:file "shell")                               
       (:file "servers")))
     ;; Build out the core of aoforce
     (:module "core"
      :depends-on ("utils")
      :components
      ((:file "database")))
     ;; UI/X Frontends
     (:module "frontends"
      :components
      (#+sbcl (:file "aofr-adw")))
     ;; Finally scaffold aoforce
     (:file "setup"    :depends-on ("utils"))
     (:file "aoforce"  :depends-on ("utils" "core" "frontends")))))
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
(register-system-packages "bordeaux-threads" '(:bt :bt2 :bordeaux-threads-2))
(register-system-packages "closer-mop" '(:c2mop :c2cl :c2cl-user))
(register-system-packages "fiveam" '(:5am))

;;; =============================================================================
;;; Secondary Systems
;;; =============================================================================
(defsystem "aoforce/tests"
  :description "Unit tests"
  :depends-on ("aoforce" "fiveam")
  :components
  ((:module "tests"
    :components
    ((:file "suite"))))
  :perform (test-op (o c)
                    (symbol-call :fiveam :run! :suite)))

(defsystem "aoforce/docs"
  :description "Documentation framework"
  :depends-on ("aoforce" "3bmd" "print-licenses")
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

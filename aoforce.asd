(defsystem "aoforce"
  :description "A Common Lisp Configuration Environment Resource"
  :author "Erik P Almaraz <erikalmaraz@fastmail.com>"
  :license "Apache-2.0"
  :version (:read-file-form "version.sexp" :at (0 1))
  :depends-on ("bordeaux-threads"
               "closer-mop"
               "fast-generic-functions"
               "osicat"
               "trivial-gray-streams"
               "cl-ppcre"
               "mito"
               "micros"
               "slynk"
               #+sbcl "cl-cffi-gtk4"
               ;; Local Systems (aka libraries)
               "cl-bexp")
  :components ;; Map of System
  ((:module "source"
    :components
    ((:module "utils" ;; Establish first our toolbox
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
     ;; Finally scaffold aoforce
     (:file "setup"    :depends-on ("utils"))
     (:file "aoforce"  :depends-on ("utils" "core"))))
   ;; UI/X Frontends
   (:module "frontends"
    :components
    (#+sbcl (:file "gtk4-tutorial"))))
  :long-description "A collection of Common Lisp development environment 
configuration resources, tools, and a playground for building new projects.")

;;; Register Systems
;;; The function `register-system-packages' must be called to register packages
;;; used or provided by your system when the name of the system/file that 
;;; provides the package is not the same as the package name
;;; (converted to lower case).
(register-system-packages "bordeaux-threads" '(:bt :bt2 :bordeaux-threads-2))
(register-system-packages "closer-mop" '(:c2mop :c2cl :c2cl-user))


(defsystem "aoforce/executable"
  :description "Build executable"
  :depends-on ("aoforce")
  :build-operation "program-op"
  :build-pathname "aoforce-preexe"
  :entry-point "aoforce:main"
  :long-description "Simply build with ccl/sbcl via (asdf:make :aoforce/executable)")


(defsystem "aoforce/docs"
  :description "Documentation framework"
  :depends-on ("aoforce" "3bmd" "print-licenses")
  :components
  ((:module "docs"
    :components
    ((:file "aoforce-docs"))))
  :long-description "aoforce documentation creation utils and additional
documentation.
Use (print-licensnes:print-licenses :print-licenses) to check dependency
licenses.")


(defsystem "aoforce/libraries"
  :description "Extra libraries to bring in if needed"
  :depends-on ("learn-cl"
               "websxcl"))


(defsystem "aoforce/test"
  :description "Unit tests"
  :depends-on ("aoforce" "rove" "mito")
  :components
  ((:module "tests"
    :components
    ((:file "suite"))))
  :perform (test-op (o c)
                    (unless (symbol-call :rove :run c)
                      (error "Tests failed"))))





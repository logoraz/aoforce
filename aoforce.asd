(defsystem "aoforce"
  :description "A Common Lisp Configuration Environment Resource"
  :author "Erik P Almaraz <erikalmaraz@fastmail.com>"
  :license "Apache-2.0"
  :version (:read-file-form "version.sexp" :at (0 1))
  :depends-on ("bordeaux-threads" "closer-mop"
               "osicat" "trivial-gray-streams"
               "mito" "cl-ppcre"
               "micros" "slynk"
               #+sbcl "cl-cffi-gtk4"
               ;; Local Systems (aka libraries)
               "cl-bexp")

  ;; Map of System Hierarchy
  :components
  ((:module "source"
    :components
    ((:module "utils" ;; Establish first our toolbox
      :components
      ((:file "syntax")
       (:file "files")
       (:file "servers")
       (:file "shell")
       (:file "strings")))

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
  
  ;; Building (executables) & Testing
  ;; Simply build with ccl/sbcl via (asdf:make :formulatum)
  ;; (ccl:save-application #p"formulatum-ccl" 
  ;;                       :toplevel #'formulatum:main 
  ;;                       :prepend-kernel t)
  :build-operation "program-op"
  :build-pathname "aoforce-preexe"
  :entry-point "aoforce:main"
  :in-order-to ((test-op (test-op "aoforce/test")))
  :long-description "
A collection of Common Lisp development environment configuration resources,
tools, and a playground for building new projects.")

;;; Register Systems
;;; The function `register-system-packages' must be called to register packages
;;; used or provided by your system when the name of the system/file that 
;;; provides the package is not the same as the package name
;;; (converted to lower case).
(register-system-packages "bordeaux-threads" '(:bt :bt2 :bordeaux-threads-2))
(register-system-packages "closer-mop" '(:c2mop :c2cl :c2cl-user))


(defsystem "aoforce/libraries"
  ;; Extra libraries to bring in if needed
  :depends-on ("learn-cl"
               "websxcl"))


(defsystem "aoforce/test"
  :depends-on ("rove" "mito")
  :components
  ((:module "tests"
    :components
    ((:file "suite"))))
  :perform (test-op (o c)
                    (unless (symbol-call :rove :run c)
                      (error "Tests failed"))))


(defsystem "aoforce/docs"
  :depends-on ())



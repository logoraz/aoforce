(defsystem #:aoforce
  :description "A Common Lisp Configuration Environment Resource"
  :author "Erik P Almaraz <erikalmaraz@fastmail.com"
  :license "MIT"
  :version (:read-file-form "version.sexp" :at (0 1))
  :defsystem-depends-on (:asdf-package-system)
  :class :package-inferred-system
  :depends-on (#:bordeaux-threads
               #:lparallel
               #:closer-mop
               #:local-time
               #:cl-interpol
               #:cl-ppcre
               #:osicat
               ;; AOFORCE
               #:aoforce/setup
               #:aoforce/core/all
               ;; Systems
               #:aoforce/libraries/juego-clos/all
               #:aoforce/libraries/cl-bexp/all
               #:aoforce/libraries/website/all
               #:aoforce/libraries/chembook/all
               #:aoforce/libraries/learncl/all)
  :in-order-to ((test-op (test-op #:aoforce-test))))

;; setup/config (setup script)
(register-system-packages "aoforce/setup" '(#:setup))

;; Core (aka src)
(register-system-packages "aoforce/core/all" '(#:aoforce))

;; Libraries (Systems)
(register-system-packages "aoforce/libraries/learncl/all"    '(#:learncl))
(register-system-packages "aoforce/libraries/website/all"    '(#:web))
(register-system-packages "aoforce/libraries/chembook/all"   '(#:chembook))
(register-system-packages "aoforce/libraries/cl-bexp/all"    '(#:bexp))
(register-system-packages "aoforce/libraries/juego-clos/all" '(#:juego))

;; External Systems
(register-system-packages "closer-mop" '(#:closer-mop #:c2mop
                                         #:closer-common-lisp #:c2cl
                                         #:closer-common-lisp-user #:c2cl-user))

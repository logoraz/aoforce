(defsystem #:aoforce
  :description "A Common Lisp Configuration Environment Resource"
  :author "Erik P Almaraz <erikalmaraz@fastmail.com"
  :license "MIT"
  :version (:read-file-form "version.sexp" :at (0 1))
  :class :package-inferred-system
  ;; `:defsystem-depends-on' is used to declare the dependency on any
  ;; ASDF dependencies defined in their own system (preferred to here).
  ;; see https://asdf.common-lisp.dev/asdf.html
  ;; :defsystem-depends-on (:my-asdf-system)
  :depends-on (#:bordeaux-threads
               #:lparallel
               #:green-threads
               #:closer-mop
               ;; Dependencies are also detected by ASDF using the
               ;; :import-from clause (i.e. `:import-from` #:system-name)...
               ;; So not necessary to place all here, just the system-wide ones
               ;; when using the package-inferred-system...
               ;; https://asdf.common-lisp.dev/asdf.htmlx
               ;; AOFORCE
               #:aoforce/setup
               #:aoforce/core/all
               #:aoforce/libraries/cl-bexp/all
               #:aoforce/libraries/webs-cl/all
               #:aoforce/libraries/learn-cl/all)
  :in-order-to ((test-op (test-op #:aoforce-test)))
  :long-description "

Long Description here...

")

;;; Register Systems
;;; The function `register-system-packages' must be called to register packages
;;; used or provided by your system when the name of the system/file that 
;;; provides the package is not the same as the package name
;;; (converted to lower case).

(register-system-packages "bordeaux-threads" '(#:bordeaux-threads
                                               #:bt2 #:bordeaux-threads-2))

(register-system-packages "closer-mop" '(#:closer-mop #:c2mop
                                         #:closer-common-lisp #:c2cl
                                         #:closer-common-lisp-user #:c2cl-user))

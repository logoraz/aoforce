(defsystem :confer
  :description "Common Lisp Configuration Environment Resource"
  :author "Erik P Almaraz <erikalmaraz@fastmail.com"
  :license "BSD-3"
  :version (:read-file-form "version.sexp")
  ;; :defsystem-depends-on (:asdf-package-system)
  :class :package-inferred-system
  :depends-on (:bordeaux-threads
               :lparallel
               :closer-mop
               :confer/core/all
               :confer/libraries/chembook/all
               :confer/libraries/learncl/all)
  :in-order-to ((test-op (test-op :confer-test))))

;; Core
(register-system-packages "confer/core/all" '(:confer))

;; Libraries
(register-system-packages "confer/library/learncl/all" '(:learncl))

;; Externals
(register-system-packages "closer-mop"
                          '(:c2mop 
                            :closer-common-lisp 
                            :c2cl 
                            :closer-common-lisp-user
                            :c2cl-user))

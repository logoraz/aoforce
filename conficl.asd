(defsystem #:conficl
  :description "Personal Common Lisp Dev Environment Configuration"
  :author "Erik Almaraz <erikalmaraz@fastmail.com"
  :license "BSD-3"
  :version "0.0.1"
  #+nil
  :defsystem-depends-on (:asdf-package-system)
  :class :package-inferred-system
  :depends-on ("rpm"
               "foo/bar/all"
               "foo/qux/all")
  :in-order-to ((test-op (test-op "foo/tests"))))


(defsystem #:foo/tests
  :class :package-inferred-system
  :depends-on ("rove"
               "foo/tests/all")
  :perform (test-op (o c) (symbol-call #:foo/tests/all 
                                       #:run-all-tests)))


(register-system-packages "foo/bar/all"   '(#:foo/bar))
(register-system-packages "foo/qux/all"   '(#:foo/qux))
(register-system-packages "foo/corge/all" '(#:corge))
(register-system-packages "foo/tests/all" '(#:tests))

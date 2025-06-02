(defsystem #:aoforce-test
  :description "Unit Testing System for aoforce"
  :author "Erik P Almaraz <erikalmaraz@fastmail.com"
  :license "MIT"
  :version (:read-file-form "version.sexp" :at (0 1))
  :class :package-inferred-system
  :depends-on (#:fiveam
               #:aoforce-test/tests/all)
  :perform (test-op (op c) 
                    (symbol-call 'fiveam 'run!
                                 (find-symbol* 'root-suite 'tests))))

(register-system-packages "fiveam" '(#:fiveam #:5am))

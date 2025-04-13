(defsystem :confer-test
  :class :package-inferred-system
  :depends-on (:fiveam
               :confer-test/tests/all)
  :perform (test-op (op c) 
                    (symbol-call :fiveam :run!
                                 (find-symbol* :root-suite 
                                               :tests))))

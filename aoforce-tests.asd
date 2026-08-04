(defsystem "aoforce-tests"
  :description "Unit tests"
  :class :package-inferred-system
  :pathname "tests"
  :depends-on ("aoforce-tests/suite")
  :perform (test-op (o c)
                    (symbol-call :fiveam :run!
                                 (find-symbol "SUITE"
                                              :aoforce-tests/suite))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Register External Systems
(register-system-packages "fiveam" '("5AM"))

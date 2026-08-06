(defsystem "aoforce-docs"
  :description "Documentation framework"
  :class :package-inferred-system
  :pathname "docs"
  :depends-on ("aoforce-docs/generator")
  :perform (build-op (o c)
                     (symbol-call 'aoforce-docs/generator 'build-docs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Register External Systems

(register-system-packages "3bmd-ext-code-blocks" '(:3bmd-code-blocks))

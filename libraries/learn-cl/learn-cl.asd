(defsystem "learn-cl"
  :description "Learning tools"
  :author "Erik P Almaraz <erikalmaraz@fastmail.com"
  :license "Various/Apache-2.0"
  :version (:read-file-form "version.sexp" :at (0 1))
  :class :package-inferred-system
  :pathname "src"
  :depends-on ("learn-cl/main")
  :long-description "
A library exibiting how to setup a library staging various learnings.
")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Register Systems

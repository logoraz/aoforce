(defsystem :aofrc
  :description "Short alias for aoforce"
  :author "Erik P Almaraz <erikalmaraz@fastmail.com>"
  :license "MIT"
  :version (:read-file-form "version.sexp" :at (0 1))
  :depends-on (:aoforce)
  :in-order-to ((test-op (test-op "aoforce"))))

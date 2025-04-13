(defsystem :confr
  :description "Short alias for confer"
  :author "Erik P Almaraz <erikalmaraz@fastmail.com>"
  :license "BSD-3"
  :version (:read-file-form "version.sexp")
  :depends-on (:confer)
  :in-order-to ((test-op (test-op :confure-test))))
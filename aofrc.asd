(defsystem #:aofrc
  :description "Short alias for aoforce"
  :author "Erik P Almaraz <erikalmaraz@fastmail.com>"
  :license "MIT"
  :version (:read-file-form "version.sexp")
  :depends-on (#:aoforce))

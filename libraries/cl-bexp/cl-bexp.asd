(defsystem "cl-bexp"
  :description "Build Expression in Common Lisp"
  :author "Erik P Almaraz <erikalmaraz@fastmail.com"
  :license "Apache-2.0"
  :version (:read-file-form "version.sexp" :at (0 1))
  :depends-on ("cl-ppcre"
               "local-time")
  :serial t
  :components
  ((:file "core"))
  :long-description "
TBD")

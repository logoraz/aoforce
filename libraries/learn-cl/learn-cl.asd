(defsystem "learn-cl"
  :description "Learning tools"
  :author "Erik P Almaraz <erikalmaraz@fastmail.com"
  :license "Apache-2.0"
  :version (:read-file-form "version.sexp" :at (0 1))
  :depends-on ()
  :serial t
  :components
  ((:file "sdraw")
   (:file "dtrace")
   (:file "fcalc")
   (:file "scalc"))
  :long-description "
TBD")

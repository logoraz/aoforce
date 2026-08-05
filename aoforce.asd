(defsystem "aoforce"
  :description "A Common Lisp Configuration Environment Resource"
  :author "Erik P Almaraz <erikalmaraz@fastmail.com>"
  :license "Apache-2.0"
  :version (:read-file-form "version.sexp" :at (0 1))
  :class :package-inferred-system
  :pathname "src"
  :depends-on
  ("aoforce/aoforce")
  :in-order-to ((test-op (test-op "aoforce-tests")))
  :long-description "A collection of Common Lisp development environment
configuration resources, tools, and a playground for building new projects.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Register Systems

(register-system-packages "iterate" '(:iter))
(register-system-packages "bordeaux-threads" '(:bt :bt2))
(register-system-packages "cl-dbi" '(:dbi))
(register-system-packages "cl-cffi-gtk4" '(:gtk :gdk))
;; cl-cffi-gtk4 dependencies
(register-system-packages "cl-cffi-glib" '(:gobject :glib :gio))
(register-system-packages "cl-cffi-gdk-pixbuf" '(:gdk-pixbuf))
(register-system-packages "cl-cffi-graphene" '(:graphene))
(register-system-packages "cl-cffi-pango" '(:pango))
(register-system-packages "cl-cffi-cairo" '(:cairo))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Subsystems

(defsystem "aoforce/libraries"
  :description "Extra libraries to bring in if needed"
  :depends-on ("learn-cl"))

(defsystem "aoforce/executable"
  :description "Build executable"
  :depends-on ("aoforce")
  :build-operation "program-op"
  :build-pathname "aoforce-preexe"
  :entry-point "aoforce:main")

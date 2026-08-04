(defpackage :aoforce-docs/generator
  (:nicknames #:docs)
  (:use :cl
        :aoforce)
  (:import-from #:3bmd)
  (:import-from #:colorize)
  (:import-from #:print-licenses)
  (:export )
  (:documentation "Documentation system for aoforce")  )

(in-package :aoforce-docs/generator)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; References
;;;
;;; Ref: https://github.com/rabbibotton/clog/ --> source/clog-docs.lisp


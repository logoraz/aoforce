(defpackage :utils/servers
  (:use :cl :uiop)
  (:import-from :micros)
  (:import-from :slynk)
  (:export #:*micros-port*
           #:start-micros
           #:stop-micros
           #:*slynk-port*
           #:start-slynk
           #:stop-slynk)
  (:documentation "TCP Lisp Servers"))
(in-package :utils/servers)


;; TCP servers

;; Only works in Lem with sbcl (ccl hangs)
;; TODO: Identify & Report bug with Lem regarding this...
(defvar *micros-port* 4005
  "Default Micros server port for Formulatum.")

(defun start-micros (&optional (micros-port *micros-port*))
  "Start a Micros server."
  (micros:create-server :port micros-port :dont-close t)
  (format nil "Micros server started at port ~A" micros-port))

(defun stop-micros (&optional (micros-port *micros-port*))
  "Stop current Micros server."
  (micros:stop-server micros-port)
  (format nil "Closing Micros server at port ~A" micros-port))

;;; Provided to Connect to Emacs
(defvar *slynk-port* 4007
  "Default Micros server port for Formulatum.")

(defun start-slynk (&optional (slynk-port *slynk-port*))
  "Start a Micros server."
  (slynk:create-server :port slynk-port :dont-close t)
  (format nil "Slynk server started at port ~A" slynk-port))

(defun stop-slynk (&optional (slynk-port *slynk-port*))
  "Stop current Slynk server."
  (slynk:stop-server slynk-port)
  (format nil "Closing Slynk server at port ~A" slynk-port))


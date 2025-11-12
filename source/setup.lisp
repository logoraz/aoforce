(defpackage :setup
  (:use :cl
        :core/config-manager)
  (:export #:deploy)
  (:documentation "Setup script to scaffold CL configuration/environment."))
(in-package :setup)

;;; =============================================================================
;;; Current Configuration Setup
;;; =============================================================================
(defparameter *config-mgr* (make-instance 'config-manager))

;; Shell Environment
;; 1.  ~/Work/aoforce/files/bash/dot-bashrc.sh --> ~/.bashrc
(add-config *config-mgr*
            "Bash RC"
            "~/Work/aoforce/files/bash/dot-bashrc.sh"
            "~/.bashrc"
            :spec :symlink
            :type :file)

;; 2.  ~/Work/aoforce/files/bash/dot-bash_profile.sh --> ~/.bash_profile
(add-config *config-mgr*
            "Bash Profile"
            "~/Work/aoforce/files/bash/dot-bash_profile.sh"
            "~/.bash_profile"
            :spec :symlink
            :type :file)

;; Common Lisp Environment
;; 3.  ~/Work/aoforce/files/common-lisp/dot-sbclrc.lisp --> ~/.sbclrc
(add-config *config-mgr*
            "SBCL Config"
            "~/Work/aoforce/files/common-lisp/dot-sbclrc.lisp"
            "~/.sbclrc"
            :spec :symlink
            :type :file)

;; 4.  ~/Work/aoforce/files/common-lisp/dot-ccl-init.lisp --> ~/.ccl-init.lisp
(add-config *config-mgr*
            "CCL Config"
            "~/Work/aoforce/files/common-lisp/dot-ccl-init.lisp"
            "~/.ccl-init.lisp"
            :spec :symlink
            :type :file)

;; Common Lisn Programs (XDG_DATA_HOME/common-lisp/bin)
;; 5.  ~/Work/builds/ccl/lx86cl64 --> $HOME/.local/bin/ccl
(add-config *config-mgr*
            "CCL Executable"
            "~/Work/builds/ccl/lx86cl64"
            "~/.local/bin/ccl"
            :spec :symlink
            :type :file)

;;; =============================================================================
;;; Deploy config-objects
;;; =============================================================================
(defun deploy ()
  "Setup & Deploy Your Configuration Environment."
  (list-configs *config-mgr*)
  (deploy-configs *config-mgr*))

;;; =============================================================================
;;; Install/Configure Common Lisp Utilities (i.e. ocicl, ccl, etc)
;;; =============================================================================
;;; TODO: Enable `config-manager` to install/setup Common Lisp utilities like
;;; ocicl...
]#+(or)
(progn
  sbcl --eval "(defconstant +dynamic-space-size+ 2048)" --load setup.lisp)

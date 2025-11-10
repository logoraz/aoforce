(defpackage :setup
  (:use :cl
        :utils/files
        :utils/shell
        :core/config)
  (:export #:cl-rcfile-slnks
           #:bash-rcfile-slnks
           #:cl-data-home-dir)
  (:documentation "Setup script to scaffold CL configuration/environment."))
(in-package :setup)

;;; =============================================================================
;;; Current setup of symlinks (XDG_CONFIG_HOME)
;;; =============================================================================
;; Shell Environment
;; 1.  ~/Work/aoforce/files/bash/dot-bashrc.sh --> ~/.bashrc
;; 2.  ~/Work/aoforce/files/bash/dot-bash_profile.sh --> ~/.bash_profile
;; 
;; Common Lisp Environment (XDG_CONFIG_HOME)
;; 3.  ~/Work/aoforce/files/common-lisp/dot-sbclrc.lisp --> ~/.sbclrc
;; 4.  ~/Work/aoforce/files/common-lisp/dot-ccl-init.lisp --> ~/.ccl-init.lisp
;;
;; Common Lisn Programs (XDG_DATA_HOME/common-lisp/bin)
;; 5.  ~/Work/builds/ccl/lx86cl64 --> $HOME/.local/bin/ccl
;;
(setf *config-objects*)

;;; =============================================================================
;;; Setup RC Files Symlinks
;;; =============================================================================
(defun cl-rcfile-symlinks (pathspec &key (ccl nil))
  "Function to setup Common Lisp symlinks to repo PATHSPEC."
  (create-symlink (uiop:xdg-config-home "Work/aoforce/files/dot-sbclrc.lisp")
                  "~/.sbclrc")
  (if ccl
      (create-symlink (uiop:xdg-config-home "Work/aoforce/files/dot-clasprc.lisp")
                      "~/.clasprc"))
  t)

(defun bash-rcfile-symlinks (pathspec)
  "Function to setup .bashrc & .bash_profile rcfiles."
  (create-symlink (uiop:xdg-config-home "Work/aoforce/files/dot-bashrc.sh")
                  "~/.bashrc")
  (create-symlink (uiop:xdg-config-home "Work/aoforce/files/dot-bash_profile.sh")
                  "~/.bash_profile")
  t)

(defun cl-data-home-dir ()
  "Thunk creating XDG_DATA_HOME Common Lisp directory"
  (ensure-dir (uiop:xdg-data-home #P"common-lisp")))


;;; =============================================================================
;;; Install or setup ocicl
;;; =============================================================================
#+or
(progn
  sbcl --eval "(defconstant +dynamic-space-size+ 2048)" --load setup.lisp)

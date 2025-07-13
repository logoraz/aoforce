(defpackage :setup
  (:nicknames :init)
  (:use :cl
        :utils/files
        :utils/shell)
  (:export #:cl-rcfile-slnks
           #:bash-rcfile-slnks
           #:cl-data-home-dir)
  (:documentation "Setup script to scaffold CL configuration/environment."))
(in-package :setup)


;;; Current setup of symlinks (XDG_CONFIG_HOME)
;; Shell Environment
;; 1.  aoforce/files/dot-bashrc.sh --> ~/.bashrc
;; 2.  aoforce/files/dot-bash_profile.sh --> ~/.bash_profile
;; Common Lisp Environment (XDG_CONFIG_HOME)
;; 3.  aoforce/files/dot-sbclrc.lisp --> ~/.sbclrc
;; 4.  aoforce/files/dot-ccl-init.lisp --> ~/.ccl-init.lisp
;; 5.  aoforce/files/dot-clapsrc.lisp --> ~/.clasprc
;; Common Lisnp Programs (XDG_DATA_HOME/common-lisp/bin)
;; 6.  (build) ccl/lx86cl64 --> HOME/.local/bin/ccl
;; 7.  (build) nyxt/nyxt --> HOME/.local/bin/nyxt
;; GNOME desktop files
;; 8. xdg-config-home/nyxt/assets/nyxt.desktop
;;    |--> xdg-data-home/applications/nyxt.desktop
;; 9.

;;; TODO: Refactor these...
;;;       & add functionality to store these changes to a database...

;; Setup RC Files symlnks
(defun cl-rcfile-slnks (pathspec &key (clasp nil))
  "Function to setup Common Lisp symlinks to repo PATHSPEC."
  (create-symlink (uiop:xdg-config-home "aoforce/files/dot-sbclrc.lisp")
                  "~/.sbclrc")
  (if clasp
      (create-symlink (uiop:xdg-config-home "aoforce/files/dot-clasprc.lisp")
                      "~/.clasprc"))
  t)

;; TODO: Need to first store exisiting rc files in database, delete, then
;; create symlinks...
(defun bash-rcfile-slnks ()
  "Function to setup .bashrc & .bash_profile rcfiles."
  (create-symlink (uiop:xdg-config-home "aoforce/files/dot-bashrc.sh")
                  "~/.bashrc")
  (create-symlink (uiop:xdg-config-home "aoforce/files/dot-bash_profile.sh")
                  "~/.bash_profile")
  t)

(defun cl-data-home-dir ()
  "Thunk creating XDG_DATA_HOME Common Lisp directory"
  (ensure-dir (uiop:xdg-data-home #P"common-lisp")))

(defun lem-setup ()
  "Thunk to setup Lem config."
  ;; Symlink lem.desktop to ~/.local/share/applications/lem.desktop
  (create-symlink (uiop:xdg-config-home #P"lem/lem.desktop")
                  (uiop:xdg-data-home #P"applications/lem.desktop"))
  t)

;; Install or setup ocicl
#+or
(progn
  sbcl --eval "(defconstant +dynamic-space-size+ 2048)" --load setup.lisp)

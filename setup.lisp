;;;; Common Lisp Environment Setup

(defpackage #:aoforce/setup
  (:nicknames #:setup)
  (:use #:cl
        #:aoforce/core/utils/base)
  (:export #:create-symlink
           #:dir-pathname
           #:cl-rcfile-slnks
           #:bash-rcfile-slnks
           #:cl-data-home-dir)
  (:documentation "Setup script to scaffold CL configuration/environment."))
(in-package #:aoforce/setup)


;; Setup RC Files symlnks
(defun cl-rcfile-slnks (pathspec &key (clasp nil))
  "Function to setup Common Lisp symlinks to repo PATHSPEC.
P"
  (create-symlink "~/.config/aoforce/rcfiles/dot-sbclrc.lisp"
                  "~/.sbclrc")
  (if clasp
      (create-symlink "~/.config/aoforce/rcfiles/dot-clasprc.lisp"
                      "~/.clasprc"))
  t)

(defun bash-rcfile-slnks ()
  "Function to setup .bashrc & .bash_profile rcfiles."
  (create-symlink "~/.config/aoforce/rcfiles/dot-bashrc.sh"
                  "~/.bashrc")
  (create-symlink "~/.config/aoforce/rcfiles/dot-bash_profile.sh"
                  "~/.bash_profile")
   t)

;; TODO: Replace sudo with doas (soft replace)
;; 1) Edit contents of '/etc/doas.conf with contents of /rcfiles/doas.conf or
;; 2) sudo cp */rcfiles/doas.conf --> /etc/doas.conf (remove orig doas first)

(defun cl-data-home-dir ()
  "Thunk creating XDG_DATA_HOME Common Lisp directory"
  (ensure-dir (uiop:xdg-data-home #P"common-lisp")))

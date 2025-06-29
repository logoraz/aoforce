(defpackage :aoforce/docs/scratch/scratch
  (:use :cl))
(in-package :aoforce/docs/scratch/scratch)

;; Find System version
(asdf:component-version (asdf:find-system :aoforce))

;; Figure out how to use mapcar!
(mapcar (lambda (x) (+ x 1)) '(1 2 3 4 5))

;; accessing/viewing the common lisp KEYWORD package
(describe (find-package 'keyword))
(apropos "" "KEYWORD")

;; describe on sbcl will show the symbols under a package(though it might truncate it because there's a ton of symbols in it)
;; Use do-symbols to iterate over all the symbols

;; Y-Combinator
(defun Y (f)
  (labels ((g (&rest args) (apply f #'g args)))
    #'g))

#+(or)
(defun Y (f)
  (lambda (&rest args)
    (apply f (Y f) args)))

#+(or)
(defun random-tree (depth)
  "Generate a random tree to climb!"
  (case (random 3)
    ((0) 'x)
    ((1) 'y)
    (t (list (random-tree (1- depth))
             (random-tree (1- depth))))))


;;; GNOME disable touchpad when external mouse connected
;; gsettings set org.gnome.desktop.peripherals.touchpad send-events \
;;           disabled-on-external-mouse

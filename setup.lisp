;;;; Common Lisp Environment Setup

(defpackage #:confer/setup
  (:nicknames #:setup)
  (:use #:cl
        #:confer/libraries/utils/base)
  (:export #:create-symlink))
(in-package #:confer/setup)



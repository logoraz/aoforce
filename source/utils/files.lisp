(defpackage :utils/files
  (:use :cl :uiop)
  (:import-from :osicat
                #:make-link
                #:file-kind)
  (:export #:ensure-dir
           #:symlinkp
           #:create-symlink)
  (:documentation "I/O and File conveniencies"))
(in-package :utils/files)


;;;==============================================================================
;;; References
;;;==============================================================================
;;; Ref: https://lispcookbook.github.io/cl-cookbook/files.html


;;;==============================================================================
;;; Pathnames
;;;==============================================================================
(defun dir-pathname (pathspec)
  "Converts the non-wild pathname designator PATHSPEC to directory form."
  (uiop:ensure-directory-pathname pathspec))


;;;==============================================================================
;;; directory creation/manipulation
;;;==============================================================================
;; Create directories (if they do no already exist)
(defun ensure-dir (pathspec &key (mode #o700))
  "Ensure directory in PATHSPEC exists"
  (let ((dir (dir-pathname pathspec)))
    (ensure-directories-exist dir :mode mode)))


;;;==============================================================================
;;; Symlinks
;;;==============================================================================
(defun symlinkp (pathspec)
  "Test whether PATHSPEC is a symlink."
  (eq (file-kind pathspec) :symbolic-link))

(defun create-symlink (src link &key (dir nil))
  "Create a symlink for SRC to LINK."
  (let ((src-dir (dir-pathname src)))
    (if dir (make-link link :target src-dir)
        (make-link link :target src))))


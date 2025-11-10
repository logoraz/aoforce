(defpackage :utils/streams
  (:use :cl)
  (:import-from :trivial-gray-streams)
  (:export )
  (:documentation "Tools/Utilities for streams."))
(in-package :utils/streams)


;;;==============================================================================
;;; References
;;;==============================================================================
;; 1. https://github.com/trivial-gray-streams/trivial-gray-streams
;; 2. https://github.com/fukamachi/rove


;;;==============================================================================
;;; Colors
;;;==============================================================================
(defvar *enable-colors*
  (or (not
       (or (equal (uiop:getenv "EMACS") "t")
           (uiop:getenv "INSIDE_EMACS")))
      (let ((enable-color-symbol (intern (string :*aofr-enable-colors*) :cl-user)))
        (and (boundp enable-color-symbol)
             (symbol-value enable-color-symbol))))
  "Boolean to enable colors.")

(defparameter *color-code*
  `((:red    . 31)
    (:green  . 32)
    (:yellow . 33)
    (:aqua   . 36)
    (:white  . 37)
    (:gray   . 90))
  "List of colors.")

(defun color-text (color text)
  "Function to color text based on color code."
  (if *enable-colors*
      (if (= (length text) 0)
          text
          (let ((code (cdr (assoc color *color-code*))))
            (assert code)
            (format nil "~C[~Am~A~C[0m"
                    #\Esc
                    code
                    text
                    #\Esc)))
      text))


;;;==============================================================================
;;; Streams
;;;==============================================================================
(defclass indent-stream (trivial-gray-stream-mixin
                         fundamental-character-output-stream)
  ((stream :initarg :stream
           :accessor stream-real-stream)
   (level :initarg :level
          :initform 0
          :accessor stream-indent-level)

   (line-column :initform 0
                :accessor indent-stream-line-column)
   (fresh-line-p :initform t
                 :accessor stream-fresh-line-p)))

(defun make-indent-stream (stream)
  (make-instance 'indent-stream :stream stream))

(defmacro with-indent ((stream level) &body body)
  (let ((g-level (gensym "LEVEL"))
        (g-stream (gensym "STREAM")))
    `(let ((,g-level ,level)
           (,g-stream ,stream))
       (incf (stream-indent-level ,g-stream) ,g-level)
       (unwind-protect (progn ,@body)
         (decf (stream-indent-level ,g-stream) ,g-level)))))

(defun new-line-char-p (char)
  (or (char= char #\Newline)
      (char= char #\Linefeed)))

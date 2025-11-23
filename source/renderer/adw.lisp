(defpackage #:renderer/adw
  (:use #:cl #:gtk4)
  (:export #:simple-repl
           #:main))

(in-package #:renderer/adw)

;;; =============================================================================
;;; Classes
;;; =============================================================================
(defclass app-controller ()
  ((current-expression
    :initform nil
    :accessor current-expression
    :documentation "Currently parsed expression, or NIL if invalid or not yet parsed.")
   (status-page
    :accessor status-page
    :documentation "The AdwStatusPage used for output.")
   (input-entry
    :accessor input-entry
    :documentation "The GtkEntry widget for input.")))

;;; =============================================================================
;;; Interface
;;; =============================================================================
(defgeneric evaluate-expression (repl)
  (:documentation "Evaluate the current expression."))

(defgeneric build-ui (repl window)
  (:documentation "Build the UI hierarchy."))

(defgeneric build-header-bar (repl)
  (:documentation "Create and configure the header bar."))

(defgeneric build-status-page (repl)
  (:documentation "Create and configure the main status page with logo and title."))

(defgeneric build-input-area (repl parent-box)
  (:documentation "Create the preferences group with input row."))

(defgeneric build-input-row (repl row)
  (:documentation "Configure the entry and attach it to the action row."))

(defgeneric build-button-bar (repl parent-box window)
  (:documentation "Create the horizontal bar with Exit/Eval buttons."))

;;; =============================================================================
;;; Methods — Core
;;; =============================================================================
(defmethod evaluate-expression ((repl app-controller))
  (when (current-expression repl)
    (setf (adw:status-page-description (status-page repl))
          (princ-to-string
           (handler-case (eval (current-expression repl))
             (error (err) err)))))
  (setf (current-expression repl) nil)
  (let ((entry (input-entry repl)))
    (setf (entry-buffer-text (entry-buffer entry)) "")
    (widget-remove-css-class entry "error")))

;;; =============================================================================
;;; Methods — UI Construction
;;; =============================================================================
(defmethod build-header-bar ((repl app-controller))
  (let ((header-bar (adw:make-header-bar)))
    (setf (adw:header-bar-title-widget header-bar)
          (adw:make-window-title :title (lisp-implementation-type)
                                 :subtitle (lisp-implementation-version)))
    header-bar))

(defmethod build-status-page ((repl app-controller))
  (let ((page (adw:make-status-page)))
    (setf (status-page repl) page)
    (setf (widget-hexpand-p page) t
          (widget-vexpand-p page) t
          (adw:status-page-paintable page) (gdk:make-texture :path "assets/lisp-logo.png")
          (adw:status-page-title page) "AOFORCE"
          (adw:status-page-description page) " ")
    page))

(defmethod build-input-row ((repl app-controller) row)
  (let ((entry (make-entry)))
    (setf (input-entry repl) entry)
    (setf (widget-valign entry) +align-center+
          (widget-hexpand-p entry) t)
    (connect entry "changed"
             (lambda (entry)
               (let ((text (entry-buffer-text (entry-buffer entry))))
                 (setf (current-expression repl)
                       (ignore-errors (read-from-string text)))
                 (funcall (if (current-expression repl)
                              #'widget-remove-css-class
                              #'widget-add-css-class)
                          entry "error"))))
    (connect entry "activate"
             (lambda (widget)
               (declare (ignore widget))
               (evaluate-expression repl)))
    (adw:action-row-add-suffix row entry)))

(defmethod build-input-area ((repl app-controller) parent-box)
  (let ((group (adw:make-preferences-group)))
    (setf (widget-margin-all group) 10)
    (let ((row (adw:make-action-row)))
      (setf (adw:preferences-row-title row)
            (format nil "~A> " (or (car (package-nicknames *package*))
                                   (package-name *package*))))
      (build-input-row repl row)
      (adw:preferences-group-add group row))
    (box-append parent-box group)))

(defmethod build-button-bar ((repl app-controller) parent-box window)
  (let ((button-box (horizontal-box)))
    (setf (widget-hexpand-p button-box) t
          (widget-halign button-box) +align-fill+)

    ;; Exit
    (let ((exit-btn (pill-button "Exit" nil)))
      (connect exit-btn "clicked"
               (lambda (button)
                 (declare (ignore button))
                 (window-destroy window)))
      (box-append button-box exit-btn))

    ;; Eval
    (let ((eval-btn (pill-button "Eval" t)))
      (connect eval-btn "clicked"
               (lambda (widget)
                 (declare (ignore widget))
                 (evaluate-expression repl)))
      (box-append button-box eval-btn))

    (box-append parent-box button-box)))

(defmethod build-ui ((repl app-controller) window)
  (let ((root (vertical-box)))
    (setf (adw:window-content window) root)

    ;; Header
    (box-append root (build-header-bar repl))

    ;; Main carousel
    (let ((carousel (adw:make-carousel)))
      (setf (widget-hexpand-p carousel) t
            (widget-vexpand-p carousel) t
            (adw:carousel-interactive-p carousel) t)

      (let ((page (build-status-page repl))
            (content (vertical-box :spacing 12)))
        (setf (widget-margin-top content) 24
              (widget-margin-bottom content) 24
              (widget-margin-start content) 24
              (widget-margin-end content) 24)

        (build-input-area repl content)
        (build-button-bar repl content window)
        (setf (adw:status-page-child page) content)
        (adw:carousel-append carousel page))

      (box-append root carousel))))

;;; =============================================================================
;;; Helper Functions
;;; =============================================================================
(defun vertical-box (&key (spacing 0))
  (make-box :orientation +orientation-vertical+ :spacing spacing))

(defun horizontal-box ()
  (make-box :orientation +orientation-horizontal+ :spacing 0))

(defun pill-button (label &optional suggested-p)
  (let ((btn (make-button :label label)))
    (setf (widget-css-classes btn)
          (cons "pill" (when suggested-p '("suggested-action")))
          (widget-margin-all btn) 10
          (widget-hexpand-p btn) t)
    btn))

;;; =============================================================================
;;; Define Application
;;; =============================================================================
(define-application (:name simple-repl
                     :id "aoforce.libadwaita-example.simple-repl")
  (define-main-window (window (adw:make-application-window :app *application*))
    (setf (adw:style-manager-color-scheme 
           (adw:style-manager-default)) adw:+color-scheme-force-dark+)
    (let ((repl (make-instance 'app-controller)))
      (widget-add-css-class window "devel")
      (setf (widget-size-request window) '(400 600))
      (build-ui repl window)
      (unless (widget-visible-p window)
        (window-present window)))))

;;; =============================================================================
;;; Main
;;; =============================================================================
(defun main ()
  (unless (adw:initialized-p)
    (adw:init))
  (simple-repl))
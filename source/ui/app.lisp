(defpackage #:ui/app
  (:use #:cl #:gtk4
        #:ui/widgets
        #:ui/layouts
        #:ui/controller
        #:ui/builder)
  (:export #:main
           #:start-app
           #:aoforce-app)
  (:documentation "Main renderer application package.

This module includes:
  - Action definitions for business logic
  - Page layout definitions
  - Application entry point

To extend the UI, you can:
  - Define new actions with define-action
  - Create new page layouts with define-page
  - Add new sections to existing pages
  - Create custom section builders

"))

(in-package #:ui/app)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Actions - Business Logic

(define-action :parse-expression (controller entry text)
  "Parse the input text as a Lisp expression."
  (let ((expr (ignore-errors (read-from-string text))))
    (set-state controller :current-expression expr)
    ;; Update validation styling
    (funcall (if expr #'widget-remove-css-class #'widget-add-css-class)
             entry "error")
    expr))

(define-action :evaluate (controller entry)
  "Evaluate the current expression and display the result."
  (declare (ignore entry))
  (let ((expr (get-state controller :current-expression)))
    (when expr
      (let ((result (handler-case (eval expr)
                      (error (err) err))))
        ;; Update status page description with result
        (with-widget controller :main-status
          (lambda (page)
            (setf (adw:status-page-description page) 
                  (princ-to-string result)))))
      ;; Clear state and input
      (set-state controller :current-expression nil)
      (with-widget controller :repl-input-entry
        (lambda (entry)
          (setf (entry-buffer-text (entry-buffer entry)) "")
          (widget-remove-css-class entry "error"))))))

(define-action :exit (controller)
  "Exit the application."
  (with-widget controller :window
    (lambda (window)
      (window-destroy window))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Page Layouts
;;;
;;; The main REPL page layout

(define-page :repl-page (:title "REPL" :icon "utilities-terminal-symbolic")
  ;; Branding/Status section at top
  (status-section :main-status
                  :title "AOFORCE"
                  :description " "
                  :paintable-path "assets/lisp-logo.png")
  
  ;; Input section for REPL
  (input-section :repl-input
                 :prompt (format nil "~A> " 
                                 (or (car (package-nicknames *package*))
                                     (package-name *package*)))
                 :on-changed :parse-expression
                 :on-activate :evaluate
                 :error-class "error")
  
  ;; Action buttons
  (button-section :actions
                  :buttons '((:label "Exit" :action :exit :style nil)
                             (:label "Eval" :action :evaluate :style :suggested))
                  :layout :horizontal))

;;; Example settings page layout (for future expansion)
(define-page :settings-page (:title "Settings" :icon "emblem-system-symbolic")
  (preferences-section :appearance
                       :title "Appearance"
                       :description "Customize the look and feel"
                       :rows '((:type :switch
                                :id :dark-mode-switch
                                :title "Dark Mode"
                                :subtitle "Use dark color scheme"
                                :active t
                                :on-toggled :toggle-dark-mode)
                               (:type :combo
                                :id :theme-combo
                                :title "Theme"
                                :items ("Default" "High Contrast" "Solarized")
                                :selected 0
                                :on-selected :select-theme)))
  
  (preferences-section :editor
                       :title "Editor"
                       :description "REPL configuration"
                       :rows '((:type :entry
                                :id :prompt-entry
                                :title "Custom Prompt"
                                :subtitle "Leave empty for default"
                                :on-activate :set-prompt)
                               (:type :switch
                                :id :auto-eval-switch
                                :title "Auto-Evaluate"
                                :subtitle "Evaluate expressions on Enter"
                                :active t))))

;;; Settings actions (stubs for now)
(define-action :toggle-dark-mode (controller switch state)
  (declare (ignore controller switch))
  (let ((scheme (if state 
                    adw:+color-scheme-force-dark+
                    adw:+color-scheme-force-light+)))
    (setf (adw:style-manager-color-scheme (adw:style-manager-default)) scheme)))

(define-action :select-theme (controller dropdown index)
  (declare (ignore controller dropdown))
  (format t "Theme selected: ~A~%" index))

(define-action :set-prompt (controller entry)
  (declare (ignore controller))
  (let ((text (entry-buffer-text (entry-buffer entry))))
    (format t "Prompt set to: ~A~%" text)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Application Definition
;;;
;;; Forward declaration for the macro-generated function

(declaim (ftype function aoforce-app))

(define-application (:name aoforce-app
                     :id "org.aoforce.app")
    (define-main-window (window (adw:make-application-window :app *application*))
        (let ((controller (make-instance 'app-controller)))
          ;; Register window in controller
          (register-widget controller :window window)

          ;; Build the UI from the page definition
          (build-application-window
           controller window
           (get-page-definition :repl-page)
           :header t
           :use-carousel t
           :window-size '(400 600)
           :style-classes '("devel")
           :color-scheme :force-dark)

          ;; Present the window
          (unless (widget-visible-p window)
            (window-present window)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API

(defun main ()
  "Main entry point for the application."
  (unless (adw:initialized-p)
    (adw:init))
  (aoforce-app))

(defun start-app ()
  "Alias for main."
  (main))

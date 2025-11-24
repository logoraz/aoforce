;;;; controller.lisp - Application state management and business logic
;;;;
;;;; This module provides the application controller which manages:
;;;;   - Application state (key-value store)
;;;;   - Widget registry (named references to widgets)
;;;;   - Action registry (named callbacks/handlers)
;;;;
;;;; The controller is passed to all builders and handlers, providing
;;;; a central coordination point without global state.

(defpackage #:ui/controller
  (:use #:cl #:gtk4)
  ;; Action Registry
  (:export #:*action-registry*
           #:register-action
           #:get-action
           #:invoke-action
           #:clear-actions
           #:define-action)
  ;; Controller Class
  (:export #:app-controller)
  ;; Accessors
  (:export #:window
           #:widgets
           #:state
           #:config)
  ;; Widget Registry
  (:export #:register-widget
           #:get-widget
           #:list-widgets)
  ;; State Managementp
  (:export #:get-state
           #:set-state
           #:update-state
           #:clear-state)
  ;; Configuration  
  (:export #:get-config)
  ;; Utilities
  (:export #:with-widget
           #:with-controller-widgets)
  (:documentation "Application state management and business logic."))

(in-package #:ui/controller)

;;; =============================================================================
;;; Action Registry
;;; =============================================================================
(defvar *action-registry* (make-hash-table :test 'eq)
  "Global registry of action handlers.
Actions are named functions that can be invoked by UI components.")

(defun register-action (name handler)
  "Register an action HANDLER under NAME.
HANDLER should be a function that accepts (controller &rest args)."
  (setf (gethash name *action-registry*) handler)
  name)

(defun get-action (name)
  "Get the handler for action NAME."
  (gethash name *action-registry*))

(defun invoke-action (name controller &rest args)
  "Invoke the action NAME with CONTROLLER and ARGS."
  (let ((handler (get-action name)))
    (if handler
        (apply handler controller args)
        (warn "Action not found: ~A" name))))

(defun clear-actions ()
  "Clear all registered actions."
  (clrhash *action-registry*))

(defmacro define-action (name (controller &rest args) &body body)
  "Define and register an action.
Example:
  (define-action :evaluate (controller)
    (let ((expr (get-state controller :current-expression)))
      ...))"
  `(register-action
    ,name
    (lambda (,controller ,@args)
      ,@body)))

;;; =============================================================================
;;; Application Controller Class
;;; =============================================================================
(defclass app-controller ()
  ((window
    :initarg :window
    :initform nil
    :accessor window
    :documentation "Reference to the main application window.")
   (widgets
    :initform (make-hash-table :test 'eq)
    :accessor widgets
    :documentation "Registry of named widget references.")
   (state
    :initform (make-hash-table :test 'eq)
    :accessor state
    :documentation "Application state key-value store.")
   (config
    :initarg :config
    :initform '()
    :accessor config
    :documentation "Application configuration plist."))
  (:documentation "Central controller for application state and coordination."))

(defmethod print-object ((controller app-controller) stream)
  (print-unreadable-object (controller stream :type t)
    (format stream "~D widgets, ~D state keys"
            (hash-table-count (widgets controller))
            (hash-table-count (state controller)))))

;;; =============================================================================
;;; Widget Registry Methods
;;; =============================================================================
(defgeneric register-widget (controller name widget)
  (:documentation "Register WIDGET under NAME in CONTROLLER."))

(defmethod register-widget ((controller app-controller) name widget)
  "Register WIDGET under NAME for later retrieval."
  (setf (gethash name (widgets controller)) widget)
  widget)

(defgeneric get-widget (controller name)
  (:documentation "Get widget registered under NAME."))

(defmethod get-widget ((controller app-controller) name)
  "Retrieve a widget by NAME, or NIL if not found."
  (gethash name (widgets controller)))

(defun list-widgets (controller)
  "List all registered widget names."
  (loop :for key :being :the hash-keys :of (widgets controller)
        :collect key))

;;; =============================================================================
;;; State Management Methods
;;; =============================================================================
(defgeneric get-state (controller key &optional default)
  (:documentation "Get state value for KEY."))

(defmethod get-state ((controller app-controller) key &optional default)
  "Get state value for KEY, returning DEFAULT if not set."
  (gethash key (state controller) default))

(defgeneric set-state (controller key value)
  (:documentation "Set state value for KEY."))

(defmethod set-state ((controller app-controller) key value)
  "Set state KEY to VALUE."
  (setf (gethash key (state controller)) value))

(defgeneric update-state (controller key fn)
  (:documentation "Update state KEY by applying FN to current value."))

(defmethod update-state ((controller app-controller) key fn)
  "Update state KEY by applying FN to current value.
FN receives the current value and returns the new value."
  (setf (gethash key (state controller))
        (funcall fn (gethash key (state controller)))))

(defun clear-state (controller)
  "Clear all state in CONTROLLER."
  (clrhash (state controller)))

;;; =============================================================================
;;; Configuration Access
;;; =============================================================================
(defgeneric get-config (controller key &optional default)
  (:documentation "Get configuration value."))

(defmethod get-config ((controller app-controller) key &optional default)
  "Get configuration value for KEY."
  (getf (config controller) key default))

;;; =============================================================================
;;; Utility Methods
;;; =============================================================================
(defgeneric with-widget (controller name fn)
  (:documentation "Execute FN with the widget named NAME if it exists."))

(defmethod with-widget ((controller app-controller) name fn)
  "Execute FN with the widget registered as NAME.
FN receives the widget as its argument. Does nothing if widget not found."
  (let ((widget (get-widget controller name)))
    (when widget
      (funcall fn widget))))

;;; =============================================================================
;;; Convenience Macros
;;; =============================================================================
(defmacro with-controller-widgets (controller bindings &body body)
  "Bind multiple widgets from CONTROLLER for use in BODY.
Example:
  (with-controller-widgets ctrl
      ((entry :input-entry)
       (label :status-label))
    (setf (label-text label) (entry-text entry)))"
  (let ((ctrl-var (gensym "CONTROLLER")))
    `(let ((,ctrl-var ,controller))
       (let ,(loop :for (var name) :in bindings
                   :collect `(,var (get-widget ,ctrl-var ,name)))
         ,@body))))

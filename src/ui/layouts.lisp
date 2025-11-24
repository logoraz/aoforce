;;;; layouts.lisp - Declarative layout definitions and page registry
;;;;
;;;; This module provides a declarative way to define UI layouts.
;;;; Pages and sections are defined as data structures, which are then
;;;; built into actual widgets by the builder module.
;;;;
;;;; Design Principles:
;;;;   - Layouts are pure data - no widget instances
;;;;   - Easy to serialize, modify, and compose
;;;;   - Separation between definition and instantiation
;;;;   - Registry pattern for organizing pages

(defpackage #:ui/layouts
  (:use #:cl)
  ;; Section class and accessors
  (:export #:section
           #:section-id
           #:section-type
           #:section-title
           #:section-properties
           #:section-property
           #:make-section)
  ;; Section type constructors
  (:export #:status-section
           #:input-section
           #:button-section
           #:preferences-section
           #:custom-section)
  ;; Page definition
  (:export #:page-definition
           #:page-id
           #:page-title
           #:page-icon
           #:page-sections
           #:page-properties
           #:page-property
           #:make-page-definition)
  ;; Registry
  (:export #:*page-registry*
           #:register-page
           #:get-page-definition
           #:list-pages
           #:clear-page-registry)
  ;; Macros
  (:export #:define-page
           #:define-section)
  (:documentation "Declarative layout definitions and page registry."))

(in-package #:ui/layouts)

;;; =============================================================================
;;; Section Type Definitions
;;; =============================================================================
(defclass section ()
  ((id :initarg :id
       :accessor section-id
       :documentation "Unique identifier for this section.")
   (type :initarg :type
         :accessor section-type
         :documentation "Section type keyword for dispatch.")
   (title :initarg :title
          :initform nil
          :accessor section-title
          :documentation "Optional section title.")
   (properties :initarg :properties
               :initform '()
               :accessor section-properties
               :documentation "Plist of section-specific properties."))
  (:documentation "Base class for all section definitions."))

(defmethod print-object ((section section) stream)
  (print-unreadable-object (section stream :type t)
    (format stream "~A (~A)" (section-id section) (section-type section))))

;;; -----------------------------------------------------------------------------
;;; Section Constructors
;;; -----------------------------------------------------------------------------
(defun make-section (id type &rest properties)
  "Create a section definition with ID, TYPE, and property plist."
  (make-instance 'section
                 :id id
                 :type type
                 :properties properties))

(defun section-property (section key &optional default)
  "Get a property value from SECTION by KEY."
  (getf (section-properties section) key default))

(defun (setf section-property) (value section key)
  "Set a property value on SECTION."
  (setf (getf (section-properties section) key) value))

;;; =============================================================================
;;; Predefined Section Types
;;; =============================================================================
(defun status-section (id &key title description icon-name paintable-path)
  "Define a status/branding section.
Keywords:
  TITLE - Main title text
  DESCRIPTION - Description text
  ICON-NAME - Icon to display
  PAINTABLE-PATH - Path to image file"
  (make-section id :status
                :title title
                :description description
                :icon-name icon-name
                :paintable-path paintable-path))

(defun input-section (id &key title prompt on-changed on-activate 
                              validate-fn error-class)
  "Define an input field section.
Keywords:
  TITLE - Section title
  PROMPT - Input row label/prompt
  ON-CHANGED - Symbol naming action for text changes
  ON-ACTIVATE - Symbol naming action for Enter key
  VALIDATE-FN - Function (text) -> boolean for validation
  ERROR-CLASS - CSS class to apply on validation failure"
  (make-section id :input
                :title title
                :prompt prompt
                :on-changed on-changed
                :on-activate on-activate
                :validate-fn validate-fn
                :error-class (or error-class "error")))

(defun button-section (id &key buttons layout)
  "Define a button bar section.
Keywords:
  BUTTONS - List of button specs: ((:label \"Label\" :action 'action-name :style :suggested) ...)
  LAYOUT - :horizontal (default) or :vertical"
  (make-section id :buttons
                :buttons buttons
                :layout (or layout :horizontal)))

(defun preferences-section (id &key title description rows)
  "Define a preferences group section.
Keywords:
  TITLE - Group title
  DESCRIPTION - Group description
  ROWS - List of row specs: ((:type :entry :title \"Name\" ...) ...)"
  (make-section id :preferences
                :title title
                :description description
                :rows rows))

(defun custom-section (id builder-fn)
  "Define a custom section with a builder function.
BUILDER-FN should be (lambda (controller parent) ...) and should
add widgets to PARENT."
  (make-section id :custom
                :builder-fn builder-fn))

;;; =============================================================================
;;; Page Definition
;;; =============================================================================
(defclass page-definition ()
  ((id :initarg :id
       :accessor page-id
       :documentation "Unique page identifier.")
   (title :initarg :title
          :initform nil
          :accessor page-title
          :documentation "Page title for navigation.")
   (icon :initarg :icon
         :initform nil
         :accessor page-icon
         :documentation "Icon name for navigation.")
   (sections :initarg :sections
             :initform '()
             :accessor page-sections
             :documentation "Ordered list of section definitions.")
   (properties :initarg :properties
               :initform '()
               :accessor page-properties
               :documentation "Additional page properties."))
  (:documentation "Definition of a complete page with sections."))

(defmethod print-object ((page page-definition) stream)
  (print-unreadable-object (page stream :type t)
    (format stream "~A (~D sections)" 
            (page-id page) 
            (length (page-sections page)))))

(defun make-page-definition (id &key title icon sections (properties nil))
  "Create a page definition."
  (make-instance 'page-definition
                 :id id
                 :title title
                 :icon icon
                 :sections sections
                 :properties properties))

(defun page-property (page key &optional default)
  "Get a property from PAGE."
  (getf (page-properties page) key default))

;;; =============================================================================
;;; Page Registry
;;; =============================================================================
(defvar *page-registry* (make-hash-table :test 'eq)
  "Registry mapping page IDs to page definitions.")

(defun register-page (page-definition)
  "Register a PAGE-DEFINITION in the global registry."
  (setf (gethash (page-id page-definition) *page-registry*)
        page-definition)
  page-definition)

(defun get-page-definition (id)
  "Retrieve a page definition by ID."
  (gethash id *page-registry*))

(defun list-pages ()
  "Return a list of all registered page IDs."
  (loop for key being the hash-keys of *page-registry*
        collect key))

(defun clear-page-registry ()
  "Clear all registered pages."
  (clrhash *page-registry*))

;;; =============================================================================
;;; Macro for Defining Pages
;;; =============================================================================
(defmacro define-page (id (&key title icon) &body sections)
  "Define and register a page.
Example:
  (define-page :main-page (:title \"Main\" :icon \"go-home-symbolic\")
    (status-section :header :title \"Welcome\")
    (input-section :repl-input :prompt \">\")
    (button-section :actions :buttons ((...))))"
  `(register-page
    (make-page-definition
     ,id
     :title ,title
     :icon ,icon
     :sections (list ,@sections))))

(defmacro define-section (id type &body properties)
  "Convenience macro for defining sections outside of define-page.
Example:
  (define-section :my-input :input
    :title \"Enter Command\"
    :prompt \">\")"
  `(make-section ,id ,type ,@properties))

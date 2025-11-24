;;;; widgets.lisp - Reusable UI component constructors
;;;;
;;;; This module provides factory functions for creating commonly used
;;;; libadwaita/GTK4 widgets with sensible defaults. All functions return
;;;; configured widget instances ready for use.
;;;;
;;;; Design Principles:
;;;;   - Each constructor returns a fully configured widget
;;;;   - Keyword arguments for customization with sensible defaults
;;;;   - No side effects beyond widget creation
;;;;   - Composable - widgets can be nested freely

(defpackage #:ui/widgets
  (:use #:cl #:gtk4)
  ;; Box Constructors
  (:export #:make-vbox
           #:make-hbox)
  ;; Button constructors
  (:export #:make-pill-button
           #:make-icon-button)
  ;; Layout helpers
  (:export #:make-toolbar-view
           #:make-scrolled-content)
  ;; Preferences widgets
  (:export #:make-pref-group
           #:make-action-row-entry
           #:make-action-row-switch
           #:make-action-row-combo)
  ;; Status/Display
  (:export #:make-branded-status-page)
  ;; Header
  (:export #:make-app-header-bar)
  ;; Utility
  (:export #:set-margins
           #:set-expand)
  (:documentation "Reusable widget constructors for libadwaita UI components."))

(in-package #:ui/widgets)

;;; =============================================================================
;;; Utility Functions
;;; =============================================================================
(defun set-margins (widget &key (all nil) (top nil) (bottom nil) 
                                (start nil) (end nil))
  "Set margins on WIDGET. ALL sets all margins uniformly, 
individual margins override ALL when specified."
  (let ((top-val (or top all 0))
        (bottom-val (or bottom all 0))
        (start-val (or start all 0))
        (end-val (or end all 0)))
    (setf (widget-margin-top widget) top-val
          (widget-margin-bottom widget) bottom-val
          (widget-margin-start widget) start-val
          (widget-margin-end widget) end-val))
  widget)

(defun set-expand (widget &key (horizontal nil) (vertical nil) (both nil))
  "Set expansion properties on WIDGET. BOTH sets both directions."
  (when (or horizontal both)
    (setf (widget-hexpand-p widget) t))
  (when (or vertical both)
    (setf (widget-vexpand-p widget) t))
  widget)

;;; =============================================================================
;;; Box Constructors
;;; =============================================================================
(defun make-vbox (&key (spacing 0))
  "Create a vertical box with optional SPACING between children."
  (make-box :orientation +orientation-vertical+ :spacing spacing))

(defun make-hbox (&key (spacing 0))
  "Create a horizontal box with optional SPACING between children."
  (make-box :orientation +orientation-horizontal+ :spacing spacing))

;;; =============================================================================
;;; Button Constructors
;;; =============================================================================
(defun make-pill-button (label &key (suggested nil) (destructive nil) 
                                    (margin 10) (expand-h t))
  "Create a pill-styled button with LABEL.
Keywords:
  SUGGESTED - If true, apply suggested-action style (highlighted)
  DESTRUCTIVE - If true, apply destructive-action style (red/warning)
  MARGIN - Margin around the button (default 10)
  EXPAND-H - Whether to expand horizontally (default T)"
  (let ((btn (make-button :label label))
        (classes (list "pill")))
    (when suggested
      (push "suggested-action" classes))
    (when destructive
      (push "destructive-action" classes))
    (setf (widget-css-classes btn) classes)
    (set-margins btn :all margin)
    (when expand-h
      (setf (widget-hexpand-p btn) t))
    btn))

(defun make-icon-button (icon-name &key (tooltip nil) (flat t) (circular nil))
  "Create a button with an icon.
Keywords:
  ICON-NAME - The icon name (e.g., 'list-add-symbolic')
  TOOLTIP - Optional tooltip text
  FLAT - If true, apply flat style (default T)
  CIRCULAR - If true, apply circular style"
  (let ((btn (make-button))
        (classes '()))
    (setf (button-icon-name btn) icon-name)
    (when tooltip
      (setf (widget-tooltip-text btn) tooltip))
    (when flat
      (push "flat" classes))
    (when circular
      (push "circular" classes))
    (when classes
      (setf (widget-css-classes btn) classes))
    btn))

;;; =============================================================================
;;; Layout Containers
;;; =============================================================================
(defun make-toolbar-view (&key (top-bar nil) (bottom-bar nil) (content nil))
  "Create an AdwToolbarView with optional TOP-BAR, BOTTOM-BAR, and CONTENT.
This is the recommended container for libadwaita applications."
  (let ((toolbar (adw:make-toolbar-view)))
    (when top-bar
      (adw:toolbar-view-add-top-bar toolbar top-bar))
    (when bottom-bar
      (adw:toolbar-view-add-bottom-bar toolbar bottom-bar))
    (when content
      (setf (adw:toolbar-view-content toolbar) content))
    toolbar))

(defun make-scrolled-content (child &key (policy-h :automatic) 
                                         (policy-v :automatic))
  "Wrap CHILD in a scrolled window.
Keywords:
  POLICY-H - Horizontal scroll policy (:automatic, :always, :never)
  POLICY-V - Vertical scroll policy (:automatic, :always, :never)"
  (let ((scrolled (make-scrolled-window)))
    (setf (scrolled-window-child scrolled) child)
    ;; Convert keyword to GTK policy constant
    (flet ((policy->gtk (p)
             (ecase p
               (:automatic +policy-type-automatic+)
               (:always +policy-type-always+)
               (:never +policy-type-never+))))
      (setf (scrolled-window-policy scrolled) 
            (list (policy->gtk policy-h) (policy->gtk policy-v))))
    scrolled))

;;; =============================================================================
;;; Header Components
;;; =============================================================================
(defun make-app-header-bar (&key (title nil) (subtitle nil)
                                 (show-title t)
                                 (start-widgets nil)
                                 (end-widgets nil))
  "Create an AdwHeaderBar with optional title/subtitle and widgets.
Keywords:
  TITLE - Main title string (defaults to Lisp implementation type)
  SUBTITLE - Subtitle string (defaults to Lisp implementation version)
  SHOW-TITLE - Whether to show the title (default T)
  START-WIDGETS - List of widgets for the start (left) side
  END-WIDGETS - List of widgets for the end (right) side"
  (let ((header (adw:make-header-bar))
        (actual-title (or title (lisp-implementation-type)))
        (actual-subtitle (or subtitle (lisp-implementation-version))))
    (when show-title
      (setf (adw:header-bar-title-widget header)
            (adw:make-window-title :title actual-title
                                   :subtitle actual-subtitle)))
    (dolist (w start-widgets)
      (adw:header-bar-pack-start header w))
    (dolist (w end-widgets)
      (adw:header-bar-pack-end header w))
    header))

;;; =============================================================================
;;; Status/Display Components
;;; =============================================================================
(defun make-branded-status-page (&key (title "AOFORCE")
                                      (description " ")
                                      (icon-name nil)
                                      (paintable-path nil)
                                      (child nil))
  "Create an AdwStatusPage with branding.
Keywords:
  TITLE - Main title text
  DESCRIPTION - Description text below the title
  ICON-NAME - Icon name (mutually exclusive with PAINTABLE-PATH)
  PAINTABLE-PATH - Path to image file for branding
  CHILD - Optional child widget displayed below description"
  (let ((page (adw:make-status-page)))
    (setf (adw:status-page-title page) title
          (adw:status-page-description page) description)
    (cond
      (paintable-path
       (setf (adw:status-page-paintable page) 
             (gdk:make-texture :path paintable-path)))
      (icon-name
       (setf (adw:status-page-icon-name page) icon-name)))
    (when child
      (setf (adw:status-page-child page) child))
    (set-expand page :both t)
    page))

;;; =============================================================================
;;; Preferences Components
;;; =============================================================================
(defun make-pref-group (&key (title nil) (description nil) (margin 10))
  "Create an AdwPreferencesGroup.
Keywords:
  TITLE - Group title
  DESCRIPTION - Group description
  MARGIN - Margin around the group"
  (let ((group (adw:make-preferences-group)))
    (when title
      (setf (adw:preferences-group-title group) title))
    (when description
      (setf (adw:preferences-group-description group) description))
    (set-margins group :all margin)
    group))

(defun make-action-row-entry (&key (title "") 
                                   (subtitle nil)
                                   (placeholder nil)
                                   (on-changed nil)
                                   (on-activate nil))
  "Create an AdwActionRow with an embedded entry field.
Keywords:
  TITLE - Row title (shown as label)
  SUBTITLE - Optional subtitle
  PLACEHOLDER - Placeholder text for the entry
  ON-CHANGED - Callback (lambda (entry text)) for text changes
  ON-ACTIVATE - Callback (lambda (entry)) for Enter key
Returns: (VALUES row entry) - both the row and the entry widget"
  (let ((row (adw:make-action-row))
        (entry (make-entry)))
    (setf (adw:preferences-row-title row) title)
    (when subtitle
      (setf (adw:action-row-subtitle row) subtitle))
    (when placeholder
      (setf (entry-placeholder-text entry) placeholder))
    (setf (widget-valign entry) +align-center+
          (widget-hexpand-p entry) t)
    (when on-changed
      (connect entry "changed"
               (lambda (e)
                 (funcall on-changed e (entry-buffer-text (entry-buffer e))))))
    (when on-activate
      (connect entry "activate" on-activate))
    (adw:action-row-add-suffix row entry)
    (values row entry)))

(defun make-action-row-switch (&key (title "")
                                    (subtitle nil)
                                    (active nil)
                                    (on-toggled nil))
  "Create an AdwActionRow with an embedded switch.
Keywords:
  TITLE - Row title
  SUBTITLE - Optional subtitle
  ACTIVE - Initial state of the switch
  ON-TOGGLED - Callback (lambda (switch active-p)) for state changes
Returns: (VALUES row switch) - both the row and the switch widget"
  (let ((row (adw:make-action-row))
        (switch (make-switch)))
    (setf (adw:preferences-row-title row) title)
    (when subtitle
      (setf (adw:action-row-subtitle row) subtitle))
    (setf (widget-valign switch) +align-center+)
    (when active
      (setf (switch-active-p switch) t))
    (when on-toggled
      (connect switch "state-set"
               (lambda (sw state)
                 (funcall on-toggled sw state)
                 nil)))  ; Return nil to allow default handling
    (adw:action-row-add-suffix row switch)
    (setf (adw:action-row-activatable-widget row) switch)
    (values row switch)))

(defun make-action-row-combo (&key (title "")
                                   (subtitle nil)
                                   (items '())
                                   (selected 0)
                                   (on-selected nil))
  "Create an AdwActionRow with a dropdown/combo.
Keywords:
  TITLE - Row title
  SUBTITLE - Optional subtitle
  ITEMS - List of string items
  SELECTED - Initially selected index
  ON-SELECTED - Callback (lambda (dropdown index)) for selection changes
Returns: (VALUES row dropdown) - both the row and the dropdown widget"
  (let* ((row (adw:make-action-row))
         (string-list (apply #'make-string-list 
                             :strings (or items '(""))))
         (dropdown (make-drop-down :model string-list :expression nil)))
    (setf (adw:preferences-row-title row) title)
    (when subtitle
      (setf (adw:action-row-subtitle row) subtitle))
    (setf (widget-valign dropdown) +align-center+
          (drop-down-selected dropdown) selected)
    (when on-selected
      (connect dropdown "notify::selected"
               (lambda (dd pspec)
                 (declare (ignore pspec))
                 (funcall on-selected dd (drop-down-selected dd)))))
    (adw:action-row-add-suffix row dropdown)
    (values row dropdown)))

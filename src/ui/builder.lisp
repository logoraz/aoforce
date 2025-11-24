;;;; builder.lisp - UI builder that constructs widgets from layout definitions
;;;;
;;;; This module bridges the gap between declarative layouts and actual
;;;; GTK/libadwaita widgets. It provides generic functions that dispatch
;;;; on section types to create the appropriate widgets.
;;;;
;;;; Design Principles:
;;;;   - Generic functions for extensibility
;;;;   - Section type dispatch via CLOS
;;;;   - All created widgets are registered in the controller
;;;;   - Support for nested/recursive building

(defpackage #:ui/builder
  (:use #:cl #:gtk4
        #:ui/widgets
        #:ui/layouts
        #:ui/controller)
  (:export #:build-section
           #:build-page
           #:build-application-window)
  (:documentation "UI builder that constructs UI from layout definitions."))

(in-package #:ui/builder)

;;; =============================================================================
;;; Section Building - Generic Function
;;; =============================================================================
(defgeneric build-section (type section controller parent)
  (:documentation "Build a section of TYPE into PARENT, using CONTROLLER for state.
Returns the created widget(s)."))

;;; =============================================================================
;;; Section Builders - Status Section
;;; =============================================================================
(defmethod build-section ((type (eql :status)) section controller parent)
  "Build a status/branding section."
  (let* ((title (section-property section :title))
         (description (section-property section :description " "))
         (icon-name (section-property section :icon-name))
         (paintable-path (section-property section :paintable-path))
         (page (make-branded-status-page
                :title title
                :description description
                :icon-name icon-name
                :paintable-path paintable-path)))
    ;; Register the status page for later updates
    (register-widget controller (section-id section) page)
    ;; Add to parent if provided
    (when parent
      (box-append parent page))
    page))

;;; =============================================================================
;;; Section Builders - Input Section
;;; =============================================================================
(defmethod build-section ((type (eql :input)) section controller parent)
  "Build an input field section with validation."
  (let* ((title (section-property section :title))
         (prompt (section-property section :prompt "> "))
         (on-changed (section-property section :on-changed))
         (on-activate (section-property section :on-activate))
         (validate-fn (section-property section :validate-fn))
         (error-class (section-property section :error-class "error"))
         (group (make-pref-group :title title)))
    
    (multiple-value-bind (row entry)
        (make-action-row-entry
         :title prompt
         :on-changed (when (or on-changed validate-fn)
                       (lambda (entry text)
                         ;; Validation
                         (when validate-fn
                           (funcall (if (funcall validate-fn text)
                                        #'widget-remove-css-class
                                        #'widget-add-css-class)
                                    entry error-class))
                         ;; Action callback
                         (when on-changed
                           (invoke-action on-changed controller entry text))))
         :on-activate (when on-activate
                        (lambda (entry)
                          (invoke-action on-activate controller entry))))
      ;; Register widgets
      (register-widget controller (section-id section) group)
      (register-widget controller 
                       (intern (format nil "~A-ENTRY" (section-id section)) 
                               :keyword)
                       entry)
      (adw:preferences-group-add group row))
    
    (when parent
      (box-append parent group))
    group))

;;; =============================================================================
;;; Section Builders - Button Section
;;; =============================================================================
(defmethod build-section ((type (eql :buttons)) section controller parent)
  "Build a button bar section."
  (let* ((buttons-spec (section-property section :buttons))
         (layout (section-property section :layout :horizontal))
         (box (if (eq layout :horizontal)
                  (make-hbox)
                  (make-vbox))))
    (setf (widget-hexpand-p box) t
          (widget-halign box) +align-fill+)
    
    (dolist (spec buttons-spec)
      (let* ((label (getf spec :label "Button"))
             (action (getf spec :action))
             (style (getf spec :style))
             (btn (make-pill-button
                   label
                   :suggested (eq style :suggested)
                   :destructive (eq style :destructive))))
        (when action
          (connect btn "clicked"
                   (lambda (button)
                     (declare (ignore button))
                     (invoke-action action controller))))
        (box-append box btn)))
    
    (register-widget controller (section-id section) box)
    (when parent
      (box-append parent box))
    box))

;;; =============================================================================
;;; Section Builders - Preferences Section
;;; =============================================================================
(defmethod build-section ((type (eql :preferences)) section controller parent)
  "Build a preferences group with multiple rows."
  (let* ((title (section-property section :title))
         (description (section-property section :description))
         (rows-spec (section-property section :rows))
         (group (make-pref-group :title title :description description)))
    
    (dolist (row-spec rows-spec)
      (let ((row-type (getf row-spec :type))
            (row-id (getf row-spec :id)))
        (case row-type
          (:entry
           (multiple-value-bind (row entry)
               (make-action-row-entry
                :title (getf row-spec :title "")
                :subtitle (getf row-spec :subtitle)
                :on-changed (let ((action (getf row-spec :on-changed)))
                              (when action
                                (lambda (e text)
                                  (invoke-action action controller e text))))
                :on-activate (let ((action (getf row-spec :on-activate)))
                               (when action
                                 (lambda (e)
                                   (invoke-action action controller e)))))
             (when row-id
               (register-widget controller row-id entry))
             (adw:preferences-group-add group row)))
          
          (:switch
           (multiple-value-bind (row switch)
               (make-action-row-switch
                :title (getf row-spec :title "")
                :subtitle (getf row-spec :subtitle)
                :active (getf row-spec :active)
                :on-toggled (let ((action (getf row-spec :on-toggled)))
                              (when action
                                (lambda (sw state)
                                  (invoke-action action controller sw state)))))
             (when row-id
               (register-widget controller row-id switch))
             (adw:preferences-group-add group row)))
          
          (:combo
           (multiple-value-bind (row dropdown)
               (make-action-row-combo
                :title (getf row-spec :title "")
                :subtitle (getf row-spec :subtitle)
                :items (getf row-spec :items)
                :selected (or (getf row-spec :selected) 0)
                :on-selected (let ((action (getf row-spec :on-selected)))
                               (when action
                                 (lambda (dd idx)
                                   (invoke-action action controller dd idx)))))
             (when row-id
               (register-widget controller row-id dropdown))
             (adw:preferences-group-add group row))))))
    
    (register-widget controller (section-id section) group)
    (when parent
      (box-append parent group))
    group))

;;; =============================================================================
;;; Section Builders - Custom Section
;;; =============================================================================
(defmethod build-section ((type (eql :custom)) section controller parent)
  "Build a custom section using the provided builder function."
  (let ((builder-fn (section-property section :builder-fn)))
    (when builder-fn
      (funcall builder-fn controller parent))))

;;; =============================================================================
;;; Page Building
;;; =============================================================================
(defun build-page (page-def controller &key (container nil) (carousel nil))
  "Build a complete page from PAGE-DEF.
Keywords:
  CONTAINER - If provided, add the page content to this container
  CAROUSEL - If provided, wrap in status page and add to carousel
Returns the built page widget."
  (let ((sections (page-sections page-def)))
    
    (cond
      ;; Carousel mode - build status page with child content
      (carousel
       (let* ((status-sec (find :status sections :key #'section-type))
              (non-status-sections (remove :status sections :key #'section-type))
              ;; Build the status page first (without adding to any parent)
              (status-page (if status-sec
                               (build-section :status status-sec controller nil)
                               (make-branded-status-page)))
              ;; Create child box for non-status content
              (child-box (make-vbox :spacing 12)))
         (set-margins child-box :all 24)
         ;; Build non-status sections directly into child-box
         (dolist (section non-status-sections)
           (build-section (section-type section) section controller child-box))
         ;; Set child-box as status page child
         (setf (adw:status-page-child status-page) child-box)
         ;; Add status page to carousel
         (adw:carousel-append carousel status-page)
         status-page))
      
      ;; Direct container mode or standalone
      (t
       (let ((content-box (make-vbox :spacing 12)))
         (set-margins content-box :all 24)
         ;; Build all sections into the content box
         (dolist (section sections)
           (build-section (section-type section) section controller content-box))
         ;; Add to container if provided
         (when container
           (box-append container content-box))
         content-box)))))

;;; =============================================================================
;;; Application Window Building
;;; =============================================================================
(defun build-application-window (controller window page-def
                                  &key (header t) 
                                       (header-title nil)
                                       (header-subtitle nil)
                                       (use-carousel nil)
                                       (window-size '(400 600))
                                       (style-classes '("devel"))
                                       (color-scheme :force-dark))
  "Build a complete application window from PAGE-DEF.
Keywords:
  HEADER - Whether to include a header bar (default T)
  HEADER-TITLE - Custom header title
  HEADER-SUBTITLE - Custom header subtitle
  USE-CAROUSEL - Wrap content in a carousel
  WINDOW-SIZE - (width height) list
  STYLE-CLASSES - CSS classes to add to window
  COLOR-SCHEME - :default, :force-dark, :force-light, :prefer-dark, :prefer-light"
  
  ;; Set window reference
  (setf (window controller) window)
  
  ;; Apply style classes
  (dolist (class style-classes)
    (widget-add-css-class window class))
  
  ;; Set window size
  (setf (widget-size-request window) window-size)
  
  ;; Set color scheme
  (let ((scheme (ecase color-scheme
                  (:default adw:+color-scheme-default+)
                  (:force-dark adw:+color-scheme-force-dark+)
                  (:force-light adw:+color-scheme-force-light+)
                  (:prefer-dark adw:+color-scheme-prefer-dark+)
                  (:prefer-light adw:+color-scheme-prefer-light+))))
    (setf (adw:style-manager-color-scheme (adw:style-manager-default)) scheme))
  
  ;; Build UI structure
  (let ((root (make-vbox)))
    (setf (adw:window-content window) root)
    
    ;; Header bar
    (when header
      (let ((header-bar (make-app-header-bar
                         :title header-title
                         :subtitle header-subtitle)))
        (register-widget controller :header-bar header-bar)
        (box-append root header-bar)))
    
    ;; Main content
    (if use-carousel
        (let ((carousel (adw:make-carousel)))
          (setf (widget-hexpand-p carousel) t
                (widget-vexpand-p carousel) t
                (adw:carousel-interactive-p carousel) t)
          (register-widget controller :carousel carousel)
          (build-page page-def controller :carousel carousel)
          (box-append root carousel))
        (build-page page-def controller :container root)))
  
  window)

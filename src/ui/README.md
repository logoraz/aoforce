# AOFORCE Modular UI Framework

A declarative, modular UI framework built on top of `cl-gtk4.adw` (libadwaita bindings for Common Lisp).

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────────┐
│                          app.lisp                               │
│  - Application entry point (ui/app)                             │
│  - Action definitions (business logic)                          │
│  - Page layout definitions                                      │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                        builder.lisp                             │
│  - Builds widgets from layout definitions (ui/builder)          │
│  - Section type dispatching                                     │
│  - Window construction                                          │
└─────────────────────────────────────────────────────────────────┘
                              │
              ┌───────────────┼───────────────┐
              ▼               ▼               ▼
┌──────────────────┐ ┌──────────────────┐ ┌──────────────────┐
│   widgets.lisp   │ │   layouts.lisp   │ │ controller.lisp  │
│    ui/widgets    │ │    ui/layouts    │ │  ui/controller   │
│                  │ │                  │ │                  │
│ - Widget factory │ │ - Section types  │ │ - State mgmt     │
│ - Reusable       │ │ - Page registry  │ │ - Widget registry│
│   components     │ │ - Declarative    │ │ - Actions        │
│                  │ │   definitions    │ │                  │
└──────────────────┘ └──────────────────┘ └──────────────────┘
```

## Integration with AOFORCE

These files belong in `src/ui/` and are loaded via the main `aoforce.asd`:

```lisp
(:module "ui"
 :components
 ((:file "widgets")
  (:file "layouts")
  (:file "controller")
  (:file "builder")
  (:file "app")))
```

## Package Structure

Each file defines its own package using `defpackage`:

| File | Package | Purpose |
|------|---------|---------|
| `widgets.lisp` | `ui/widgets` | Widget factory functions |
| `layouts.lisp` | `ui/layouts` | Declarative layout definitions |
| `controller.lisp` | `ui/controller` | State & action management |
| `builder.lisp` | `ui/builder` | Builds UI from layouts |
| `app.lisp` | `ui/app` | Main application entry |

## Module Descriptions

### `widgets.lisp` - Widget Constructors
Provides factory functions for creating configured widgets:

```lisp
(in-package #:ui/widgets)

;; Buttons
(make-pill-button "Click Me" :suggested t)
(make-icon-button "list-add-symbolic" :tooltip "Add item")

;; Containers
(make-vbox :spacing 12)
(make-toolbar-view :top-bar header :content main-area)

;; Preferences
(make-action-row-entry :title "Name" :on-changed #'handle-change)
(make-action-row-switch :title "Enable" :active t)

;; Status
(make-branded-status-page :title "Welcome" :paintable-path "logo.png")
```

### `layouts.lisp` - Declarative Layouts
Define UI structure as data, not code:

```lisp
(in-package #:ui/layouts)

(define-page :my-page (:title "My Page" :icon "go-home-symbolic")
  ;; Status section with branding
  (status-section :header
                  :title "My App"
                  :paintable-path "assets/logo.png")
  
  ;; Input fields
  (input-section :user-input
                 :prompt "Enter command: "
                 :on-activate :process-command)
  
  ;; Preferences group
  (preferences-section :settings
                       :title "Options"
                       :rows ((:type :switch
                               :id :option-1
                               :title "Enable Feature"
                               :on-toggled :toggle-feature)))
  
  ;; Button bar
  (button-section :actions
                  :buttons ((:label "Cancel" :action :cancel)
                            (:label "OK" :action :confirm :style :suggested))))
```

### `controller.lisp` - State Management
Centralized state and widget registry:

```lisp
(in-package #:ui/controller)

;; State management
(set-state controller :user-name "John")
(get-state controller :user-name)

;; Widget registry
(register-widget controller :my-button button)
(get-widget controller :my-button)

;; Actions
(define-action :greet (controller)
  (let ((name (get-state controller :user-name)))
    (format t "Hello, ~A!~%" name)))

(invoke-action :greet controller)
```

### `builder.lisp` - UI Construction
Transforms layout definitions into actual widgets:

```lisp
(in-package #:ui/builder)

;; Build a page
(build-page (get-page-definition :my-page) controller :container box)

;; Build complete window
(build-application-window controller window
                          (get-page-definition :main-page)
                          :use-carousel t
                          :color-scheme :force-dark)
```

## How to Extend

### Adding a New Section Type

1. Define the section constructor in `layouts.lisp`:

```lisp
(defun my-custom-section (id &key title custom-prop)
  (make-section id :my-custom
                :title title
                :custom-prop custom-prop))
```

2. Add the builder method in `builder.lisp`:

```lisp
(defmethod build-section ((type (eql :my-custom)) section controller parent)
  (let* ((title (section-property section :title))
         (custom-prop (section-property section :custom-prop))
         (widget (create-my-custom-widget title custom-prop)))
    (register-widget controller (section-id section) widget)
    (when parent
      (box-append parent widget))
    widget))
```

### Adding a New Page

```lisp
(define-page :new-feature (:title "New Feature" :icon "star-symbolic")
  (status-section :feature-header
                  :title "New Feature"
                  :icon-name "star-new-symbolic")
  
  (custom-section :feature-content
                  (lambda (controller parent)
                    ;; Build custom widgets here
                    (let ((label (make-label :str "Custom content!")))
                      (box-append parent label)))))
```

### Adding New Actions

```lisp
(define-action :save-settings (controller)
  "Save current settings to file."
  (let ((dark-mode (with-widget controller :dark-mode-switch
                     (lambda (sw) (switch-active-p sw)))))
    (save-to-config :dark-mode dark-mode)))
```

## File Structure (within aoforce)

```
aoforce/
├── src/
│   └── ui/
│       ├── widgets.lisp      # ui/widgets package
│       ├── layouts.lisp      # ui/layouts package
│       ├── controller.lisp   # ui/controller package
│       ├── builder.lisp      # ui/builder package
│       └── app.lisp          # ui/app package
└── aoforce.asd
```

## Usage

```lisp
;; Load the system
(asdf:load-system :aoforce)

;; Run the UI
(aoforce:ui)
;; or
(ui/app:main)
```

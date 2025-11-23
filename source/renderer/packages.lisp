;;;; packages.lisp - Package structure documentation
;;;;
;;;; NOTE: Each module defines its own package at the top of its file.
;;;; This file is kept for documentation purposes only.
;;;;
;;;; Package hierarchy (matches source/renderer/ module structure):
;;;;   renderer/widgets    - Reusable widget constructors (widgets.lisp)
;;;;   renderer/layouts    - Declarative page/section definitions (layouts.lisp)
;;;;   renderer/controller - Application state management (controller.lisp)
;;;;   renderer/builder    - UI construction from layouts (builder.lisp)
;;;;   renderer/app        - Main application entry point (app.lisp)
;;;;
;;;; Dependencies (via :use):
;;;;   widgets    -> cl, gtk4
;;;;   layouts    -> cl
;;;;   controller -> cl, gtk4
;;;;   builder    -> cl, gtk4, widgets, layouts, controller
;;;;   app        -> cl, gtk4, widgets, layouts, controller, builder
;;;;
;;;; Load order (handled by ASDF via :serial t):
;;;;   1. widgets.lisp
;;;;   2. layouts.lisp
;;;;   3. controller.lisp
;;;;   4. builder.lisp
;;;;   5. app.lisp

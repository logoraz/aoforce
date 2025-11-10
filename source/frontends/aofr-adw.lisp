(defpackage :frontends/aofr-adw
  (:nicknames :aofr.adw)
  (:use :cl :gtk4)
  (:export #:simple-repl)
  (:documentation "Frontend for GTK4/ADW using cl-gtk4."))
(in-package :frontends/aofr-adw)


;;; =============================================================================
;;; Example code using cl-gtk4
;;; =============================================================================
(define-application (:name simple-repl
                     :id "aoforce.libadwaita-example.simple-repl")
  (define-main-window (window (adw:make-application-window :app *application*))
    (setf (adw:style-manager-color-scheme (adw:style-manager-default)) adw:+color-scheme-force-dark+)
    (let ((expression nil))
      (widget-add-css-class window "devel")
      (setf (widget-size-request window) '(400 600))
      (let ((box (make-box :orientation +orientation-vertical+
                           :spacing 0)))
        (setf (adw:window-content window) box)
        (let ((header-bar (adw:make-header-bar)))
          (setf (adw:header-bar-title-widget header-bar)
                (adw:make-window-title :title (lisp-implementation-type)
                                       :subtitle (lisp-implementation-version)))
          (box-append box header-bar))
        (let ((carousel (adw:make-carousel)))
          (setf (widget-hexpand-p carousel) t
                (widget-vexpand-p carousel) t
                (adw:carousel-interactive-p carousel) t)
          (let ((page (adw:make-status-page)))
            (setf (widget-hexpand-p page) t
                  (widget-vexpand-p page) t
                  ;; (adw:status-page-icon-name page) "utilities-terminal-symbolic"
                  (adw:status-page-paintable page) (gdk:make-texture :path "assets/lisp-logo.png")
                  (adw:status-page-title page) "AOFORCE"
                  (adw:status-page-description page) " ")
            (flet ((eval-expression (widget)
                     (declare (ignore widget))
                     (when expression
                       (setf (adw:status-page-description page)
                             (princ-to-string
                              (handler-case (eval expression)
                                (error (err) err)))))))
              (let ((box (make-box :orientation +orientation-vertical+
                                   :spacing 0)))
                ;;
                (let ((group (adw:make-preferences-group)))
                  (setf (widget-margin-all group) 10)
                  (let ((row (adw:make-action-row)))
                    (setf (adw:preferences-row-title row) 
                          (format nil "~A>" (or (car (package-nicknames *package*))
                                                (package-name *package*))))
                    (let ((entry (make-entry)))
                      (setf (widget-valign entry) +align-center+
                            (widget-hexpand-p entry) t)
                      (connect entry "changed" 
                               (lambda (entry)
                                 (setf expression
                                       (ignore-errors 
                                         (read-from-string 
                                          (entry-buffer-text (entry-buffer entry)))))
                                 (funcall 
                                  (if expression #'widget-remove-css-class
                                      #'widget-add-css-class) entry "error")))
                      (connect entry "activate" #'eval-expression)
                      (adw:action-row-add-suffix row entry))
                    (adw:preferences-group-add group row))
                  (box-append box group))
                ;;
                (let ((carousel-box box)
                      (box (make-box :orientation +orientation-horizontal+
                                     :spacing 0)))
                  (setf (widget-hexpand-p box) t
                        (widget-halign box) +align-fill+)
                  (let ((button (make-button :label "Exit")))
                    (setf (widget-css-classes button) '("pill")
                          (widget-margin-all button) 10
                          (widget-hexpand-p button) t)
                    (connect button "clicked" (lambda (button)
                                                (declare (ignore button))
                                                (window-destroy window)))
                    (box-append box button))
                  (let ((button (make-button :label "Eval")))
                    (setf (widget-css-classes button) '("suggested-action" "pill")
                          (widget-margin-all button) 10
                          (widget-hexpand-p button) t)
                    (connect button "clicked" #'eval-expression)
                    (box-append box button))
                  (box-append carousel-box box))
                (setf (adw:status-page-child page) box)))
            (adw:carousel-append carousel page))
          (box-append box carousel)))
      (unless (widget-visible-p window)
        (window-present window)))))

;;; =============================================================================
;;; Main
;;; =============================================================================
#+nil
(defun main ()
  (unless (adw:initialized-p)
    (adw:init))
  (simple-repl))

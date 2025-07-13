(defpackage :frontends/gtk4-tutorial
  (:nicknames :gtk4-tutorial)
  (:use :cl :cffi)
  (:export #:window-simple
           #:hello-world
           #:button-packing
           #:button-packing-with-builder
           #:drawing-area)
  (:documentation "Frontend for GTK4."))

(in-package :frontends/gtk4-tutorial)


(defun window-simple ()
  (let ((app (make-instance 'gtk:application
                            :application-id "com.logoraz.window-simple")))
    (g:signal-connect app "activate"
                      (lambda (application)
                        (let ((window (make-instance 'gtk:application-window
                                                     :application application
                                                     :title "Window"
                                                     :resizable nil
                                                     :default-width 200
                                                     :default-height 200)))
                          (gtk:widget-show window))))
    (g:application-run app nil)))

(defun hello-world ()
  (let ((app (make-instance 'gtk:application
                            :application-id "com.logoraz.hello-world")))
    (g:signal-connect app "activate"
                      (lambda (application)
                        (let* ((button (make-instance 'gtk:button
                                                      :label "Hello World"))
                               (box (make-instance 'gtk:box
                                                   :orientation :vertical
                                                   :halign :center
                                                   :valign :center))
                               (window (make-instance 'gtk:application-window
                                                      :application application
                                                      :child box
                                                      :title "Window"
                                                      :resizable nil
                                                      :default-width 200
                                                      :default-height 200)))
                          (g:signal-connect window "close-request"
                                            (lambda (window)
                                              (declare (ignore window))
                                              (format t "Ignore close request. Click the button.~%")
                                              gdk:+event-stop+))
                          (g:signal-connect button "clicked"
                                            (lambda (button)
                                              (declare (ignore button))
                                              (format t "Hello World~%")
                                              (gtk:window-destroy window)))
                          (gtk:box-append box button)
                          (gtk:widget-show window))))
    (g:application-run app nil)))

(defun button-packing ()
  (let ((app (make-instance 'gtk:application
                            :application-id "com.logoraz.button-packing")))

    (g:signal-connect app "activate"
                      (lambda (application)
                        (let* ((grid (make-instance 'gtk:grid
                                                    :margin-top 12
                                                    :margin-bottom 12
                                                    :margin-start 12
                                                    :margin-end 12
                                                    :column-homogeneous t
                                                    :column-spacing 6
                                                    :row-homogeneous t
                                                    :row-spacing 6))
                               (window (make-instance 'gtk:window
                                                      :title "Button Packing"
                                                      :application application
                                                      :child grid
                                                      :default-width 320))
                               (button1 (make-instance 'gtk:button
                                                       :label "Button 1"))
                               (button2 (make-instance 'gtk:button
                                                       :label "Button 2"))
                               (button3 (make-instance 'gtk:button
                                                       :label "Quit")))

                          (g:signal-connect button1 "clicked"
                                            (lambda (widget)
                                              (declare (ignore widget))
                                              (format t "Button 1 clicked.~%")))
                          (g:signal-connect button2 "clicked"
                                            (lambda (widget)
                                              (declare (ignore widget))
                                              (format t "Button 2 clicked.~%")))
                          (g:signal-connect button3 "clicked"
                                            (lambda (widget)
                                              (declare (ignore widget))
                                              (gtk:window-destroy window)))
                          ;; Pack and show the widgets
                          (gtk:grid-attach grid button1 0 0 1 1)
                          (gtk:grid-attach grid button2 1 0 1 1)
                          (gtk:grid-attach grid button3 0 1 2 1)
                          (gtk:widget-show window))))

    (g:application-run app nil)))

(defparameter *ui*
  "<?xml version='1.0' encoding='UTF-8'?>
<interface>
  <object id='window' class='GtkWindow'>
    <property name='title'>Button Packing</property>
    <child>
      <object id='grid' class='GtkGrid'>
        <child>
          <object id='button1' class='GtkButton'>
            <property name='label'>Button 1</property>
            <layout>
              <property name='column'>0</property>
              <property name='row'>0</property>
            </layout>
          </object>
        </child>
        <child>
          <object id='button2' class='GtkButton'>
            <property name='label'>Button 2</property>
            <layout>
              <property name='column'>1</property>
              <property name='row'>0</property>
            </layout>
          </object>
        </child>
        <child>
          <object id='quit' class='GtkButton'>
            <property name='label'>Quit</property>
            <layout>
              <property name='column'>0</property>
              <property name='row'>1</property>
              <property name='column-span'>2</property>
            </layout>
          </object>
        </child>
      </object>
    </child>
  </object>
</interface>")

(defun button-packing-with-builder ()
  (let ((app (make-instance 'gtk:application
                            :application-id "com.logoraz.button-packing")))
    (g:signal-connect app "activate"
                      (lambda (application)
                        (let* ((builder (gtk:builder-new-from-string *ui*))
                               (window (gtk:builder-object builder "window"))
                               (button1 (gtk:builder-object builder "button1"))
                               (button2 (gtk:builder-object builder "button2"))
                               (button3 (gtk:builder-object builder "quit")))
                          (setf (gtk:window-application window) application)
                          (g:signal-connect button1 "clicked"
                                            (lambda (widget)
                                              (declare (ignore widget))
                                              (format t "Button 1 clicked.~%")))
                          (g:signal-connect button2 "clicked"
                                            (lambda (widget)
                                              (declare (ignore widget))
                                              (format t "Button 2 clicked.~%")))
                          (g:signal-connect button3 "clicked"
                                            (lambda (widget)
                                              (declare (ignore widget))
                                              (gtk:window-destroy window)))
                          (gtk:widget-show window))))
    (g:application-run app nil)))

(defun print-gesture-info (gesture)
  (let ((name (gtk:event-controller-name gesture))
        (event (gtk:event-controller-current-event gesture))
        (device (gtk:event-controller-current-event-device gesture))
        (state (gtk:event-controller-current-event-state gesture))
        (time (gtk:event-controller-current-event-time gesture)))
    (format t " GtkEventController~%")
    (format t "   name : ~a~%" name)
    (format t "   event : ~a~%" event)
    (format t "   device : ~a~%" device)
    (format t "   state : ~a~%" state)
    (format t "   time : ~a~%" time))
  (let* ((event (gtk:event-controller-current-event gesture))
         (type (gdk:event-event-type event))
         (surface (gdk:event-surface event))
         (device (gdk:event-device event))
         (device-tool (gdk:event-device-tool event))
         (time (gdk:event-time event))
         (display (gdk:event-display event))
         (seat (gdk:event-seat event))
         (sequence (gdk:event-event-sequence event))
         (state (gdk:event-modifier-state event))
         (position (multiple-value-list (gdk:event-position event)))
         (emulated (gdk:event-pointer-emulated event))
         (triggers (gdk:event-triggers-context-menu event))
         (button (gdk:button-event-button event)))
    (format t " GdkEvent~%")
    (format t "    type : ~a~%" type)
    (format t "    surface : ~a~%" surface)
    (format t "    device : ~a~%" device)
    (format t "    device-tool : ~a~%" device-tool)
    (format t "    time : ~a~%" time)
    (format t "    display : ~a~%" display)
    (format t "    seat : ~a~%" seat)
    (format t "    sequence : ~a~%" sequence)
    (format t "    state : ~a~%" state)
    (format t "    position : ~a~%" position)
    (format t "    emulated : ~a~%" emulated)
    (format t "    triggers : ~a~%" triggers)
    (format t "    button : ~a~%" button)))

(defun drawing-area ()
  (let ((app (make-instance 'gtk:application
                            :application-id "com.logoraz.drawing-area"))
        (surface nil) (xstart 0) (ystart 0))
    (g:signal-connect app "activate"
                      (lambda (application)
                        (let* ((area (make-instance 'gtk:drawing-area
                                                    :width-request 200
                                                    :height-request 300))
                               (frame (make-instance 'gtk:frame
                                                     :child area))
                               (window (make-instance 'gtk:application-window
                                                      :application application
                                                      :child frame
                                                      :title "Drawing Area"))
                               (drag (make-instance 'gtk:gesture-drag
                                                    :name "GESTURE DRAG Controller"
                                                    :button 1))
                               (press (make-instance 'gtk:gesture-click
                                                     :name "GESTURE CLICK Controller"
                                                     :button 3)))
                          (gtk:widget-add-controller area drag)
                          (gtk:widget-add-controller area press)
                          (g:signal-connect drag "drag-begin"
                                            (lambda (gesture x y)
                                              (print-gesture-info gesture)
                                              (let ((cr (cairo:create surface)))
                                                (setf xstart x)
                                                (setf ystart y)
                                                (cairo:rectangle cr (- xstart 3) (- ystart 3) 6 6)
                                                (cairo:fill cr)
                                                (cairo:destroy cr)
                                                (gtk:widget-queue-draw area))))
                          (g:signal-connect drag "drag-update"
                                            (lambda (gesture x y)
                                              (print-gesture-info gesture)
                                              (let ((cr (cairo:create surface)))
                                                (cairo:rectangle cr (+ xstart x -3) (+ ystart y -3) 6 6)
                                                (cairo:fill cr)
                                                (cairo:destroy cr)
                                                (gtk:widget-queue-draw area))))
                          (g:signal-connect drag "drag-end"
                                            (lambda (gesture x y)
                                              (print-gesture-info gesture)
                                              (let ((cr (cairo:create surface)))
                                                (cairo:rectangle cr (+ xstart x -3) (+ ystart y -3) 6 6)
                                                (cairo:fill cr)
                                                (cairo:destroy cr)
                                                (gtk:widget-queue-draw area))))
                          (g:signal-connect press "pressed"
                                            (lambda (gesture n x y)
                                              (declare (ignore n x y))
                                              (print-gesture-info gesture)
                                              (let ((cr (cairo:create surface)))
                                                (cairo:set-source-rgb cr 1 1 1)
                                                (cairo:paint cr)
                                                (cairo:destroy cr)
                                                (gtk:widget-queue-draw area))))
                          (gtk:drawing-area-set-draw-func area
                                                          (lambda (widget cr width height)
                                                            (declare (ignore widget width height))
                                                            (cairo:set-source-surface cr surface 0 0)
                                                            (cairo:paint cr)))
                          (g:signal-connect area "resize"
                                            (lambda (area width height)
                                              (declare (ignore width height))
                                              (when surface
                                                (cairo:surface-destroy surface)
                                                (setf surface nil))
                                              (when (gtk:native-surface (gtk:widget-native area))
                                                (setf surface
                                                      (gdk:surface-create-similar-surface
                                                       (gtk:native-surface (gtk:widget-native area))
                                                       :color
                                                       (gtk:widget-width area)
                                                       (gtk:widget-height area)))
                                                (let ((cr (cairo:create surface)))
                                                  (cairo:set-source-rgb cr 1 1 1)
                                                  (cairo:paint cr)
                                                  (cairo:destroy cr)))))
                          (gtk:widget-show window))))
    (g:application-run app nil)))

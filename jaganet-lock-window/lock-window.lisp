(ql:quickload 'ltk)
(use-package 'ltk)
(ql:quickload 'bordeaux-threads)

(defun show-idle-window ()
  (ignore-errors
    (start-wish)
    (set-wm-overrideredirect *tk* 1)
    (set-geometry-xy *tk* 0 0)
    (defparameter image (make-image))
    ;(let ((image (make-image)))
    (image-load image "bg.gif")
    (defparameter canvas (make-instance 'canvas))
      ;(let ((canvas (make-instance 'canvas)))
    (create-image canvas 0 0 :image image)
    (configure canvas :width 1600)
    (configure canvas :height 900)
    (pack canvas)
    (loop
      when (probe-file "active.lck") do
          (loop-finish)
      (sleep 1))
    (exit-wish)))

(defun build-exe ()
  (sb-ext:save-lisp-and-die "lock-window.exe" :toplevel #'show-idle-window
                                              :executable t
                                              :application-type :gui))

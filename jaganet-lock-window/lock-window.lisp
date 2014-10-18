(ql:quickload 'ltk)
(use-package 'ltk)
(ql:quickload 'skippy)

(defun show-idle-window ()
  (ignore-errors
    (start-wish)
    (let* ((img (skippy:load-data-stream "bg.gif"))
           (img-width (skippy:width img))
           (img-height (skippy:height img))
           (win-x (truncate (- (/ (screen-width) 2) (/ img-width 2))))
           (win-y (truncate (- (/ (screen-height) 2) (/ img-height 2)))))

      (on-close *tk* (lambda () (format t "Closed")))
      (set-wm-overrideredirect *tk* 1)
      (set-geometry *tk* img-width img-height win-x win-y)
      (defparameter image (make-image))
      (image-load image "bg.gif")
      (defparameter canvas (make-instance 'canvas))
      (create-image canvas 0 0 :image image)
      (configure canvas :width img-width)
      (configure canvas :height img-height)
      (pack canvas)
      (loop
        when (probe-file "active.lck") do
            (loop-finish)
        (sleep 1))
      (exit-wish))))

(defun build-exe ()
  (sb-ext:save-lisp-and-die "lock-window.exe" :toplevel #'show-idle-window
                                              :executable t
                                              :application-type :gui))

(ql:quickload 'ltk)
(ql:quickload 'bordeaux-threads)

(defun interrupt-thread-by-name (thread-name)
  (handler-case (bt:interrupt-thread (find thread-name
                                           (bt:all-threads)
                                           :test #'string-equal
                                           :key #'bt:thread-name)
                                     #'(lambda ()
                                         (signal 'shutting-down)))
    (type-error () nil)))

(defun show-idle-window ()
  (handler-case
    (with-ltk ()
        (let ((image (make-image)))
          (image-load image "bg.gif")
          (let ((canvas (make-instance 'canvas)))
            (create-image canvas 0 0 :image image)
            (configure canvas :width 1600)
            (configure canvas :height 900)
            (pack canvas))))
    (shutting-down () )))

(defun main ()
  (bt:make-thread #'show-idle-window :name "window")
  (loop
    (if (probe-file "active.lck")
      (interrupt-thread-by-ame "window"))))

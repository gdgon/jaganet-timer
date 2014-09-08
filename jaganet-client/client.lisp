;;;; client.lisp

(in-package #:jaganet-client)

(defun run-tests ()
  (lisp-unit:run-tests :all))

;;; "jaganet-timer" goes here. Hacks and glory await!

(defvar *minutes-allowed** 0)
(defvar *start-time*)
(defvar *end-time*)
(defvar *status* 'stopped)
(defvar *server-address* "127.0.0.1")
(defvar *server-port* 4321)
(defvar *config-file* "config")

(defun interrupt-thread-by-name (thread-name)
  (handler-case (bt:interrupt-thread (find thread-name
                                           (bt:all-threads)
                                           :test #'string-equal
                                           :key #'bt:thread-name)
                                     #'(lambda ()
                                         (signal 'shutting-down)))
    (type-error () nil)))

(defun prompt-to-list (prompt-message)
  (format t prompt-message)
  (multiple-value-list (read)))

(define-condition shutting-down (error)
  ())

(define-condition cannot-read-config-file-error (error)
  ((file :initarg :text :accessor text)))

(defun quit-program ()
  (cl-user::quit))

(defun invoke-quit-program ()
  (invoke-restart 'quit-program))

(defun read-config-from-file (file)
  (restart-case (with-open-file (stream file)
                  (let ((contents (make-string (file-length stream))))
                    (read-sequence contents stream)
                    (read-from-string contents)))
    (try-different-file (new-file)
      :report "Try to open a different config file."
      :interactive (lambda () (prompt-to-list "Enter new filename: "))
      (read-config-from-file new-file))
    (quit-program ()
      :report "Quit the program."
      (quit-program))))

(defun set-config (config)
  (defparameter config (read-config-from-file *config-file*))
  (setf *server-address* (getf config ':server-address)
        *server-port* (getf config ':server-port)))

(defun lock-screen ()
  (format t "Locking screen.~&")
  (bt:make-thread
    (lambda ()
      (loop while (eql *status* 'stopped)
        do (process-desktop "lockScreen" "C:/windows/system32/calc.exe")))
    :name "screen-lock"))

(defun start-session (session-type)
  (setf *status* session-type)
  (start-timer))

(defun stop-session ()
  (setf *last-time-freeze* (get-universal-time)
        *status* 'stopped)
  (lock-screen))

;;; Client commands

(defun add-time (minutes)
  (if (numberp minutes)
    (progn
      (when (eql *status* 'stopped)
        (start-session 'limited-time))
      (defparameter *status* 'limited-time)
      (defparameter *minutes-allowed** (+ *minutes-allowed** minutes))
      (interrupt-thread-by-name "time-end-wait")
      (start-time-end-wait)
      (format t "Added ~a minutes." minutes))
    (error 'type-error :datum minutes :expected-type 'integer)))

(defun stop ()
  (defparameter *status* 'stopped)
  (format t "Stopped.~&")
  (lock-screen))

;;; Network
;; Copied from https://github.com/ciaranbradley/land-of-lisp-chap-12-usocket

(defvar *tcp-stream* nil)

(defun stream-read ()
  "Reads from a usocket connected stream"
  (read (usocket:socket-stream *tcp-stream*)))

(defun stream-print (string)
  "Prints to a usocket connected stream"
  (print string (usocket:socket-stream *tcp-stream*))
  (force-output (usocket:socket-stream *tcp-stream*)))

(defun persistently-connect-to-server ()
  (handler-case
    (progn
      (format t "Connecting to server...~%")
      (setf *tcp-stream* (usocket:socket-connect
                           *server-address* *server-port*)))
    (connection-refused-error () (persistently-connect-to-server))))

;;; Message/command reader

(defun process-message (message)
  (handler-case
    (let ((msg-type (car message))
          (msg-param (cadr message)))
      (cond ((eql msg-type :add-time) (add-time msg-param))
            ((eql msg-type :stop) (stop))))
    (type-error () "Ignore messages that aren't lists."
                (format t "Invalid message received: ~a~%"
                        (write-to-string message)))))

(defun tcp-reader-loop ()
  (handler-case
    (loop
      (process-message (stream-read)))
    (end-of-file () (persistently-connect-to-server))
    (simple-error () (persistently-connect-to-server))
    (shutting-down ())))

(defun start-tcp-reader ()
  (interrupt-thread-by-name "tcp-reader-loop")
  (bt:make-thread #'tcp-reader-loop :name "tcp-reader-loop"))

;;; Timekeeping
(defvar *start-time*)
;;*end-time*
(defvar *seconds-paused* 0)
(defvar *last-time-freeze*)
(defvar *status-before-pause*)

(defun start-timer ()
  (setf *start-time* (get-universal-time)
        *minutes-allowed** 0
        *last-time-freeze* nil
        *seconds-paused* 0))

(defun pause-timer ()
  (unless (or (eql *status* 'paused) (eql *status 'stopped))
    (setf *status-before-pause* *status*)
    (setf *status* 'paused)
    (setf *last-time-freeze* (get-universal-time))))

(defun unpause-timer ()
  (when (eql *status* 'paused)
    (setf *status* *status-before-pause*)
    (setf *seconds-paused* (+ *seconds-paused*
                              (- (get-universal-time) *last-time-freeze*)))))

(defun get-seconds-used ()
  (let ((total-seconds-paused
          (if (or (eql *status* 'paused) (eql *status* 'stopped))
            (+ *seconds-paused* (- (get-universal-time) *last-time-freeze*))
            *seconds-paused*)))
    (- (- (get-universal-time) *start-time*)
       total-seconds-paused)))

(defun format-time (seconds)
  (multiple-value-bind
    (second minute hour date month year day-of-week dts-p tz)
    (decode-universal-time seconds)
   (with-output-to-string (stream)
      (format stream "~2,'0d:~2,'0d:~2,'0d" hour minute second))))

(defun time-end-wait ()
  (handler-case
    (progn
      (loop while (>= (- (* 60 *minutes-allowed**) (get-seconds-used))
                   0)
            do (sleep (* 60 *minutes-allowed**)))
      (stop-session))
    (shutting-down () )))

(defun start-time-end-wait ()
  (bt:make-thread #'time-end-wait :name "time-end-wait"))

;;; Cost calculation/tracking
(defvar *total-cost* 0)
(defvar *cost-per-hour* 10)
(defvar *minimum-cost* nil)
(defun get-total-cost (minutes cost-per-hour &key minimum-cost)
  (let ((cost (* (/ cost-per-hour 60) minutes)))
    (if (and minimum-cost (< cost minimum-cost))
      minimum-cost
      cost)))

;;; GUI

(defun client-window ()
  (start-wish)
    (wm-title *tk* "Jaganet Client")
    (defparameter f
             (make-instance 'ltk:frame
                             :master nil))
    (defparameter time-label
             (make-instance 'ltk:labelframe
                            :master f
                            :width 10
                            :text "Time remaining:"))
    (defparameter time-text
             (make-instance 'ltk:label
                            :master time-label))
    (defparameter cost-label
             (make-instance 'ltk:labelframe
                            :master f
                            :width 10
                            :text "Total cost:"))
    (defparameter cost-text
             (make-instance 'ltk:label
                            :master cost-label))
    (defparameter logout-button
             (make-instance 'ltk:button
                            :master f
                            :text "Logout"))

    (on-close *tk* (lambda () (format t "Closed")))

      (pack f)
      (pack time-label)
      (pack time-text)
      (pack cost-label)
      (pack cost-text)
      (pack logout-button))

(defun update-client-window ()
  (handler-case
    (loop
      (unless (eql *status* 'stopped)
        (progn
          (setf (text time-text)
                (format-time (get-seconds-used)))
          (setf (text cost-text)
                (with-output-to-string (stream)
                  (format stream "~$" (get-total-cost
                                        (/ (get-seconds-used) 60)
                                        *cost-per-hour*
                                        :minimum-cost *minimum-cost*))))))
      (sleep 1))
    (shutting-down () )))

(defun stop-client-window ()
  (handler-case (exit-wish)
    (control-error () nil)))

(defun start-client-window ()
  (stop-client-window)
  (interrupt-thread-by-name "update-client-window")
  (client-window)
  (bt:make-thread #'update-client-window :name "update-client-window"))

;;;

(defun main ()
  (cffi::load-foreign-library "WinLockDll.dll")
  (set-config (read-config-from-file "config"))

  (persistently-connect-to-server *server-address* *server-port*)

  (loop while (not (eql *status* 'quit))
    do (read-and-process-message))

  (quit-program))

;;;; client.lisp

(in-package #:jaganet-client)

(defun run-tests ()
  (lisp-unit:run-tests :all))

;;; "jaganet-timer" goes here. Hacks and glory await!

(defvar *time-remaining* 0)
(defvar *start-time*)
(defvar *end-time*)
(defvar *status* 'new-instance)
(defvar *server-address* "127.0.0.1")
(defvar *server-port* 4321)
(defvar *config-file* "config")

(defun prompt-to-list (prompt-message)
  (format t prompt-message)
  (multiple-value-list (read)))

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
  ;(bt:make-thread
    ;(lambda ()
      ;(loop while (eql *status* 'stopped)
        ;do (process-desktop "lockScreen" "C:/windows/system32/calc.exe")))))
        )

(defun add-time (minutes)
  (if (numberp minutes)
    (progn
      (defparameter *status* 'limited-session)
      (defparameter *time-remaining* (+ *time-remaining* minutes))
      (format t "Added ~a minutes." minutes))
    (error 'type-error :datum minutes :expected-type 'integer)))

(defun stop ()
  (defparameter *status* 'stopped)
  (format t "Stopped.~&")
  (lock-screen))

(defun process-message (message)
  (let ((msg-type (car message))
        (msg-param (cadr message)))
  (cond ((eql msg-type :add-time) (add-time msg-param))
        ((eql msg-type :stop) (stop)))))

(defun read-and-process-message ()
  (process-message (stream-read)))

;;; Timekeeping
(defvar *start-time*)
;;*end-time*
(defvar *seconds-paused* 0)
;;*last-pause-time*
(defvar *status-before-pause*)

(defun start-timer ()
  (setf *start-time* (get-universal-time)))

(defun pause-timer ()
  (unless (eql *status* 'paused)
    (setf *status-before-pause* *status*)
    (setf *status* 'paused)
    (setf *last-pause-time* (get-universal-time))))

(defun unpause-timer ()
  (when (eql *status* 'paused)
    (setf *status* *status-before-pause*)
    (setf *seconds-paused* (- (get-universal-time) *last-pause-time*))))

(defun get-used-seconds ()
  (- (- (get-universal-time) *start-time*)
     *seconds-paused*))

;;; GUI
(defun counter-window ()
  (with-ltk ()
    (wm-title *tk* "Jaganet Client")
    (let* ((f
             (make-instance 'ltk:frame
                             :master nil))
           (time-label
             (make-instance 'ltk:labelframe
                            :master f
                            :width 10
                            :text "Time remaining:"))
           (time-text
             (make-instance 'ltk:label
                            :master time-label))
           (cost-label
             (make-instance 'ltk:labelframe
                            :master f
                            :width 10
                            :text "Total cost:"))
           (cost-text
             (make-instance 'ltk:label
                            :master cost-label))
           (logout-button
             (make-instance 'ltk:button
                            :master f
                            :text "Logout")))
      (pack f)
      (pack time-label)
      (pack time-text)
      (pack cost-label)
      (pack cost-text)
      (pack logout-button))))

(defun main ()
  (cffi::load-foreign-library "WinLockDll.dll")
  (set-config (read-config-from-file "config"))

  (network-setup *server-address* *server-port*)

  (loop while (not (eql *status* 'quit))
    do (read-and-process-message))

  (quit-program))

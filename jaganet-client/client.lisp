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

(defun lock-screen ()
  (format t "Locking screen.")
  (loop while (eql *status* 'stopped)
        do (process-desktop "lockScreen" "C:/windows/system32/calc.exe")))

(defun add-time (minutes)
  (if (numberp minutes)
    (progn
      (defparameter *status* 'limited-session)j
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

(defun main ()
  ;; Workaround where cffi can't find the dll when run as an executable image
  (cffi::load-foreign-library "WinLockDll.dll")

  (network-setup *server-address* *server-port*)

  (loop while (not (eql *status* 'quit))
        do (read-and-process-message))

  (quit))

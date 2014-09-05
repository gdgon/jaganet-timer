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

(define-condition cannot-read-config-file-error (error)
  ((file :initarg :text :accessor text)))

(defun quit-program ()
  (quit))

(defun invoke-quit-program ()
  (invoke-restart 'quit-program))

(defun read-config-from-file (file)
  (restart-case (with-open-file (stream file)
                  (let ((contents (make-string (file-length stream))))
                    (read-sequence contents stream)
                    (read-from-string contents)))
    (quit-program () (quit-program))))

(defun set-config (config)
  (defparameter config (read-config-from-file *config-file*))
  (setf *server-address* (getf config ':server-address)
        *server-port* (getf config ':server-port)))

(defun lock-screen ()
  (format t "Locking screen.")
  (bt:make-thread
    (lambda ()
      (loop while (eql *status* 'stopped)
        do (process-desktop "lockScreen" "C:/windows/system32/calc.exe")))))

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

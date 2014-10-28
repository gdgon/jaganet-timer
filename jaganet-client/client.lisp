;;;; client.lisp

(in-package #:jaganet-client)

(defun run-tests ()
  (lisp-unit:run-tests :all))

;;; "jaganet-timer" goes here. Hacks and glory await!

(defvar *server-address* "127.0.0.1")
(defvar *server-port* 4321)
(defvar *config-file* "config")
(defvar *hostname* (machine-instance))

(defun get-formatted-date ()
  "Returns the date as a string in the format YYYY-MM-DD"
  (multiple-value-bind
        (second minute hour date month year day-of-week dst-p tz)
        (get-decoded-time)
        (concatenate 'string (write-to-string year) "-"
                             (write-to-string month) "-"
                             (write-to-string date))))

(defun get-formatted-time ()
  "Returns the time as a string in the format HH:MM:SS"
  (multiple-value-bind
        (second minute hour date month year day-of-week dst-p tz)
        (get-decoded-time)
        (concatenate 'string (write-to-string hour) ":"
                             (write-to-string minute) ":"
                             (write-to-string second))))
(defun make-session-id ()
  (concatenate 'string (machine-instance) "::"
                       (get-formatted-date) "::"
                       (get-formatted-time)))

;; Session data
(defvar *status* :stopped)
(defvar *session-id* nil)
(defvar *minutes-allowed* 0)
(defvar *start-time* 0)
(defvar *last-time-freeze* 0)
(defvar *seconds-paused* 0)
(defvar *status-before-pause* nil)

;; Cost
(defvar *cost-per-hour* 10)
(defvar *minimum-cost* 5)

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
  (ignore-errors (delete-file "active.lck"))
  (bt:make-thread
    (lambda ()
      (loop while (eql *status* :stopped)
        do (progn (process-desktop "lockScreen" "lock-window.exe")
                  (format t "Locking screen.~&")
                  (sleep 2))))
    :name "screen-lock"))

(defun reset-session ()
  (setf *status* :stopped)
  (setf *session-id* nil)
  (setf *minutes-allowed* 0)
  (setf *start-time* 0)
  (setf *last-time-freeze* 0)
  (setf *seconds-paused* 0)
  (setf *status-before-pause* nil))

(defun start-session (session-type)
  "start-session should be called when changing status from :stopped to open/limited time."
  ;; Unlock the screen
  (open "active.lck" :direction :probe :if-does-not-exist :create)
  ;; Set global variables to 0 or nil
  (reset-session)
  ;; Set session variables, including session-id
  (setf *status* session-type
        *start-time* (get-universal-time)
        *session-id* (make-session-id)))

;;; Client commands

(defun limit-time (minutes)
  (if (not (eql *status* :unpaid))
    (if (numberp minutes)
      (progn
        (if (eql *status* :stopped)
          ;; Start a new session
          (start-session :limited-time)
          ;; Otherwise, continue the session, just change the status
          (defparameter *status* :limited-time))
        (defparameter *minutes-allowed* minutes)
        (format t "Limited to ~a minutes." minutes))
      (error 'type-error :datum minutes :expected-type 'integer))))

(defun add-time (minutes)
  (if (not (eql *status* :unpaid))
    (if (numberp minutes)
      (progn
        (if (eql *status* :stopped)
          ;; Start a new session
          (start-session :limited-time)
          ;; Otherwise, continue the session, just change the status
          (defparameter *status* :limited-time))
        (defparameter *minutes-allowed* (+ *minutes-allowed* minutes))
        (format t "Added ~a minutes." minutes))
      (error 'type-error :datum minutes :expected-type 'integer))))

(defun open-time ()
  (if (not (eql *status* :unpaid))
    (progn
      (if (eql *status* :stopped)
         ;; Start a new session
        (start-session :open-time)
         ;; Otherwise, continue the session, just change the status
        (setf *status* :open-time
              *minutes-allowed* 0))
      (format t "Open time.~%"))))

(defun continue-session (session-data)
  (if (not (eql *status* :unpaid))
    (if (eql *status* :stopped)
      (progn
        (format t "Continuing session: ~a~%" session-data)
        ;(start-session (getf session-data :status))
        (defparameter *status* (getf session-data :status))
        (defparameter *session-id* (getf session-data :session-id))
        (defparameter *minutes-allowed* (getf session-data :minutes-allowed))
        (defparameter *start-time* (getf session-data :start-time))
        (defparameter *last-time-freeze* (getf session-data :last-time-freeze))
        (defparameter *seconds-paused* (getf session-data :seconds-paused))
        (defparameter *status-before-pause* (getf session-data :status-before-pause))
        ))))

(defun stop ()
  "Stops the current session immediately."
  (reset-session)
  (setf *status* :stopped)
  (lock-screen)
  (format t "Stopped!!~%"))

(defun stop-unpaid ()
  "Stops the current session and marks changes the status to :unpaid. If a limited-time session was stopped before all *minutes-allowed* are used, reduce *minutes-allowed* to the current number of minutes used, so the customer will not be charged for the minutes they didn't use."
  (if (or (eql *status* :open-time)
          (eql *status* :limited-time))
    (progn
      ;; If limited time, do not add unused minutes to cost calculation
      (if (eql *status* :limited-time)
        (setf *minutes-allowed* (truncate (/ (get-seconds-used) 60))))
      ;; Freeze and set the status to :unpaid
      (setf *last-time-freeze* (get-universal-time)
            *status* :unpaid)
      (lock-screen)))
  (if (eql *status* :paused)
    ;; If *status-before-pause* is limited time, do not add unused minutes to cost calculation
    (if (eql *status-before-pause* :limited-time)
      (setf *minutes-allowed* (truncate (/ (get-seconds-used) 60))))
    (setf *status* :unpaid)))

(defun collect ()
  (stop-session))

(defun stop-session ()
  (setf *last-time-freeze* (get-universal-time)
        *status* :stopped)
  (lock-screen)
  (format t "Stopped.~%")
  ;; close all running programs

  (sb-ext:run-program "taskkill.exe" '("/F"
                                       "/FI" "imagename ne explorer.exe"
                                       "/FI" "imagename ne lock-window.exe"
                                       "/FI" "imagename ne jaganet.exe"
                                       "/FI" "imagename ne wish.exe")
                      :input nil :output t :search t)

  (sb-ext:run-program "taskkill.exe" '("/F"
                                       "/FI" "imagename ne explorer.exe"
                                       "/FI" "imagename ne lock-window.exe"
                                       "/FI" "imagename ne jaganet*"
                                       "/FI" "imagename ne wish.exe")
                      :input nil :output t :search t)
  ;; clear browser data
  (sb-ext:run-program "cmd.exe" '("/K" "clear-browser-data.bat") :input nil :output t :search t))

(defun shutdown ()
  (format t "Shutting down.~%")
  (sb-ext:run-program "shutdown.exe" '("/p" "/f") :search t))

;;; Network
;; Copied from https://github.com/ciaranbradley/land-of-lisp-chap-12-usocket

(defvar *tcp-stream* nil)
(defvar *connected-to-server* nil)

(defun stream-read ()
  "Reads from a usocket connected stream"
  (read (usocket:socket-stream *tcp-stream*)))

(defun stream-print (string)
  "Prints to a usocket connected stream"
  (print string (usocket:socket-stream *tcp-stream*))
  (force-output (usocket:socket-stream *tcp-stream*)))

(defun try-to-connect-to-server ()
  (format t "Connecting to server...~%")
  (handler-case
      (progn 
	(setf *tcp-stream* nil)
	(setf *tcp-stream* (usocket:socket-connect
			    *server-address* *server-port*))
	(if *tcp-stream*
	    (progn
	      (stream-print `(:new-connection ,(append (get-client-data)
                                                   `(:status ,*status*))))
	      (setf *connected-to-server* t))))
    (connection-refused-error () (setf *connected-to-server* nil))))

(defun network-monitor ()
  "Checks if *connected-to-server* is t. Invokes try-to-connect-to-server if it isn't. Also invokes send-session-state if currently connected."
  (handler-case
      (loop
     (if *connected-to-server*
         ;; Send status update if connected to server
         (send-status-update)
         ;; Try to connect. Update the connection status on the client window if successful.
         (loop
        unless *connected-to-server*
        do
          (try-to-connect-to-server)
          (sleep 1)
          (if *connected-to-server*
              (progn
            (setf (text connection-status-label) "Connected")
            (return)))))
     (sleep 1))
    (shutting-down ())))

(defun ip-byte-array-to-string (ip-byte-array)
  (let ((ip (map 'list #'write-to-string ip-byte-array)))
    (concatenate 'string (pop ip) "." (pop ip) "." (pop ip) "." (pop ip))))

(defun get-client-data ()
  `(:hostname ,*hostname*
    :ip-address ,(or (ignore-errors (usocket:get-local-address *tcp-stream*))
		     nil)))

(defun get-session-state ()
   `(:status ,*status*
      :session-id ,*session-id*
      :cost ,(if (eq *status* :open-time)
		 (get-total-cost)
		 (get-total-cost :minutes *minutes-allowed*))
      :minutes-allowed ,*minutes-allowed*
      :start-time ,*start-time*
      :last-time-freeze ,*last-time-freeze*
      :seconds-used ,(get-seconds-used)
      :seconds-paused ,*seconds-paused*
      :status-before-pause ,*status-before-pause*))

(defun get-status-data ()
  `(:client-data ,(get-client-data) :session-data ,(get-session-state)))

(defun send-status-update ()
  (handler-case
      (stream-print `(:status-update ,(get-status-data)))
    (end-of-file () (progn
		      (setf *connected-to-server* nil)
		      (setf (text connection-status-label) "Disconnected")))
    (simple-stream-error () (progn
			      (setf *connected-to-server* nil)
			      (setf (text connection-status-label) "Disconnected")))))

(defun start-network-monitor ()
  (interrupt-thread-by-name "network-monitor")
  (bt:make-thread #'network-monitor :name "network-monitor"))

;;; Message/command reader

(defun process-message (message)
  (handler-case
      (let ((msg-type (car message))
	    (msg-param (cadr message)))
	(cond ((eql msg-type :add-time) (add-time msg-param))
	      ((eql msg-type :limit-time) (limit-time msg-param))
	      ((eql msg-type :open-time) (open-time))
	      ((eql msg-type :pause) (pause))
	      ((eql msg-type :unpause) (unpause))
	      ((eql msg-type :continue-session) (continue-session msg-param))
	      ((eql msg-type :collect) (collect))
	      ((eql msg-type :stop) (stop))
	      ((eql msg-type :stop-unpaid) (stop-unpaid))
	      ((eql msg-type :shutdown) (shutdown))))
    (type-error () "Ignore messages that aren't lists."
                (format t "Invalid message received: ~a~%"
                        (write-to-string message)))))

(defun tcp-reader-loop ()
  (handler-case
    (loop
       (if *connected-to-server*
       (handler-case
           (process-message (stream-read))
         (end-of-file () (progn
                   (setf *connected-to-server* nil)
                   (setf (text connection-status-label) "Disconnected")
                   ))
         (simple-stream-error () (progn
                       (setf *connected-to-server* nil)
                       (setf (text connection-status-label) "Disconnected")))))
       (sleep 1))
    (shutting-down ())))

(defun start-tcp-reader ()
  (interrupt-thread-by-name "tcp-reader-loop")
  (bt:make-thread #'tcp-reader-loop :name "tcp-reader-loop"))

;;; Timekeeping
(defun pause ()
  (unless (or (eql *status* :paused))
    (setf *status-before-pause* *status*)
    (setf *status* :paused)
    (setf *last-time-freeze* (get-universal-time))))

(defun unpause ()
  (when (eql *status* :paused)
    (setf *status* *status-before-pause*)
    (setf *seconds-paused* (+ *seconds-paused*
                              (- (get-universal-time) *last-time-freeze*)))))

(defun get-seconds-used ()
  (let ((total-seconds-paused
          (if (or (eql *status* :paused)
                  (eql *status* :stopped)
                  (eql *status* :unpaid))
            (+ *seconds-paused* (- (get-universal-time) *last-time-freeze*))
            *seconds-paused*)))
    (- (- (get-universal-time) *start-time*)
       total-seconds-paused)))

(defun format-time (seconds)
  "Given a number of seconds, returns the hours, minutes, and seconds in hh:mm:ss format."
  (if (> seconds 0)
      (let* ((s (mod seconds 60))
	     (m (truncate (mod (/ seconds 60) 60)))
	     (h (truncate (/ (/ seconds 60) 60))))
	(with-output-to-string (stream)
	  (format stream "~2,'0d:~2,'0d:~2,'0d" h m s)))
      ; If seconds is a negative number, just write all zeros
      "00:00:00"))

(defun time-end-wait ()
  (handler-case
    (loop
      (sleep 1)
      (if (and (eql *status* :limited-time)
               (<= (- (* 60 *minutes-allowed*) (get-seconds-used)) -1))
        (stop-unpaid)))
    (shutting-down () )))

(defun start-time-end-wait ()
  (bt:make-thread #'time-end-wait :name "time-end-wait"))

;;; Cost calculation/tracking
(defun get-total-cost (&key minutes)
  (let* ((minutes-to-calculate (if (not (eql minutes nil))
                   minutes
                   (/ (get-seconds-used) 60)))
         (cost (* (/ *cost-per-hour* 60) minutes-to-calculate)))
    (truncate (if (and *minimum-cost* (< cost *minimum-cost*))
                *minimum-cost*
                cost))))

;;; GUI

(defun client-window ()
  (start-wish)
    (wm-title *tk* "Jaganet Client")
    ;(set-wm-overrideredirect *tk* 1)
    (set-geometry *tk* 100 180
                       (- (screen-width) 125)
                       (- (screen-height) 220))
    (defparameter f
             (make-instance 'ltk:frame
                             :master nil))
    (defparameter connection-status-label
             (make-instance 'ltk:label
			    :master f
			    :text "Disconnected"))
    (defparameter status-label
             (make-instance 'ltk:labelframe
                            :master f
                            :text "Status"))
    (defparameter status-text
             (make-instance 'ltk:label
                            :master status-label))
    (defparameter time-label
             (make-instance 'ltk:labelframe
                            :master f
                            :text "Time"))
    (defparameter time-text
             (make-instance 'ltk:label
                            :master time-label))
    (defparameter cost-label
             (make-instance 'ltk:labelframe
                            :master f
                            :text "Total cost"))
    (defparameter cost-text
             (make-instance 'ltk:label
                            :master cost-label))
    (defparameter logout-button
             (make-instance 'ltk:button
                            :master f
                            :text "Logout"))

    (on-close *tk* (lambda () (format t "Closed")))

      (pack f)
      (pack status-label)
      (pack status-text)
      (pack time-label)
      (pack time-text)
      (pack cost-label)
      (pack cost-text)
      (pack connection-status-label)
      (pack logout-button))

(defun update-client-window ()
  (handler-case
    (loop
      (progn
        (if (eql *status* :stopped)
          (setf (text status-text) "Stopped"))
        (if (eql *status* :paused)
          (setf (text status-text) "Paused"))
        (if (eql *status* :limited-time)
          (progn
            (setf (text status-text) "Limited time")
            (setf (text time-text) (format-time (- (* *minutes-allowed* 60)
                                                   (get-seconds-used))))
            (setf (text cost-text)
                  (with-output-to-string (stream)
                    (format stream "~$" (get-total-cost
                                          :minutes *minutes-allowed*))))))
        (if (eql *status* :open-time)
          (progn
            (setf (text status-text) "Open time")
            (setf (text time-text) (format-time (get-seconds-used)))
            (setf (text cost-text)
                  (with-output-to-string (stream)
                    (format stream "~$" (get-total-cost)))))))
      (sleep 1))
    (shutting-down () )))

(defun stop-client-window ()
  (interrupt-thread-by-name "update-client-window")
  (handler-case (exit-wish)
    (control-error () nil)))

(defun start-client-window ()
  (stop-client-window)
  (client-window)
  (bt:make-thread #'update-client-window :name "update-client-window"))

(defun build-exe ()
  (sb-ext:save-lisp-and-die "client.exe" :toplevel #'main
                                              :executable t
                                              :application-type :gui))

;;;

(defun main ()
  (cffi::load-foreign-library "WinLockDll.dll")
  (lock-screen)
  (set-config (read-config-from-file "config"))
  (start-client-window)
  (start-time-end-wait)
  (sleep 1)
  (start-network-monitor)
  (sleep 1)
  (start-tcp-reader)
  (mainloop))

(in-package #:jaganet-server)

(defvar *server-address* usocket::*wildcard-host*)
(defvar *server-port* 4321)
(defvar *clients* (make-hash-table :test #'equalp))
(defvar *streams* nil)

(defmacro make-client-gui-element (key widget-type text client-gui-row &rest extra-arguments)
  `(setf (getf ,client-gui-row ,key)
	 ,(if extra-arguments
	      `(make-instance ,widget-type :master client-list-frame :text ,text ,@extra-arguments)
	      `(make-instance ,widget-type :master client-list-frame :text ,text))))

(defun server-window ()
  "Create the server window."
  (start-wish)

  "Run the ltk mainloop event handler in a separate thread."
  (bt:make-thread #'ltk:mainloop :name "ltk mainloop")

  (wm-title *tk* "Jaganet Server")
  
  ;; Main window frame
  (defparameter f
    (make-instance 'ltk:frame :master nil))

  ;; Frame for client list and labels. Elements will be in a grid.
  (defparameter client-list-frame
    (make-instance 'ltk:frame :master f))

  ;; Labels for the client list. This will be row 0.
  (defparameter label-row ())

  (make-client-gui-element :hostname  'ltk:button "Hostname" label-row)
  (make-client-gui-element :ip-address  'ltk:button "IP Address" label-row)
  (make-client-gui-element :status  'ltk:button "Status" label-row)
  (make-client-gui-element :cost 'ltk:button "Cost" label-row)
  (make-client-gui-element :time-left  'ltk:button "Time Left" label-row)
  (make-client-gui-element :time-used  'ltk:button "Time Used" label-row)
  (make-client-gui-element :start-time  'ltk:button "Start Time" label-row)
  (make-client-gui-element :end-time  'ltk:button "End Time" label-row)
  
  (make-client-gui-element :limit-time  'ltk:button "Limit Time" label-row)
  (make-client-gui-element :open-time  'ltk:button "Open Time" label-row)
  (make-client-gui-element :add-time  'ltk:button "Add Time" label-row)
  (make-client-gui-element :reduce-time  'ltk:button "Reduce Time" label-row)
  (make-client-gui-element :pause-time  'ltk:button "Pause Time" label-row)
  (make-client-gui-element :suspend  'ltk:button "Suspend" label-row)
  (make-client-gui-element :stop-session  'ltk:button "Stop Session" label-row)

  (grid-client-row label-row 0)

  (pack f)
  (pack client-list-frame)
)

(defmacro make-button-send-command (message hostname)
  "Creates a lambda that calls sends-message with a given message and hostname."
  `(lambda ()
     (send-message ,message ,hostname)))

(defun make-client-row (hostname)
  (format t "making client row~%")
  (let ((row '()))
    (make-client-gui-element :hostname  'ltk:label "Hostname" row)
    (make-client-gui-element :ip-address  'ltk:label "IP Address" row)
    (make-client-gui-element :status  'ltk:label "Status" row)
    (make-client-gui-element :cost 'ltk:label "Cost" row)
    (make-client-gui-element :time-left  'ltk:label "Time Left" row)
    (make-client-gui-element :time-used  'ltk:label "Time Used" row)
    (make-client-gui-element :start-time  'ltk:label "Start Time" row)
    (make-client-gui-element :end-time  'ltk:label "End Time" row)

    (make-client-gui-element :limit-time  'ltk:button "Limit Time" row)

    (make-client-gui-element :open-time  'ltk:button "Open Time" row
			     :command (lambda () (send-message '(:open-time) hostname)))

    (make-client-gui-element :add-time  'ltk:button "Add Time" row
			     :command (lambda ()
					(let* ((t2 (make-instance 'ltk:toplevel :master nil))
					       (l2 (make-instance 'ltk:label :master t2 :text "Add minutes"))
					       (s2 (make-instance 'ltk:spinbox :master t2 :text "30"
								  :from 0 :to 600 :increment 30))
					       (b2 (make-instance 'ltk:button :master t2 :text "Ok"
								  :command
								  (lambda ()
								    (send-message `(:add-time ,(parse-integer
											       (text s2)))
										  hostname)
								    (destroy t2)))))
					  (ltk:pack l2)
					  (ltk:pack s2)
					  (ltk:pack b2))))

    (make-client-gui-element :reduce-time  'ltk:button "Reduce Time" row)
    (make-client-gui-element :pause-time  'ltk:button "Pause Time" row)
    (make-client-gui-element :suspend  'ltk:button "Suspend" row)
    (make-client-gui-element :stop-session  'ltk:button "Stop Session" row)
    row))


(defun grid-client-row (client-gui-row row-number)
  "Puts the supplied row to the client list grid."
  (grid (getf client-gui-row :hostname) row-number 0 :padx 5)
  (grid (getf client-gui-row :ip-address) row-number 1 :padx 5)
  (grid (getf client-gui-row :status) row-number 2 :padx 5)
  (grid (getf client-gui-row :cost) row-number 3 :padx 5)
  (grid (getf client-gui-row :time-left) row-number 4 :padx 5)
  (grid (getf client-gui-row :time-used) row-number 5 :padx 5)
  (grid (getf client-gui-row :start-time) row-number 6 :padx 5)
  (grid (getf client-gui-row :end-time) row-number 7 :padx 5)
  (grid (getf client-gui-row :limit-time) row-number 8 :padx 5)
  (grid (getf client-gui-row :open-time) row-number 9 :padx 5)
  (grid (getf client-gui-row :add-time) row-number 10 :padx 5)
  (grid (getf client-gui-row :reduce-time) row-number 11 :padx 5)
  (grid (getf client-gui-row :pause-time) row-number 12 :padx 5)
  (grid (getf client-gui-row :suspend) row-number 13 :padx 5)
  (grid (getf client-gui-row :stop-session) row-number 14 :padx 5)

)

(defun get-time-left (minutes-allowed seconds-used)
  "Returns the number of unused seconds. Returns 0 if minutes-allowed is 0."
  (if (> minutes-allowed 0)
      (- (* 60 minutes-allowed) seconds-used)
      0))

(defun get-end-time (minutes-allowed start-time)
  "Returns the time when the session will end in universal time format."
  (if (and (> minutes-allowed 0) (> start-time 0))
      (+ (* 60 minutes-allowed) start-time)
      0))

;; Networking

(defun get-stream (hostname)
  (getf *streams* (intern hostname 'jaganet-server)))

(defun set-stream (hostname stream)
  (setf (getf *streams* (intern hostname 'jaganet-server)) stream))

(defun remove-stream (hostname)
  (remf *streams* (intern hostname 'jaganet-server)))

(defun send-message (message hostname)
  "Sends a message to the specified host."
  (stream-print message (get-stream hostname)))

;; Copied from https://github.com/ciaranbradley/land-of-lisp-chap-12-usocket
(defun stream-read (stream)
  "Reads from a usocket connected stream"
  (read  stream))

(defun stream-print (string stream)
  "Prints to a usocket connected stream"
  (print string stream)
  (force-output stream))

(defun network-setup (address port)
  (defparameter *socket* (usocket:socket-listen address port :reuseaddress t :reuse-address t)))

(defun stream-handler (stream)
  "Handler for connecting clients. Registers the stream for the new connection, reads messages from the client, then unregisters the stream when the client disconnects."
  (declare (type stream stream))
  (let* ((message (stream-read stream))
	 (new-connection-data (getf message :new-connection)))
    (if new-connection-data
	(progn
	  (let ((hostname (getf new-connection-data :hostname)))
	    ;; First, register the stream with the host name
	    (set-stream hostname stream)
	    ;; Reader loop
	    (loop
	       (handler-case
		   (funcall #'server-message-handler (stream-read stream))
		 (end-of-file () (return))
		 (simple-stream-error () (return))))
	    ;; Unregister the stream after it has disconnected
	    (remove-stream hostname))))))

(defun start-server ()
  "Starts the TCP server."
  (usocket:socket-server *server-address* *server-port* #'stream-handler nil ))

(defun start-server-thread ()
  "Starts the TCP server in a new thread."
  (bt:make-thread #'start-server :name "Server"))

(defun server-message-handler (message)
  "Parses and acts on messages received from clients."
  (handler-case
      (let ((msg-type (car message))
	    (msg-data (cadr message)))
	(cond ((eql msg-type :status-update) (funcall #'status-update-handler msg-data))))
    (type-error () )))

;; Client records

(defun status-update-handler (msg-data)
  "Handles :status-update messages from clients."
  (let* ((client-data (getf msg-data :client-data))
	 (session-data (getf msg-data :session-data))
	 (hostname (getf client-data :hostname))
	 (client-record (gethash hostname *clients*)))
    (if client-record
	(update-client-record hostname client-data session-data)
	(new-client-record client-data session-data))))

(defun update-client-record (hostname client-data session-data)
  "Updates the client record from :status-update messages. Also updates the client row on the server window."
  (setf (getf (gethash hostname *clients*) :client-data) client-data)
  (setf (getf (gethash hostname *clients*) :session-data) session-data)
  (grid-client-row (getf (gethash hostname *clients*) :client-row) 13)
  (update-client-row hostname))

(defun new-client-record (client-data session-data)
  "Makes a new client record. Also makes a new client row on the server window."
  (let ((hostname (getf client-data :hostname)))
    (setf (gethash hostname *clients*) '())
    (setf (getf (gethash hostname *clients*) :client-data) client-data)
    (setf (getf (gethash hostname *clients*) :session-data) session-data)
    (setf (getf (gethash hostname *clients*) :client-row) (make-client-row hostname))
    (grid-client-row (getf (gethash hostname *clients*) :client-row) 13)
    (update-client-row hostname)))

(defun format-time (seconds)
  "Given a number of seconds, returns the hours, minutes, and seconds in hh:mm:ss format."
  (let* ((s (mod seconds 60))
	 (m (truncate (mod (/ seconds 60) 60)))
	 (h (truncate (/ (/ seconds 60) 60))))
    (with-output-to-string (stream)
      (format stream "~2,'0d:~2,'0d:~2,'0d" h m s))))

(defun decode-time-to-string (seconds)
  "Convert time from universal time format to hh:mm:ss."
  (if (> seconds 0)
      (multiple-value-bind
	    (s m h)
	  (decode-universal-time seconds)
	(with-output-to-string (stream)
	  (format stream "~2,'0d:~2,'0d:~2,'0d" h m s)))
      "00:00:00"))

(defun update-client-row (hostname)
  "Looks up the *clients* hash table and updates the client row on the server window."
  (let* ((client-data (getf (gethash hostname *clients*) :client-data))
	 (session-data (getf (gethash hostname *clients*) :session-data))
	 (ip-address (getf client-data :ip-address))
	 (status (getf session-data :status))
	 (minutes-allowed (getf session-data :minutes-allowed))
	 (seconds-used (getf session-data :seconds-used))
	 (cost (getf session-data :cost))
	 (time-left (get-time-left minutes-allowed seconds-used))
	 (time-used seconds-used)
	 (start-time (getf session-data :start-time))
	 (end-time (get-end-time minutes-allowed start-time)))

    (setf (text (getf (getf (gethash hostname *clients*)
			    :client-row)
		      :hostname))
	  hostname)

    (setf (text (getf (getf (gethash hostname *clients*)
			    :client-row)
		      :ip-address))
	  (write-to-string ip-address))

    (setf (text (getf (getf (gethash hostname *clients*)
			    :client-row)
		      :status))
	  (write-to-string status))

    (setf (text (getf (getf (gethash hostname *clients*)
			    :client-row)
		      :cost))
	  cost)

    (setf (text (getf (getf (gethash hostname *clients*)
			    :client-row)
		      :time-left))
	  (format-time time-left))

    (setf (text (getf (getf (gethash hostname *clients*)
			    :client-row)
		      :time-used))
	  (format-time time-used))

    (setf (text (getf (getf (gethash hostname *clients*)
			    :client-row)
		      :start-time))
	  (decode-time-to-string start-time))

    (setf (text (getf (getf (gethash hostname *clients*)
			    :client-row)
		      :end-time))
	  (decode-time-to-string end-time))))
		

(defun main ()
  (server-window)
  (start-server-thread))

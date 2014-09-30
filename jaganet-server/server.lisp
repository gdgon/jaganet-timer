(in-package #:jaganet-server)

(defvar *server-address* usocket::*wildcard-host*)
(defvar *server-port* 4321)
(defvar *clients* (make-hash-table :test #'equalp))
(defvar *streams* nil)

;(defun server-window ()
;  (start-wish)
;  (wm-title *tk* "Jaganet Server")
;  (defparameter f
;    (make-instance 'ltk:frame :master nil))
;  (defparameter client-list-frame
 ;   (make-instance 'ltk:frame :master f))
;  (defparameter 

;; Networking

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
  (declare (type stream stream))
  (let* ((message (stream-read stream))
	 (new-connection-data (getf message :new-connection)))
    (if new-connection-data
	(progn
	  (let ((hostname (getf new-connection-data :hostname)))
	    ;; First, register the stream with the host name
	    (setf (getf *streams* (intern hostname 'jaganet-server)) stream)
	    ;; Reader loop
	    (loop
	       (handler-case
		   (funcall #'server-message-handler (stream-read stream))
		 (end-of-file () (return))
		 (simple-stream-error () (return))))
	    ;; Unregister the stream after it has disconnected
	    (remf *streams* (intern hostname 'jaganet-server)))))))

(defun start-server ()
  (usocket:socket-server *server-address* *server-port* #'stream-handler nil
			 :multi-threading t))

(defun stream-reader (stream message-handler)
  (loop
       (handler-case
	   (funcall #'message-handler (stream-read stream))
	 (end-of-file () (return))
	 (simple-stream-error () (return)))))

(defun server-message-handler (message)
  (handler-case
      (let ((msg-type (car message))
	    (msg-data (cadr message)))
	(cond ((eql msg-type :status-update) (funcall #'status-update-handler msg-data))))
    (type-error () )))

;; Client records

(defun status-update-handler (msg-data)
  (let* ((client-data (getf msg-data :client-data))
	 (session-data (getf msg-data :session-data))
	 (hostname (getf client-data :hostname))
	 (client-record (gethash hostname *clients*)))
    (if client-record
	(update-client-record hostname client-data session-data)
	(new-client-record client-data session-data))))

(defun update-client-record (hostname client-data session-data)
  (setf (getf (gethash hostname *clients*) :client-data) client-data)
  (setf (getf (gethash hostname *clients*) :session-data) session-data))

(defun new-client-record (client-data session-data)
  (let ((hostname (getf client-data :hostname)))
    (setf (gethash hostname *clients*) '())
    (setf (getf (gethash hostname *clients*) :client-data) client-data)
    (setf (getf (gethash hostname *clients*) :session-data) session-data)))

(defun main ()
  (bt:make-thread #'start-server :name "server"))

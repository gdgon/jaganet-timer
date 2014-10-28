(in-package #:jaganet-server)

(defvar *locks* (make-hash-table))

(defvar *client-lock* (bt:make-recursive-lock))
(defvar *stream-lock* (bt:make-recursive-lock))
(defvar *log-lock* (bt:make-recursive-lock))

(defun add-lock (key)
  (setf (gethash key *locks*) (bt:make-recursive-lock)))

(defun get-lock (key)
  ;(gethash key *locks*))
  (cond
    ((eql key :clients) *client-lock*)
    ((eql key :streams) *stream-lock*)
    ((eql key :log) *log-lock*)))

(defvar *server-address* usocket::*wildcard-host*)
(defvar *server-port* 4321)

(defvar *clients* (make-hash-table :test #'equalp))
; *clients* = hash table
;   client = plist
;     :client-data
;     :session-data
;     :client-row
;     :disconnected
(add-lock :clients)

(defvar *streams* (make-hash-table :test #'equalp))
(add-lock :streams)

(add-lock :log)

(defun interrupt-thread-by-name (thread-name)
  (handler-case (bt:interrupt-thread (find thread-name
                                           (bt:all-threads)
                                           :test #'string-equal
                                           :key #'bt:thread-name)
                                     #'(lambda ()
                                         (signal 'shutting-down)))
    (type-error () nil)))

;; Misc
(defun transfer-stop-worker (source target)
  "When a transfer command is sent, waits until the target client has continued the original session then stops the session at the source client."
  (loop
    do
    (if (equalp (get-session-property source :session-id)
                (get-session-property target :session-id))
      (progn
        (send-message '(:stop) source)
        (format t "stop source")
        (return))
      (sleep 1))))

;; Thead management
(define-condition shutting-down (error)
  ())

(defun shutdown ()
  (format t "Shutting down.~%")
  (sb-ext:run-program "shutdown.exe" '("/p" "/f") :search t))

(defun start-transfer-stop-worker (source target)
        (format t "stop source")
  (bt:make-thread
    (lambda ()
      (funcall #'transfer-stop-worker source target))
    :name (concatenate 'string "transfer-stop-" (get-session-property source :session-id))))

;; Accessor functions
(defun get-session-data (hostname)
  (bt:with-recursive-lock-held ((get-lock :clients))
    (getf (gethash hostname *clients*) :session-data)))

(defun set-session-data (hostname session-data)
  (bt:with-recursive-lock-held ((get-lock :clients))
    (setf (getf (gethash hostname *clients*) :session-data) session-data)))

(defun get-session-property (hostname session-property)
  (bt:with-recursive-lock-held ((get-lock :clients))
    (getf (getf (gethash hostname *clients*) :session-data) session-property)))

(defun set-session-property (hostname session-property value)
  (bt:with-recursive-lock-held ((get-lock :clients))
    (setf (getf (getf (gethash hostname *clients*) :session-data) session-property) value)))

(defun get-client-data (hostname)
  (bt:with-recursive-lock-held ((get-lock :clients))
    (getf (gethash hostname *clients*) :client-data)))

(defun set-client-data (hostname client-data)
  (bt:with-recursive-lock-held ((get-lock :clients))
    (setf (getf (gethash hostname *clients*) :client-data) client-data)))

(defun get-client-property (hostname client-property)
  (bt:with-recursive-lock-held ((get-lock :clients))
    (getf (getf (gethash hostname *clients*) :client-data) client-property)))

(defun set-client-property (hostname client-property value)
  (bt:with-recursive-lock-held ((get-lock :clients))
    (setf (getf (getf (gethash hostname *clients*) :client-data) client-property) value)))

(defun set-client-row-text (hostname key text)
  (bt:with-recursive-lock-held ((get-lock :clients))
    (setf (text (getf (getf (gethash hostname *clients*) :client-row) key)) (write-to-string text))))

;; Logging

;; formatted-time,
;; hostname,
;; activity (collect/open-time/limited-time),
;; session-id,
;; time-used (add-time, collect only),
;; payment amount

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

(defun log-row (row)
  "Appends a log entry to <current date>.csv of the payment made."
  (with-recursive-lock-held ((get-lock :log))
    (ensure-directories-exist "logs/")
    (handler-case
      (with-open-file (stream (concatenate 'string "logs/" (get-formatted-date) ".csv")
                              :direction :output
                              :if-exists :append
                              :if-does-not-exist :create)
        (write-csv-row row :stream stream))
      (simple-file-error () (sleep 4)))))

(defun log-payment (hostname session-id time-used cost)
  (let ((row (list (get-formatted-time)
                   hostname
                   "payment"
                   session-id
                   time-used
                   cost)))
    (log-row row)))

(defun log-open-time (hostname session-id)
  (let ((row (list (get-formatted-time)
                   hostname
                   "open-time"
                   session-id
                   nil
                   nil)))
    (log-row row)))

(defun log-add-time (hostname session-id minutes)
  (let ((row (list (get-formatted-time)
                   hostname
                   "add-time"
                   session-id
                   minutes
                   nil)))
    (log-row row)))

(defun log-limit-time (hostname session-id minutes)
  (let ((row (list (get-formatted-time)
                   hostname
                   "limit"
                   session-id
                   minutes
                   nil)))
    (log-row row)))

;; GUI

(defmacro make-client-gui-element (key widget-type text client-gui-row &rest extra-arguments)
  `(setf (getf ,client-gui-row ,key)
     ,(if extra-arguments
          `(make-instance ,widget-type :master client-list-frame :text ,text ,@extra-arguments)
          `(make-instance ,widget-type :master client-list-frame :text ,text))))

(defun server-window ()
  "Create the server window."
  (with-ltk ()
   (wm-title *tk* "Jaganet Server")
   (on-close *tk* (lambda () (exit)))

   ;; Main window frame
   (defparameter f
     (make-instance 'ltk:frame :master nil))

   ;; Frame for client list and labels. Elements will be in a grid.
   (defparameter client-list-frame
     (make-instance 'ltk:frame :master f))

   ;; Labels for the client list. This will be row 0.
   (defparameter label-row ())

   (make-client-gui-element :hostname  'ltk:button "Hostname" label-row
                            :command
                            (lambda ()
                              (rearrange-client-rows #'string< #'key-hostname)))
   (make-client-gui-element :ip-address  'ltk:button "IP Address" label-row
                            :command
                            (lambda ()
                              (rearrange-client-rows #'string< #'key-ip-address)))
   (make-client-gui-element :status  'ltk:button "Status" label-row
                            :command
                            (lambda ()
                              (rearrange-client-rows #'string< #'key-status)))
   (make-client-gui-element :cost 'ltk:button "Cost" label-row
                            :command
                            (lambda ()
                              (rearrange-client-rows #'< #'key-cost)))
   (make-client-gui-element :time-left  'ltk:button "Time Left" label-row
                            :command
                            (lambda ()
                              (rearrange-client-rows #'< #'key-time-left)))
   (make-client-gui-element :time-used  'ltk:button "Time Used" label-row
                            :command
                            (lambda ()
                              (rearrange-client-rows #'< #'key-seconds-used)))
   (make-client-gui-element :start-time  'ltk:button "Start Time" label-row
                            :command
                            (lambda ()
                              (rearrange-client-rows #'< #'key-start-time)))
   (make-client-gui-element :end-time  'ltk:button "End Time" label-row
                            :command
                            (lambda ()
                              (rearrange-client-rows #'< #'key-end-time)))

   ;(make-client-gui-element :limit-time  'ltk:button "Limit Time" label-row)
   ;(make-client-gui-element :open-time  'ltk:button "Open Time" label-row)
   ;(make-client-gui-element :add-time  'ltk:button "Add Time" label-row)
   ;(make-client-gui-element :transfer  'ltk:button "Transfer" label-row)
   ;(make-client-gui-element :pause  'ltk:button "Pause" label-row)
   ;(make-client-gui-element :unpause  'ltk:button "Unpause" label-row)
   ;(make-client-gui-element :collect  'ltk:button "Collect" label-row)
   ;(make-client-gui-element :stop-session  'ltk:button "Stop Session" label-row)

   (grid-label-row label-row 0)

   (pack f)
   (pack client-list-frame)
))

(defmacro make-button-send-command (message hostname)
  "Creates a lambda that calls sends-message with a given message and hostname."
  `(lambda ()
     (send-message ,message ,hostname)))

(defun transfer-session (source target)
  "Transfer session data from one host to another."
  (with-recursive-lock-held ((get-lock :clients))
    (let ((session-data (get-session-data source)))
      ;; Send the continue message to the target
      (send-message `(:continue-session ,session-data) target)
      ;; Stop the source session once the session has been transfered
      (start-transfer-stop-worker source target))))

(defun ask-yn (prompt &key (yes-command nil) (no-command nil) (title "Prompt"))
  (let* ((answer nil)
         (tl (make-instance 'ltk:toplevel :master nil))
         (la (make-instance 'ltk:label :master tl :text prompt))
         (f (make-instance 'frame :master tl))
         (no (make-instance 'ltk:button :master f :text "No"
                            :command
                            (lambda ()
                              (destroy tl))))
         (yes (make-instance 'ltk:button :master f :text "Yes"
                            :command
                           (lambda ()
                             (funcall yes-command)
                             (destroy tl)))))
    (set-geometry-xy tl (screen-mouse-x) (screen-mouse-y))
    (pack la)
    (pack f)
    (grid yes 0 0)
    (grid no 0 1)
    ;(pack yes :anchor :sw)
    ;(pack no :anchor :se)
    answer))

(defun make-client-row (hostname)
  (bt:with-recursive-lock-held ((get-lock :clients))
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

      (make-client-gui-element :limit-time  'ltk:button "Limit Time" row
                   :command (lambda ()
                      (let* ((t2 (make-instance 'ltk:toplevel :master nil))
                             (l2 (make-instance 'ltk:label :master t2 :text "Limit time to"))
                             (s2 (make-instance 'ltk:spinbox :master t2
                                    :text "30"
                                    ;:text (write-to-string
                                            ;(truncate (/ (get-session-property hostname :seconds-used)
                                                       ;60)))
                                    :from 0 :to 600 :increment 30))
                             (b2 (make-instance 'ltk:button :master t2 :text "Ok"
                                    :command
                                    (lambda ()
                                      (let ((minutes (parse-integer (text s2))))
                                        (log-limit-time hostname "sessid" minutes)
                                        (send-message `(:limit-time ,minutes) hostname)
                                        (destroy t2))))))
                        (set-geometry-xy t2 (screen-mouse-x) (screen-mouse-y))
                        (ltk:pack l2)
                        (ltk:pack s2)
                        (ltk:pack b2))))

      (make-client-gui-element :open-time  'ltk:button "Open Time" row
                   :command (lambda ()
                              (log-open-time hostname "sessid")
                              (send-message '(:open-time) hostname)))

      (make-client-gui-element :add-time  'ltk:button "Add Time" row
                   :command (lambda ()
                      (let* ((t2 (make-instance 'ltk:toplevel :master nil))
                             (l2 (make-instance 'ltk:label :master t2 :text "Add minutes"))
                             (s2 (make-instance 'ltk:spinbox :master t2 :text "30"
                                    :from 0 :to 600 :increment 30))
                             (b2 (make-instance 'ltk:button :master t2 :text "Ok"
                                    :command
                                    (lambda ()
                                      (let ((time-to-add (parse-integer (text s2))))
                                        (log-add-time hostname "sessid" time-to-add)
                                        (send-message `(:add-time ,time-to-add)
                                            hostname)
                                      (destroy t2))))))
                        (set-geometry-xy t2 (screen-mouse-x) (screen-mouse-y))
                        (ltk:pack l2)
                        (ltk:pack s2)
                        (ltk:pack b2))))
      (make-client-gui-element :transfer  'ltk:button "Transfer" row
                   :command (lambda ()
                      (let* ((available-hosts nil)
                             (tl (make-instance 'ltk:toplevel :master nil))
                             (la (make-instance 'ltk:label :master tl :text "Transfer"))
                             (lb (make-instance 'ltk:listbox :master tl))
                             (ok (make-instance 'ltk:button :master tl :text "Ok"
                                    :command
                                    (lambda ()
                                        (funcall #'transfer-session hostname
                                                                    (nth (car (listbox-get-selection lb))
                                                                         available-hosts))
                                        (destroy tl)))))

                        (loop for client being the hash-values of *clients*
                              do (setf available-hosts
                                       (push (get-client-data client :hostname)
                                             available-hosts)))

                        (listbox-append lb (loop for h in available-hosts
                                               collect h))
                        (pack la)
                        (pack lb)
                        (pack ok))))

      (make-client-gui-element :pause 'ltk:button "Pause" row
                   :command (lambda () (send-message '(:pause) hostname)))
      (make-client-gui-element :unpause 'ltk:button "Unpause" row
                   :command (lambda () (send-message '(:unpause) hostname)))

      (make-client-gui-element :collect  'ltk:button "Collect" row
                   :command (lambda ()
                              (let* ((prompt (concatenate 'string "Collect payment from " hostname "?")))
                                (ask-yn prompt
                                        :yes-command
                                        (lambda ()
                                          (if (eql (get-session-property hostname :status) :open-time)
                                            (send-message '(:collect) hostname)
                                            ;; In case a limited-time session has remaining minutes, let the client
                                            ;; recalculate the cost minus the unused minutes
                                            (progn
                                              (send-message '(:stop-unpaid) hostname)
                                              (loop
                                                until (eql (get-session-property hostname :status) :unpaid)
                                                do (sleep 0.2))
                                              (send-message '(:collect) hostname)))

                                            (log-payment hostname
                                                       "sessid"
                                                       ;; Get the number of minutes used
                                                       (truncate (/ (get-session-property hostname :seconds-used) 60))
                                                       (get-session-property hostname :cost)))))))
      (make-client-gui-element :stop-session  'ltk:button "Stop Session" row
                               :command
                               (lambda ()
                                 (let* ((prompt (concatenate 'string "Stop session on " hostname "?")))
                                   (ask-yn prompt
                                           :yes-command
                                           (lambda ()
                                             (send-message '(:stop-unpaid) hostname))))))

      (make-client-gui-element :shutdown  'ltk:button "Shutdown" row
                               :command
                               (lambda ()
                                 (let* ((prompt (concatenate 'string "Shutdown " hostname "?")))
                                   (ask-yn prompt
                                           :yes-command
                                           (lambda ()
                                             (send-message '(:shutdown) hostname))))))
      row)
    ))

(defun grid-label-row (label-gui-row row-number)
  "Puts the supplied row to the label list grid."
  (grid (getf label-gui-row :hostname) row-number 0 :padx 1)
  (grid (getf label-gui-row :ip-address) row-number 1 :padx 1)
  (grid (getf label-gui-row :status) row-number 2 :padx 1)
  (grid (getf label-gui-row :cost) row-number 3 :padx 1)
  (grid (getf label-gui-row :time-left) row-number 4 :padx 1)
  (grid (getf label-gui-row :time-used) row-number 5 :padx 1)
  (grid (getf label-gui-row :start-time) row-number 6 :padx 1)
  (grid (getf label-gui-row :end-time) row-number 7 :padx 1)
  (grid (getf label-gui-row :end-time) row-number 7 :padx 1)
)

(defun grid-client-row (client-gui-row row-number)
  "Puts the supplied row to the client list grid."
  (grid (getf client-gui-row :hostname) row-number 0 :padx 1)
  (grid (getf client-gui-row :ip-address) row-number 1 :padx 1)
  (grid (getf client-gui-row :status) row-number 2 :padx 1)
  (grid (getf client-gui-row :cost) row-number 3 :padx 1)
  (grid (getf client-gui-row :time-left) row-number 4 :padx 1)
  (grid (getf client-gui-row :time-used) row-number 5 :padx 1)
  (grid (getf client-gui-row :start-time) row-number 6 :padx 1)
  (grid (getf client-gui-row :end-time) row-number 7 :padx 1)
  (grid (getf client-gui-row :limit-time) row-number 8 :padx 1)
  (grid (getf client-gui-row :open-time) row-number 9 :padx 1)
  (grid (getf client-gui-row :add-time) row-number 10 :padx 1)
  (grid (getf client-gui-row :transfer) row-number 11 :padx 1)
  (grid (getf client-gui-row :pause) row-number 12 :padx 1)
  (grid (getf client-gui-row :unpause) row-number 13 :padx 1)
  (grid (getf client-gui-row :collect) row-number 14 :padx 1)
  (grid (getf client-gui-row :stop-session) row-number 15 :padx 1)
  (grid (getf client-gui-row :shutdown) row-number 16 :padx 1)
)

(defun hash-table-alist (table)
    "Returns an association list containing the keys and values of hash table TABLE."
      (let ((alist nil))
            (maphash (lambda (k v)
                       (push (cons k v) alist))
                     table)
            alist))

(defun key-hostname (c)
  (getf (getf (cdr c) :client-data) :hostname))

(defun key-ip-address (c)
  (write-to-string (getf (getf (cdr c) :client-data) :ip-address)))

(defun key-status (c)
  (write-to-string (getf (getf (cdr c) :session-data) :status)))

(defun key-cost (c)
  (getf (getf (cdr c) :session-data) :cost))

(defun key-time-left (c)
  (let ((hostname (key-hostname c)))
    (get-time-left (get-session-property hostname :minutes-allowed)
                   (get-session-property hostname :seconds-used))))

(defun key-seconds-used (c)
  (getf (getf (cdr c) :session-data) :seconds-used))

(defun key-start-time (c)
  (getf (getf (cdr c) :session-data) :start-time))

(defun key-end-time (c)
  (let ((hostname (key-hostname c)))
    (get-end-time (get-session-property hostname :minutes-allowed)
                  (get-session-property hostname :start-time)
                  (get-session-property hostname :seconds-paused))))

(defun rearrange-client-rows (predicate key)
  (bt:with-recursive-lock-held ((get-lock :clients))
    (let ((i 1)
          (arranged-client-alist (sort (hash-table-alist *clients*)
                                       predicate
                                       :key key)))
      (dolist (c arranged-client-alist)
        (grid-client-row (getf (cdr c) :client-row) i)
        (setf i (1+ i))))))

(defun get-time-left (minutes-allowed seconds-used)
  "Returns the number of unused seconds. Returns 0 if minutes-allowed is 0."
  (if (> minutes-allowed 0)
      (- (* 60 minutes-allowed) seconds-used)
      0))

(defun get-end-time (minutes-allowed start-time seconds-paused)
  "Returns the time when the session will end in universal time format."
  (if (and (> minutes-allowed 0) (> start-time 0))
      (+ (* 60 minutes-allowed) start-time seconds-paused)
      0))

;; Networking

(defun get-stream (hostname)
  (bt:with-recursive-lock-held ((get-lock :streams))
    (gethash hostname *streams*)))

(defun set-stream (hostname stream)
  (bt:with-recursive-lock-held ((get-lock :streams))
    (ignore-errors (setf (gethash hostname *streams*) stream))))

(defun remove-stream (hostname)
  (bt:with-recursive-lock-held ((get-lock :streams))
    (ignore-errors (remhash hostname *streams*))))

(defun send-message (message hostname)
  "Sends a message to the specified host."
    (ignore-errors (stream-print message (get-stream hostname))))

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
    (format t "New connection~%") ;debug
    (if new-connection-data
      (progn
        (format t "New conn data ~a~%" new-connection-data) ;debug
        (let ((hostname (getf new-connection-data :hostname))
              (status (getf new-connection-data :status)))
          ;; First, register the stream with the host name
          (set-stream hostname stream)

          ;; Check if the client is reconnecting
          (if (get-client-property hostname :disconnected)
            (progn
            ;; If the client that is reconnecting has rebooted
            ;; (status is :stopped), continue the session as paused
            (if (eql status :stopped)
              (let ((session-data (get-session-data hostname)))
                ;; If the client rebooted in :open-time or :limited-time,
                ;; set the pause-related variables.
                (if (or (eql (get-session-property hostname :status) :open-time)
                        (eql (get-session-property hostname :status) :limited-time))
                  (setf  ;; status-before pause
                        (getf session-data :status-before-pause)
                        (get-session-property hostname :status)

                        ;; last-time-freeze
                        (getf session-data :last-time-freeze)
                        (get-client-property hostname :disconnect-time)

                        ;; set status to paused
                        (getf session-data :status)
                        :paused)
                  )
                ;; Continue the session
                (send-message `(:continue-session ,session-data) hostname)))
              ;; Finally, set the disconnection properties to nil
              (set-client-property hostname :disconnected nil)
              (set-client-property hostname :disconnect-time nil)))

          ;; Reader loop
          (loop
             (handler-case
               (funcall #'server-message-handler (stream-read stream))
               (end-of-file () (return))
               (simple-stream-error () (return))))
          ;; Unregister the stream after it has disconnected
          (remove-stream hostname)
          ;; Mark it as disconnected and note the time
          (set-client-property hostname :disconnected t)
          (set-client-property hostname :disconnect-time (get-universal-time))
          ;(setf (getf (gethash hostname *clients*) :disconnected) t)
          ;(setf (getf (gethash hostname *clients*) :disconnect-time) (get-universal-time))
          ;; Update the client row to show the status as disconnected
          (update-client-row hostname))))))

(defun start-server ()
  "Starts the TCP server."
  (handler-case
    (usocket:socket-server *server-address* *server-port* #'stream-handler nil :multi-threading t)
    (shutting-down ())))

(defun start-server-thread ()
  "Starts the TCP server in a new thread."
  (bt:make-thread #'start-server :name "Server"))

(defun server-message-handler (message)
  "Parses and acts on messages received from clients."
  (handler-case
      (let ((msg-type (car message))
        (msg-data (cadr message)))
    (cond ((eql msg-type :status-update) (status-update-handler msg-data))))
    (type-error () )))

;; Client records

(defun status-update-handler (msg-data)
  "Handles :status-update messages from clients."
  (let* ((client-data (getf msg-data :client-data))
     (session-data (getf msg-data :session-data))
     (hostname (getf client-data :hostname))
     (client-record (gethash hostname *clients*)))
    (if client-record
      ;; If the client is reconnecting, set :disconnected to nil
      (update-client-record hostname client-data session-data)
      (new-client-record client-data session-data))))

(defun update-client-record (hostname client-data session-data)
  "Updates the client record from :status-update messages. Also updates the client row on the server window."
  ;(format t "Update client record for host: ~a~%" hostname) ;debug
  ;(setf (getf (gethash hostname *clients*) :client-data) client-data)
  (set-client-data hostname client-data)
  ;(setf (getf (gethash hostname *clients*) :session-data) session-data)
  (set-session-data hostname session-data)
  (update-client-row hostname))

(defun new-client-record (client-data session-data)
  "Makes a new client record. Also makes a new client row on the server window."
  (bt:with-recursive-lock-held ((get-lock :clients))
     (let ((hostname (getf client-data :hostname)))
       (format t "New client record for host: ~a~%" hostname) ;debug
       (setf (gethash hostname *clients*) '())
       (setf (getf (gethash hostname *clients*) :client-data) client-data)
       (setf (getf (gethash hostname *clients*) :session-data) session-data)
       (setf (getf (gethash hostname *clients*) :client-row) (make-client-row hostname))
       (rearrange-client-rows #'string< #'key-hostname)
       (update-client-row hostname))))

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
  (bt:with-recursive-lock-held ((get-lock :clients))
    (let* ((client-data (getf (gethash hostname *clients*) :client-data))
         (session-data (getf (gethash hostname *clients*) :session-data))
           (ip-address (getf client-data :ip-address))
           (status (if (get-client-property hostname :disconnected)
                     :disconnected
                     (getf session-data :status)))
           (minutes-allowed (getf session-data :minutes-allowed))
           (seconds-used (getf session-data :seconds-used))
           (cost (getf session-data :cost))
           (time-left (get-time-left minutes-allowed seconds-used))
           (time-used seconds-used)
           (start-time (getf session-data :start-time))
           (end-time (get-end-time minutes-allowed start-time (get-session-property hostname :seconds-paused))))

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
        (decode-time-to-string end-time)))))

(defun exit ()
  (interrupt-thread-by-name "Server")
  (destroy *tk*)
  (setf *exit-mainloop* t))

(defun build-exe ()
  (sb-ext:save-lisp-and-die "server.exe" :toplevel #'main
                                         :executable t))
                                         ;:application-type :gui))

(defun main ()
  (start-server-thread)
  (server-window))
  ;(mainloop))
  ;(bt:make-thread #'ltk:mainloop :name "ltk mainloop")

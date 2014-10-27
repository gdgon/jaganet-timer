;;;; network.lisp

(in-package #:jaganet-server)

;; Copied from https://github.com/ciaranbradley/land-of-lisp-chap-12-usocket

(defvar *stream*)

(defun stream-read (stream)
  "Reads from a usocket connected stream"
  (read (usocket:socket-stream stream)))

(defun stream-print (string)
  "Prints to a usocket connected stream"
  (print string (usocket:socket-stream *stream*))
  (force-output (usocket:socket-stream *stream*)))

(defun network-setup (address port)
  (defparameter *socket* (usocket:socket-listen address port :reuseaddress t :reuse-address t)))

(defun network-listen ()
  (defparameter *stream* (usocket:socket-accept *socket*)))

;;;; network.lisp

(in-package #:jaganet-client)

;; Copied from https://github.com/ciaranbradley/land-of-lisp-chap-12-usocket

(defvar *stream*)

(defun stream-read ()
  "Reads from a usocket connected stream"
  (read (usocket:socket-stream *stream*)))

(defun stream-print (string)
  "Prints to a usocket connected stream"
  (print string (usocket:socket-stream *stream*))
  (force-output (usocket:socket-stream *stream*)))

(defun network-setup (address port)
  (defparameter *stream* (usocket:socket-connect address port)))

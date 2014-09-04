(in-package #:jaganet-server)

(defvar *server-address* "127.0.0.1")
(defvar *server-port* 4321)

(defun main ()
  (network-setup *server-address* *server-port*)
  (network-listen))

(in-package #:jaganet-server)

(defvar *server-address* usocket::*wildcard-host*)
(defvar *server-port* 4321)

(defun main ()
  (network-setup *server-address* *server-port*)
  (network-listen))

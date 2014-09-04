;;;; jaganet-server.asd

(asdf:defsystem #:jaganet-server
  :serial t
  :description "Describe jaganet-timer here"
  :author "Gerry Gonzaga <g@gdgon.com>"
  :license "Specify license here"
  :depends-on (:ltk :lisp-unit :usocket)
  :components ((:file "package")
               (:file "network")
               (:file "server" :depends-on ("network"))
               (:file "tests" :depends-on ("server"))))

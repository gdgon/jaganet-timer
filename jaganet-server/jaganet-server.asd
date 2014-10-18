;;;; jaganet-server.asd

(asdf:defsystem #:jaganet-server
  :serial t
  :description "Describe jaganet-timer here"
  :author "Gerry Gonzaga <g@gdgon.com>"
  :license "Specify license here"
  :depends-on (:ltk :lisp-unit :usocket :bordeaux-threads :cl-csv)
  :components ((:file "package")
               (:file "server")
               (:file "tests" :depends-on ("server"))))

;;;; jaganet-client.asd

(asdf:defsystem #:jaganet-client
  :serial t
  :description "Jaganet-timer client"
  :author "Gerry Gonzaga <g@gdgon.com>"
  :license "Specify license here"
  :depends-on (:ltk :cffi :lisp-unit :usocket)
  :components ((:file "package")
               (:file "winlock")
               (:file "network")
               (:file "client" :depends-on ("winlock" "network"))
               (:file "tests" :depends-on ("client"))))


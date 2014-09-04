;;;; jaganet-client.asd

(asdf:defsystem #:jaganet-client
  :serial t
  :description "Jaganet-timer client"
  :author "Gerry Gonzaga <g@gdgon.com>"
  :license "Specify license here"
  :depends-on (:ltk :cffi :lisp-unit)
  :components ((:file "client")
               (:file "tests")
               (:file "winlock")))


;;;; jaganet-client.asd

(asdf:defsystem #:jaganet-client
  :serial t
  :description "Jaganet-timer client"
  :author "Gerry Gonzaga <g@gdgon.com>"
  :license "BSD"
  :depends-on (:ltk :cffi :lisp-unit :usocket :bordeaux-threads)
  :components ((:file "package")
               (:file "winlock")
               (:file "client" :depends-on ("winlock"))
               (:file "tests" :depends-on ("client"))))


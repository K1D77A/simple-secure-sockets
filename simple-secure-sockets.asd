(in-package #:asdf-user)
(asdf:defsystem "simple-secure-sockets"
  :version      "0.1.0"
  :description  "A simple encrypted network protocol between client and servers"
  :author       "K1D77A"
  :serial       t
  :license      "MIT"
  :pathname "src"
  :components   ((:file "package")
                 (:file "helpers")
                 (:file "classes")
                 (:file "generics")
                 (:file "packets")
                 (:file "client")
                 (:file "server"))
  :depends-on   (#:usocket
                 #:lparallel
                 #:ironclad))


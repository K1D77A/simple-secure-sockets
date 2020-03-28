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
                 (:file "classes/connections")
                 (:file "classes/conditions")
                 (:file "classes/packets")
                 (:file "generics")
                 (:file "packets/fsm")
                 (:file "packets/variables")
                 (:file "packets/fsm-parser")
                 (:file "packets/receive")
                 (:file "packets/send")
                 (:file "packets/packet-handlers")
                 (:file "client")
                 (:file "server")
                 (:file "tests"))
  :depends-on   (#:usocket
                 #:lparallel
                 #:closer-mop
                 #:ironclad))


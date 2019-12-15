(in-package :simple-secure-sockets)


(defgeneric connect (connection)
  (:documentation "Connections the instance of either client/server. If client it will connection to a listening server, if server it will set it up to start accepting connections"))
(defgeneric packet-download-function (connection)
  (:documentation "This method is set for a client/server and is used to handle downloading packets that are sent over the connection"))
(defgeneric process-packet (connection packet)
  (:documentation "method that will handle processing packets for either the server or client"))

(defgeneric dispatch-on-op (connection op function)
  (:documentation "Adds a function that will be called when a packet is downloaded. the function accepts a packet as an argument "))


(defgeneric shutdown  (connection)
  (:documentation "Shuts down a connection, either a client or a server"))

(defgeneric send-data (data connection))
(defgeneric send-kill (connection))

(in-package :simple-secure-sockets)
;;;;this file contains the code for handling individual packets that are received by server/client
(defmethod handle-packet ((obj server)(packet data-packet))
  "Takes in a server object and a data-packet and then with this, will forward the packet it has received to the correct connection. if client doesn't exist currently, just drop the packet (currently)"
  (let* ((recipient (convert-to-string-and-clean (recipient packet)))
         ;;recipient in the packet is a binary array so needs to be converted to a string
         
         (connection (get-current-connections-object obj recipient)))
    (when recipient;;currently just drops packet if can't find the recipient in the
      ;;current connections hash-table
      (send connection packet))))
(defmethod handle-packet :before ((obj server)(packet data-packet))
  (f-format :debug :packet-write "sending: ~s~%" packet))


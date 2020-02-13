(in-package :simple-secure-sockets)
;;;;this file contains the code for handling individual packets that are received by server/client

#|


SERVER handlers



|#
(defparameter *data-packets-pushed* 0)
(defmethod handle-packet :before ((obj server)(packet data-packet))
  (f-format :debug :packet-forward "sending: ~s~%" packet))
(defmethod handle-packet ((obj server)(packet data-packet))
  "Takes in a server object and a data-packet and then with this, will forward the packet it has received to the correct connection. if client doesn't exist currently, just drop the packet (currently)"
  (incf *data-packets-pushed*)
  (let* ((recipient (recipient* packet))
         ;;recipient in the packet is a binary array so needs to be converted to a string
         (connection (get-current-connection-by-name obj recipient)))
    (when (and recipient connection);;currently just drops packet if can't find the recipient in the
      ;;current connections hash-table
      (forward connection packet))))
(defun forward (connection packet)
  (send connection packet))
(defmethod handle-packet ((obj server) packet)
  (forced-format t "Not implemented~%")
  :NOT-IMPLEMENTED)
(defmethod handle-packet ((obj server) bad-packet)
  (forced-format t "~& server bad-packet received~%")
  :NOT-IMPLEMENTED)
(defmethod handle-packet ((obj server) (packet kill-packet))
  (let* ((sender (sender* packet))
         (connection (get-current-connection-by-name obj sender))
         (connection connection))
    (shutdown connection)    
    (ignore-errors (update-all-clients-with-disconnected-client obj connection))
    ;;tell all the connected clients
    ;;that this client has disconnected
    ;;can ignore the errors because there might be an end of file error, this will happen if
    ;;clients have shutdown their connection while the server is trying to inform them that someone
    ;;has disconnected.
    (remhash sender (current-connections obj))));;remove the connection from the hash table


#|



CLIENT handlers




|#

(defmethod handle-packet ((obj client)(packet kill-packet))
  (shutdown obj nil))

(defmethod handle-packet ((obj client)(packet clients-packet))
  "if the client is connected it adds it to the list in the slot 'available-clients, if it is not then the client is removed from the list."
  (let* ((name (client-name* packet))
         (connected (connected?* packet))
         (con (aref connected 0)))
    (cond ((string= con "0")
           (setf (available-clients obj)
                 (remove name (available-clients obj) :test #'string=)))
          ((string= con "1")
           (unless (member name (available-clients obj) :test #'string=)
             (push name (available-clients obj))))
          (t (broken-packet-error
              "The value of the connected? slot in packet is neither  0 or 1"
              packet)))))
(defmethod handle-packet ((obj client) bad-packet)
  (forced-format t "~&client bad-packet received~%")
  :NOT-IMPLEMENTED)

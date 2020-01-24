(in-package :simple-secure-sockets)
;;;;this file contains the code for handling individual packets that are received by server/client

#|


SERVER handlers



|#
(defmethod handle-packet ((obj server)(packet data-packet))
  "Takes in a server object and a data-packet and then with this, will forward the packet it has received to the correct connection. if client doesn't exist currently, just drop the packet (currently)"
  (let* ((recipient (recipient* packet))
         ;;recipient in the packet is a binary array so needs to be converted to a string
         (connection (get-current-connections-object obj recipient)))
    (when recipient;;currently just drops packet if can't find the recipient in the
      ;;current connections hash-table
      (send connection packet))))
(defmethod handle-packet ((obj server) packet)
  :NOT-IMPLEMENTED)
(defmethod handle-packet :before ((obj server)(packet data-packet))
  (f-format :debug :packet-write "sending: ~s~%" packet))
(defmethod handle-packet ((obj server) (packet kill-packet))
  (let* ((sender (sender* packet))
         (connection-obj (get-current-connections-cons obj sender))
         (connection (car connection-obj))
         (thread (cdr connection-obj)))
    (safe-socket-close (c-socket connection))
    (stop-thread thread);;stop the thread that is downloading the packets
    (ignore-errors (update-all-clients-with-disconnected-clients obj connection))
    ;;tell all the connected clients
    ;;that this client has disconnected
    ;;can ignore the errors because there might be an end of file error, this will happen if
    ;;clients have shutdown their connection while the server is trying to inform them that someone
    ;;has disconnected.
    (remhash sender (current-connections obj))));;remove the connection from the hash table
    

#|



CLIENT handlers




|#
(defmethod packet-download-function ((obj client))
  "Keeps calling the function download-sequence until the thread is manually killed. If the thread receives an :EOF from download-sequence it will simply return :DONE"
  (loop :for packet := (download-sequence obj) :then (download-sequence obj)
        :if (equal packet :EOF)
          :do  (return :DONE)
        :else
          :do (push-correct-queue obj packet)))
(defmethod push-correct-queue ((obj client)(packet data-packet))
  "Pushes the packet to the correct queue based on the type of packet. data-packets go into (gethash (sender packet) (data-packet-queues obj)), which is a specific queue identified by client names. other packets which are server related packets simply go into the queue in (packet-queue obj)"
  (let* ((c-sender (sender* packet))
         (queue (gethash c-sender (data-packet-queues obj))))
    (if queue ;;when it is not null
        (push-to-queue packet (list queue))
        (let ((queue (lparallel.queue:make-queue)))
          (push-to-queue packet (list queue))
          (setf (gethash c-sender (data-packet-queues obj)) queue)))))

(defmethod push-correct-queue ((obj client) packet)
  (push-to-queue packet (list (packet-queue obj))))

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

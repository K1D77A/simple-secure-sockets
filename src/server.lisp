
(in-package :simple-secure-sockets)



(defparameter *ip* "127.0.0.1")
(defparameter *port* 12345)

(defparameter *current-servers* (make-hash-table :test 'equal))
(defun start-server (name ip &optional (port 55555))
  (if (unique-key-p *current-servers* name)
      (setf (gethash name *current-servers*)
            (make-server name ip port))
      (error "name is not a unique name")))
(defun stop-server (name)
  "stops a server based on its name"
  (let ((server (gethash name *current-servers*)))
    (shutdown server)
    (remhash name *current-servers*)))


(defun make-server (name listen-ip &optional (listen-port 55555))
  (unless (stringp name)
    (error "Name should be a string: ~s" name))
  (let ((server (make-instance 'server :ip listen-ip :port listen-port :name name)))
    (handler-case (let ((r-c-f-n (format nil "[SERVER]:~A-receive" name))
                        (p-p-f-n (format nil "[SERVER]:~A-packet-process" name)))
                    (declare (ignore p-p-f-n)) ;;just temporary
                    (setf (receive-connections-function server)
                          (make-thread (lambda ()
                                         (accept-connections server))
                                       :name r-c-f-n)))
      ;;currently we don't do nuffin with the packets we receive
      (serious-condition (c) (progn (format t "Server error: ~s~%" c)
                                    (unless
                                        (equal (c-socket server)
                                               :server-socket-not-set)
                                      (shutdown server))
                                    server)))
                                        ; (print-object server t)
    server))


(defmethod push-to-queue ((packet packet) args-in-a-list)
  "pushes all the packets received to the queue that is supplied as the first argument in the list args-in-a-list"
  (lparallel.queue:push-queue packet (first args-in-a-list)))
(defmethod download-push-to-queue ((obj server)(connection connection))
  "Downloads packets from connection and then pushes them onto the servers queue. If the download-sequence returns :EOF then the thread will nicely return :DONE"
  (loop :for packet := (download-sequence connection) :then (download-sequence connection)
        :if (equal packet :EOF)
          :do  (return :DONE)
        :else
          :do (push-to-queue packet (list (packet-queue obj)))))
(defmethod accept-connections ((obj server))
  (loop :do 
    (let ((connection (make-instance 'connection :ip (ip obj) :port (port obj))))
      (setf (current-listening-socket obj) connection)
      (handler-case (progn (set-server-socket connection)
                           (f-format t "made it~%")
                           (wait-for-connection connection))
        ;;when we make this, how the frick does it get shut down if the server is going.. it is just going to leave us with 
        (serious-condition (c) (progn (format t "accept-connections error: ~s~%" c)
                                      (shutdown connection)
                                      connection)))
      ;;we need to accept one identity packet first, set the name of the client and then use that
      ;;key in the current-connections hash-table
      (f-format t "------WAITING ON IDENTIFY-------~%")
      (let ((identify-packet (download-sequence connection)))        
        (f-format t "-----A PACKET HAS BEEN RECEIVED-------~%")
        (if (equal (type-of identify-packet) 'identify-packet)
            (let ((id (remove-trailing-nulls (id identify-packet))))
              (setf (connection-name connection) id)
              ;;connection doesn't have ppf slot...
              ;;(push-to-queue packet (packet-queue obj))
              (setf (gethash id (current-connections obj))
                    (cons connection ;;really important to remember that there is a cons here 
                          (bt:make-thread (lambda ()
                                            (download-push-to-queue obj connection))
                                          :name (format nil "[SERVER]:~A-packet-download" id))))
                                        ; (f-format t "SENDING ACK TO CLIENT~%")
              (send connection (build-ack-packet)))
 ;;;can't dispatch on connections currently... connection dont have packet-processor-functions or  ;;;processor names    
            
            (let ((type (type-of identify-packet)))
              (f-format t "packet was not of type identify-packet: ~A~%" type)
              (f-format t "breaking connection~%")
              (shutdown connection)))))))

(defmethod shutdown ((obj connection))
  "shuts down the connection on server"
                                        ; (print-object obj t)
  ;;(find-and-kill-thread (processor-name obj))
  (usocket:socket-close (c-socket obj)))
(defmethod shutdown :before ((obj server))
  (f-format t "Attempting to shutdown server~%"))
(defmethod shutdown :after ((obj server))
  (f-format t "Shutdown complete~%"))
(defmethod shutdown ((obj server))
  "shuts down all the connections that the server is managing"
  (let ((table (current-connections obj)))
    (maphash (lambda (key val)
               (let ((con (car val))
                     (thread (cdr val)))
                 (send con (build-kill-packet))
                 (shutdown con)
                 (stop-thread thread)
                 ;;need to map the ppf hash and kill each thread manually
                 (remhash key table)))
             table))
  ;;need to stop the function that is accepting new connections
  ;;it is important that the thread is killed first, so that no modifications are made
  ;;to the value of current-listening-socket
  (stop-thread (receive-connections-function obj))
  ;;(bt:destroy-thread (process-packets-function obj))
  (usocket:socket-close (c-socket (current-listening-socket obj)))
  (remhash (name obj) *current-servers*)
  (sleep 1))

(defmethod set-server-socket :before ((object connection))
  (f-format t "Creating socket~%"))
(defmethod set-server-socket :after ((object connection))
  (f-format t "Socket created: ~A~%" (c-socket object)))
(defmethod set-server-socket ((object connection))
  (setf (c-socket object)
        (usocket:socket-listen  (ip object)
                                (port object)
                                :element-type '(unsigned-byte 8)
                                :reuse-address t
                                :reuseaddress t)))

(defmethod wait-for-connection :before ((obj connection))
  (f-format t "Waiting on socket: ~A for a connection from the client~%" (c-socket obj)))
(defmethod wait-for-connection :after ((obj connection))
  (f-format t "Completed the connection with: ~A on port: ~A" 
            (usocket:get-peer-address (c-socket obj))
            (usocket:get-peer-port (c-socket obj))))
(defmethod wait-for-connection ((obj connection))
  "takes the obj and waits until it has a connection and then sets the stream"
  (handler-case (let ((wait (usocket:socket-accept (c-socket obj))))
                  (setf (c-stream obj) (usocket:socket-stream wait)))
    (serious-condition (c) (progn (format t "Fatal issues waiting for connection: ~A" c)
                                  (shutdown obj)))))
(defmethod get-current-connections-cons ((obj server) client-name)
  (let ((connections (current-connections obj)))
    (gethash client-name connections)))
(defmethod get-current-connections-object ((obj server) client-name)
  "returns the connection object associated with client-name"
  (car (get-current-connections-cons obj client-name)))
(defmethod get-current-connections-thread ((obj server) client-name)
  (cdr (get-current-connectinos-cons obj client-name)))



;;;very simple protocol to send data across the network
;;;send message saying starting then data type first either data or kill then length first and then the data then a stopping
;;;ie
;;;start = #(115 116 97 114 116)
;;;op data = #(100)
;;;op kill = #(107)
;;;len data = #(4)
;;;data = #(1 2 3 4)
;;;stop =  #(115 116 111 112)
;;;complete transfer = #(114 116 97 114 116 100 4 1 2 3 4 115 116 111 112)
;;;couple examples if data is '(1 2 3 4) then as a string the data is = "startd9(1 2 3 4)stop"
;;;as a byte-array #(115 116 97 114 116 100 57 40 49 32 50 32 51 32 52 41 115 116 111 112)
;;;kill is "startkstop"
;;;as a sequence: #(115 116 97 114 116 107 115 116 111 112)
;;;hard limit on sequence size is going to be 255 + header and footer bytes





(in-package :simple-secure-sockets)



(defparameter *ip* "127.0.0.1")
(defparameter *port* 12345)



(defun make-server (name listen-ip &optional (listen-port 55555))
  (unless (stringp name)
    (error "Name should be a string: ~s" name))
  (let ((server (make-instance 'server :ip ip :port port :name name)))
    (handler-case (progn (set-server-socket server)
                         (wait-for-client server))
      (serious-condition (c) (progn (format t "Error of some sort oof: ~s~%" c)
                                    (unless
                                        (equal (c-socket server)
                                               :server-socket-not-set)
                                      (usocket:socket-close (c-socket server)))
                                    server)))
    (print-object server t)
    server))
(defun start (name ip port)
  (make-server name ip port))
(defun stop (name)
  "stops a server based on its name"
  (let ((server (gethash name *current-servers*)))
    (shutdown server)))

(defmethod push-to-queue ((packet packet) args-in-a-list)
  "pushes all the packets received to the queue that is supplied as the first argument in the list args-in-a-list"
  (lparallel.queue:push-queue packet (first args-in-a-list)))
(defmethod accept-connections ((obj server))
  (let ((connection (make-instance 'connection :ip (ip obj) :port (port obj))))
    (handler-case (progn (set-server-socket connection)
                         (dispatch-on-op connection :ALL #'push-to-queue (list (packet-queue obj)))
                         (wait-for-client connection))
      (serious-condition (c) (progn (format t "Error of some sort oof: ~s~%" c)
                                    (unless
                                        (equal (c-socket connection)
                                               :socket-not-set)
                                      (usocket:socket-close (c-socket connection)))
                                    connection)))
    ;;we need to accept one identity packet first, set the name of the client and then use that as the
    ;;key in the current-connections hash-table
    (let ((identify-packet (download-sequence connection)))
      (when (equal (type-of identify-packet) 'identify-packet)
        (let ((id (id identify-packet)))
          (setf (connection-name connection) id)
          (setf (gethash id (current-connections obj)) connection))
        
        
        (error "Packet received was not an identify-packet")))))
(defmethod shutdown :before ((obj server))
  (f-format t "Attempting to shutdown server~%"))
(defmethod shutdown :after ((obj server))
  (f-format t "Shutdown complete~%"))
(defmethod shutdown ((obj server))
  "shuts down all the connections that the server is managing"
  (let ((table (current-connections obj)))
    (maphash (lambda (key val)
               (shutdown val)
               (remhash key table))
             table)
    ;;need to stop the function that is accepting new connections
    (sleep 1)))

(defmethod set-server-socket :before ((obj connection))
  (f-format t "Creating socket~%"))
(defmethod set-server-socket :after ((obj connection))
  (f-format t "Socket created: ~A~%" (c-socket obj)))
(defmethod set-server-socket ((obj connection))
  (setf (c-socket obj)
        (usocket:socket-listen  (ip obj)
                                (port obj)
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
  (let ((wait (usocket:socket-accept (c-socket obj))))
    (setf (c-stream obj) (usocket:socket-stream wait))))


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




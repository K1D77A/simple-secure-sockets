
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
                    (if (equal (set-server-socket server) :ADDRESS-IN-USE)
                        (shutdown server)
                        (setf (receive-connections-function server)
                              (make-thread (lambda ()
                                             (accept-connections server))
                                           :name r-c-f-n))))
      ;;currently we don't do nuffin with the packets we receive
      (serious-condition (c) (progn (format t "Server error: ~s~%" c)
                                    (shutdown server)
                                    server)))
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
    (let ((current-connection (wait-for-connection obj (make-instance 'connection))))
      (when (not (equal current-connection :SERIOUS-CONDITION))
        ;;we need to accept one identity packet first, set the name of the client
        ;;and then use that
        ;;key in the current-connections hash-table
        (f-format :debug :server-receive   "------WAITING ON IDENTIFY-------")
        (let ((identify-packet (download-sequence current-connection)))        
          (f-format :debug :server-receive  "-----A PACKET HAS BEEN RECEIVED-------~%")
          (if (equal (type-of identify-packet) 'identify-packet)
              (let ((id (remove-trailing-nulls (id identify-packet))))
                (setf (connection-name current-connection) id)
                ;;connection doesn't have ppf slot...
                ;;(push-to-queue packet (packet-queue obj))
                (setf (gethash id (current-connections obj))
                      (cons current-connection ;;really important to remember that there is a cons 
                            (bt:make-thread
                             (lambda ()
                               (download-push-to-queue obj current-connection))
                             :name (format nil "[SERVER]:~A-packet-download" id))))
                                        ; (f-format t "SENDING ACK TO CLIENT~%")
                (send current-connection (build-ack-packet)))
 ;;;can't dispatch on connections currently... connection dont have packet-processor-functions or  ;;;processor names
              (let ((type (type-of identify-packet)))
                (f-format :error :server-receive
                          "packet was not of type identify-packet: ~A" type)
                (f-format :error :server-receive  "breaking connection")
                (shutdown current-connection))))))))

(defmethod shutdown ((obj connection))
  "shuts down the connection on server"
  ;; (print-object obj t)
  ;;(find-and-kill-thread (processor-name obj))
  (safe-socket-close (c-socket obj)))
(defmethod shutdown :before ((obj server))
  (f-format :info :server-stop "Attempting to shutdown server"))
(defmethod shutdown :after ((obj server))
  (f-format :info :server-stop "Shutdown complete~%"))
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
  (let ((thread (receive-connections-function obj))
        (socket (current-listening-socket obj)))
    (unless (keywordp thread)
      (stop-thread thread)
      ;;(bt:destroy-thread (process-packets-function obj))
      (unless (keywordp socket)
        (safe-socket-close  socket))
      (remhash (name obj) *current-servers*))))

(defmethod set-server-socket :before ((object server))
  (f-format :debug :server-receive  "Creating socket"))
(defmethod set-server-socket :after ((object server))
  (f-format :debug :server-receive  "Socket created: ~A" (current-listening-socket object)))
(defmethod set-server-socket ((object server))
  (handler-case
      (setf (current-listening-socket object)
            (usocket:socket-listen  (ip object)
                                    (port object)
                                    :element-type '(unsigned-byte 8)
                                    :reuse-address t
                                    :reuseaddress t))
    (USOCKET:ADDRESS-IN-USE-ERROR (c)
      (f-format :all :server-start "address in use ~A~%" c)
      :ADDRESS-IN-USE)))

(defmethod wait-for-connection :before ((obj server)(con connection))
  (f-format :debug :server-receive  "Waiting on socket: ~A for a connection from the client"
            (current-listening-socket obj)))
(defmethod wait-for-connection :after ((obj server)(con connection))
  (f-format :debug :server-receive  "Completed the connection"))
(defmethod wait-for-connection ((obj server)(con connection))
  "takes the obj and waits until it has a connection and then sets the stream"
  (handler-case
      (let ((wait (usocket:socket-accept (current-listening-socket obj))))
        (setf (c-socket con) wait
              (c-stream con) (usocket:socket-stream wait)
              (ip con) (usocket:get-peer-address (c-socket con))
              (port con) (usocket:get-peer-port (c-socket con)))
        con)
    (serious-condition (c)
      (f-format :error :server-receive 
                "Fatal issues waiting for connection: ~A" c)
      :SERIOUS-CONDITION)))

(defmethod get-current-connections-cons ((obj server) client-name)
  (let ((connections (current-connections obj)))
    (gethash client-name connections)))
(defmethod get-current-connections-object ((obj server) client-name)
  "returns the connection object associated with client-name"
  (car (get-current-connections-cons obj client-name)))
(defmethod get-current-connections-thread ((obj server) client-name)
  (cdr (get-current-connections-cons obj client-name)))



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




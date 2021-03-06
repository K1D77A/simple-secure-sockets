
(in-package :simple-secure-sockets)

(declaim (optimize (speed 3)(safety 1)))

(defparameter *current-servers* (make-hash-table :test 'equal))
(defparameter *encryption* nil);;encryption parsing is currently working, however
;;when attempts are made to send large quantities of packets it quits working
(defparameter *parser* #'download-sequence-fsm)
(defparameter *cipher* *aes256-cipher*)
;;(defparameter *buff-not-stream* nil)



(defun download-and-parse (connection)
  "A wrapper function that is used to parse data from a stream. If encryption is non nil then
this will download an encrypted packet, decrypt it and then set use the stream of the decrypted 
packet as the stream used within the parser"
  (bt:with-lock-held ((stream-lock connection))
    (if (and *encryption* *cipher*)
        (let ((old (c-stream connection))
              (buf (download-encrypted-packet connection *cipher*)))       
          (if (equal buf :EOF)
              :EOF
              (progn 
                (setf (c-stream connection) buf)
                (let ((parsed (funcall *parser* connection)))
                  (setf (c-stream connection) old)
                  parsed))))
        (funcall *parser* connection))))

(defun start-server (name ip &optional (port 55555))
  "Boots up a server of name and on port"
  (if (unique-key-p *current-servers* name)
      (setf (gethash name *current-servers*)
            (make-server name ip port))
      (error "name is not a unique name")))

(defun stop-server (name)
  "stops a server based on its name"
  (let ((server (gethash name *current-servers*)))
    (shutdown server)
    (remhash name *current-servers*)))


(defmethod all-connection-streams-empty-p ((server server))
  "checks if all the connections to the server no longer are empty"
  (let ((connections (current-connections-array server)))
    (every (lambda (con)
             (let ((stream (c-stream con)))
               (if (open-stream-p stream)
                   (not (listen stream))
                   nil)))
           connections)))

(defun make-server (name listen-ip &optional (listen-port 55555) (thread-count 1))
  "Starts up a server named by name on the IP listen-ip on listen-port with thread-count threads 
processing the connections and queues for storing the packet"
  (unless (stringp name)
    (error "Name should be a string: ~s" name))
  (let ((server (make-instance 'server :ip listen-ip :port listen-port
                                       :name name 
                                       :handle-cons-thread-count thread-count)))
    (handler-case (if (equal (set-server-socket server) :ADDRESS-IN-USE)
                      (progn (shutdown server)
                             :ADDRESS-IN-USE)
                      (progn (setup-thread-kernel server)
                             (start-accept-connections server)
                             (start-download-from-connections server)
                             server))
      (serious-condition (c) (progn (format t "Server error: ~s~%" c)
                                    (write-error c)
                                    (shutdown server)
                                    :ERROR-OCCURRED)))))

(defmethod start-download-from-connections ((obj server))
  (let ((d-f-c-t (format nil "[~A]:download-from-connections-thread" (name obj))))
    (setf (download-from-connections-thread obj)
          (make-thread (lambda ()
                         (process-connections obj))
                       :name d-f-c-t))))

(defmethod start-accept-connections ((obj server))
  (let ((r-c-f-n (format nil "[~A]:receive" (name obj))))
    (setf (receive-connections-function obj)
          (make-thread (lambda ()
                         (accept-connections obj))
                       :name r-c-f-n))))


(defmethod setup-thread-kernel ((obj server))
  "Stars up lparallels kernel with the right amount of threads"
  (with-accessors ((name name)
                   (thread-count handle-cons-thread-count))
      obj
    (setf lparallel:*kernel*
          (lparallel:make-kernel thread-count
                                 :name (format nil "[~A]:process-connections-kernel" name)))))


(defmethod download-and-process ((obj server)(connection con-to-server))
  "Downloads all the packets available on a connection until (listen (c-stream connection)) 
returns nil and pushes them onto the queue associated with connection.
If the download-sequence returns :EOF then :EOF is returned, if (listen ) returns nil then :DONE
is returned"
  (let ((stream (c-stream connection)))
    (handler-case
        (while-finally-loop (listen stream) ((return :DONE))
            ((let ((packet (download-and-parse connection)))
               (if (equal packet :EOF)
                   (return :EOF)
                   (handle-packet obj packet)))));;not sure if this will cause
      ;;sync problems 
      (stream-error () :EOF)
      (TYPE-ERROR () :EOF))))

(defmethod process-connections ((obj server))
  "infinitely loops over current-connections-array and calls using lparallels pmapcar function"
  (loop
    :if (zerop (length (current-connections-array obj)))
      :do (sleep 0.001)
    :else
      :do (loop :for con :across (current-connections-array obj)
                :for x := (download-and-process obj con)
                  :then (download-and-process obj con)
                :when (equal x :EOF)
                  :do (shutdown con nil)
                      (remove-con obj con))))

(defmethod accept-connections ((obj server))
  "Permanently loops and waits for new connections. When a client connects the a new instance
of 'con-to-server is created and then the process for initializing a client starts. The new client is
expected to send an identify packet which is used by the server to identify the connection, and for
other connected clients to send packets to them. If there is a successful connection then all
the other connected clients are informed that the new client has been connected"
  (loop :do
    (let ((current-connection (wait-for-connection obj (make-instance 'con-to-server))))
      (when (not (equal current-connection :SERIOUS-CONDITION))
        ;;we need to accept one identity packet first, set the name of the client
        ;;and then use that
        ;;key in the current-connections hash-table
        (f-format :debug :server-receive   "------WAITING ON IDENTIFY-------")
        (let ((identify-packet (download-and-parse current-connection)))        
          (f-format :debug :server-receive  "-----A PACKET HAS BEEN RECEIVED-------~%")
          (if (equal (type-of identify-packet) 'identify-packet)
              (let ((id (id* identify-packet)))
                (setf (connection-name current-connection) id)                
                ;; (forced-format t "~a" (type-of (queue current-connection)))
                (add-connection obj current-connection)
                (send current-connection (build-ack-packet))
                (setf (connectedp current-connection) t)
                (update-all-clients-with-all-connected obj))
              ;;ideally it would be better if we could jam this into the background
              ;;but 
              (let ((type (type-of identify-packet))) 
                (f-format :error :server-receive
                          "packet was not of type identify-packet: ~A" type)
                (f-format :error :server-receive  "breaking connection")
                (shutdown current-connection))))))))

(defmethod add-connection ((obj server)(connection connection))
  "adds a connection to the server by adding it to the servers connection hash-table and connections
array"
  ;;later, if either the value in the hashtable or the value in the array is modified, the value
  ;;of the other is modified.
  (modify-server obj
    (setf (gethash (connection-name connection)
                   (current-connections obj))
          connection)
    (let* ((cons-ar (current-connections-array obj))
           (empty-pos (position-if (lambda (ele)
                                     (not (connectedp ele)))
                                   cons-ar)))
      (if (numberp empty-pos)
          (setf (aref cons-ar empty-pos) connection)
          (vector-push-extend connection cons-ar)))))

(defmethod shutdown ((obj con-to-server) &optional send-killp)
  "shuts down the connection on server"
  ;; (declare (ignore send-killp))
  (when send-killp
    (forced-format t "~&sending kill~%"))
  ;; (print-object obj t)
  ;;(find-and-kill-thread (processor-name obj))
  (when send-killp
    (send obj (build-kill-packet)))
  (let ((pack (download-and-parse obj)))
    ;;even if there is an error and :EOF is returned just kill the connection
    (when (or (equal (type-of pack) 'ack-packet) (equal pack :EOF))
      (safe-socket-close (c-socket obj))
      (setf (connectedp obj) nil)))
  (safe-socket-close (c-socket obj))
  (setf (connectedp obj) nil))

(defmethod shutdown :before ((obj server) &optional send-killp)
  (declare (ignore send-killp))
  (f-format :info :server-stop "Attempting to shutdown server"))

(defmethod shutdown :after ((obj server) &optional send-killp)
  (declare (ignore send-killp))
  (f-format :info :server-stop "Shutdown complete~%"))
;;;there is no instance where you don't want to send kill if you are the server..

(defmethod remove-con ((obj server) (con connection))
  (modify-server obj
    (remhash (connection-name con) (current-connections obj))
    (delete con (current-connections-array obj) :test #'equal)))

(defmethod shutdown ((obj server) &optional send-killp)
  "shuts down all the connections that the server is managing"
  ;;  (declare (ignore send-killp))
  (let ((table (current-connections obj)))
    (maphash (lambda (key val)
               (declare (ignore key))
               (shutdown val (if send-killp
                                 t
                                 nil))
               (remove-con obj val))
             table))
  (with-accessors  ((receive-connections receive-connections-function)                    
                    (socket current-listening-socket)
                    (download-thread download-from-connections-thread))
      obj
    (unless (keywordp download-thread)
      (stop-thread download-thread))
    (unless (keywordp receive-connections)
      (stop-thread receive-connections))
    (lparallel:end-kernel)
    (unless (keywordp socket)
      (safe-socket-close socket))
    (remhash (name obj) *current-servers*)))

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

(defmethod get-current-connection-by-name ((obj server) client-name)
  (let ((connections (current-connections obj)))
    (gethash client-name connections)))

(defmethod get-current-connections-object ((obj server) client-name)
  "returns the connection object associated with client-name"
  (get-current-connection-by-name obj client-name))


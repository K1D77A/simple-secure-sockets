
(in-package :simple-secure-sockets)
(defclass connection()
  ((connection-name
    :accessor connection-name
    :initarg :connection-name
    :initform :name-not-set)
   (ip
    :accessor ip
    :initarg :ip
    :initform :ip-not-set)
   (port
    :accessor port
    :initarg :port
    :initform :port-not-set)
   (socket
    :accessor c-socket
    :initform :socket-not-set)
   (stream
    :accessor c-stream
    :initform :stream-not-set)
   (stream-lock
    :accessor stream-lock
    :initform (bt:make-lock))
   (connectedp
    :accessor connectedp
    :initform nil))
  (:documentation "This class contains the backbone required to create a valid connection"))

(defclass con-to-server (connection)
  ((queue
    :accessor queue
    :initform :queue-not-set))
  (:documentation "This class is to be used by a server to associate the connections 
it receives from a client with a queue, this queue is where all the packets the client sends are
are put before processing"))

(defclass client (connection)
  ((available-clients
    :accessor available-clients
    :initform (list :available-clients))
   (packet-processor-function
    :accessor packet-processor-function
    :initform :packet-processor-function-not-set)
   (packet-processor-functions
    :accessor ppf
    :initform (make-hash-table))
   (packet-queue
    :accessor packet-queue
    :type lparallel.cons-queue:cons-queue
    :initform (lparallel.queue:make-queue)
    :documentation "Queue for all the packets from the server")
   (data-packet-queues
    :accessor data-packet-queues
    :initform (make-hash-table :test #'equal))
   (packet-download-thread
    :accessor packet-download-thread
    :initform :download-thread-not-set))
  (:documentation "class containing the slots required for the client"))


(defclass server ()
  ((name
    :accessor name
    :initarg :name
    :initform :name-not-set)
   (ip
    :accessor ip
    :initarg :ip
    :initform :ip-not-set)
   (port
    :accessor port
    :initarg :port
    :initform :port-not-set)
   (current-connections
    :accessor current-connections
    :initform (make-hash-table :test #'equalp))
   (current-connections-array
    :accessor current-connections-array
    :initform (make-array 0  :adjustable t :element-type 'connection :fill-pointer 0))
   (receive-connections-function
    :accessor receive-connections-function
    :initform :connections-function-not-set)
   (packet-queues
    :accessor packet-queues
    :initform :queues-not-set
    :initarg :queues)
   (process-packets-function
    :accessor process-packets-function
    :initform :process-packets-function-not-set)
   (download-from-connections-thread
    :accessor download-from-connections-thread
    :initform :download-from-connections-thread-not-set)
   (queues-count
    :accessor queues-count
    :initform 1
    :initarg :queues-count)
   (modification-lock
    :accessor modification-lock
    :initform (bt:make-lock))
   (handle-cons-thread-count
    :accessor handle-cons-thread-count
    :initform 1
    :initarg :handle-cons-thread-count)
   (current-listening-socket
    :accessor current-listening-socket
    :initform :current-listening-socket-not-set))
  (:documentation "Class that manages the server"))


(defmethod print-object ((object server) stream)
  (with-accessors ((name name)
                   (r-c-f receive-connections-function)
                   (p-q packet-queues)
                   (p-p-f process-packets-function)
                   (current-cons current-connections))
      object
    (print-unreadable-object (object stream :type t :identity t)
      (format stream "~%Name: ~A~%Receive-connections-function: ~A~%Packet queues: ~A~%Process packets function: ~A~%Current-connections: ~A~%"
              name
              r-c-f
              (format nil "~D queues" (length p-q))
              (format nil "(~A .. ~A more .. )" (first p-p-f) (1- (length p-p-f)))
              (maphash (lambda (key val)
                         (declare (ignore key))
                         (format stream "~A~%" (connection-name val)))
                       current-cons)))))

(defmethod print-object ((object client) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "Name: ~A~%Address: ~A:~A~%Available clients: ~A~%Socket: ~A~%Stream: ~A~%Data-packet-queues: ~S~%Packet download thread: ~A~%Packet processor thread: ~A~%"
            (connection-name object)
            (ip object)
            (port object)
            (format nil "(~A) ..." (first (available-clients object)))
            (c-socket object)
            (c-stream object)
            (data-packet-queues object)
            ;;  (ppf object)
            (packet-download-thread object)
            (packet-processor-function object))))

(defmethod print-object ((object connection) stream)
  (print-unreadable-object (object stream :type t :identity t)    
    (format stream "~%Name: ~A~%Address: ~A:~A~%Socket: ~A~%Stream: ~A~%Connectedp: ~A~%"
            (connection-name object)
            (ip object)
            (port object)
            (c-socket object)
            (c-stream object)
            (connectedp object))))

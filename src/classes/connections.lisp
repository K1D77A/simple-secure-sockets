
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
    :initform :stream-not-set)))


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
    :initform (make-hash-table :test #'equal))
   (receive-connections-function
    :accessor receive-connections-function
    :initform :connections-function-not-set)
   (packet-queue
    :accessor packet-queue
    :type lparallel.cons-queue:cons-queue
    :initform (lparallel.queue:make-queue))
   (process-packets-function
    :accessor process-packets-function
    :initform :Process-packets-function-not-set)
   (current-listening-socket
    :accessor current-listening-socket
    :initform :current-listening-socket-not-set))
  ;;might at some point need a mutex here, however nothing modified this simultaneously, the
  ;;thread that modifies is killed before another thread attempts to alter it
  (:documentation "Class that manages the server"))




(defmethod print-object ((object server) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~%Name: ~A~%Receive-connections-function: ~A~%Packet queue: ~A~%Process packets function: ~A~%Current-connections: ~%"
            (name object)
            (receive-connections-function object)
            (packet-queue object)
            (process-packets-function object))
    (maphash (lambda (key val)
               (declare (ignore key))
               (format stream "~A~%" val))
             (current-connections object))))

(defmethod print-object ((object client) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "Name: ~A~%Address: ~A:~A~%Available clients: ~A~%Socket: ~A~%Stream: ~A~%Data-packet-queues: ~S~%Packet download thread: ~A~%Packet processor thread: ~A~%"
            (connection-name object)
            (ip object)
            (port object)
            (available-clients object)
            (c-socket object)
            (c-stream object)
            (data-packet-queues object)
            ;;  (ppf object)
            (packet-download-thread object)
            (packet-processor-function object))))

(defmethod print-object ((object connection) stream)
  (print-unreadable-object (object stream :type t :identity t)    
    (format stream "~%Name: ~A~%Address: ~A:~A~%Socket: ~A~%Stream: ~A~%"
            (connection-name object)
            (ip object)
            (port object)
            (c-socket object)
            (c-stream object))))

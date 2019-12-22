(in-package :simple-secure-sockets)

(defclass connection()
  ((connection-name :accessor connection-name :initform :name-not-set)
   (ip :type string :accessor ip :initarg :ip)
   (port :type integer :accessor port :initarg :port)
   (socket :accessor c-socket :initform :socket-not-set)
   (stream :accessor c-stream :initform :stream-not-set)))


(defclass client (connection)
  ((server-name :accessor server-name :initform "")
   (packet-processor-functions :accessor ppf :initform (make-hash-table))
   (processor-name :accessor processor-name :initform :name-of-processor-thread-not-set))
  (:documentation "class containing the slots required for the client"))


(defclass server ()
  ((name :accessor name :initarg :name)
   (current-connections :accessor current-connections :initform (make-hash-table))
   (receive-connections-function :accessor receive-connections-function :initform :connections-function-not-set)
   (packet-queue :accessor packet-queue :type lparallel.cons-queue:cons-queue
                 :initform (lparallel.queue:make-queue))
   (process-packets-function :accessor process-packets-function :initform :Process-packets-function-not-set))
  (:documentation "Class that manages the server"))



(defclass packet ()
  ((recipient :accessor recipient :initarg :recipient :initform :recipient-not-set)
   (header :accessor header :initform %start-header)
   (footer :accessor footer :initform %stop-footer)
   (op :accessor op :initarg :op :initform :op-not-set)))
(defclass data-packet (packet)
  ((data-length :accessor d-len :initform :data-length-not-set)
   (data :accessor data :initform :data-not-set)))
(defclass identify-packet (packet)
  ((id :accessor id :initform :id-not-set)))
(defclass kill-packet (packet)
  ()
  (:documentation "Kill packet doesn't have any special information in it so it just inherits from packet. The reason for its existence is so that methods can dispatch on the class"))

(defmethod print-object ((object server) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~%Name: ~A~%Receive-connections-function: ~A~%Process packet Function: ~A~%Process packets function: ~A~%Current-connections: ~A~%"
            (name object)
            (receive-connections-function object)
            (process-packets-function object)
            (packet-queue object)
            (maphash (lambda (key val)
                       (declare (ignore key))
                       (print-object val))
                     (current-connections object)))))
(defmethod print-object ((object client) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "Name: ~A~%~%Address: ~A:~A~%Socket: ~A~%Stream: ~A~%packet-processor-functions: ~A~%Processor thread name: ~A~%"
            (name object)
            (ip object)
            (port object)
            (c-socket object)
            (c-stream object)
            (ppf object)
            (processor-name object))))


(defmethod print-object ((object data-packet) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~%Data: ~s~%"
            (data object))))

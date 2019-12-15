(in-package :simple-secure-sockets)

(defclass connection()
  ((ip :type string :accessor ip :initarg :ip)
   (port :type integer :accessor port :initarg :port)
   (socket :accessor c-socket :initform :socket-not-set)
   (stream :accessor c-stream :initform :stream-not-set)))


(defclass client (connection)
  ((packet-processor-functions :accessor ppf :initform (make-hash-table))
   (processor-name :accessor processor-name :initform :name-of-processor-thread-not-set))
  (:documentation "class containing the slots required for the client"))


(defclass server (connection)
  ((current-connections :accessor current-connections :initform (make-hash-table))
   (receive-connections-function :accessor receive-connections-function))
  (:documentation "Class that manages the server"))



(defclass packet ()
  ((header :accessor header :initform :header-not-set)
   (footer :accessor footer :initform :footer-not-set)
   (op :accessor op :initform :op-not-set)))
(defclass data-packet (packet)
  ((data-length :accessor d-len :initform :data-length-not-set)
   (data :accessor data :initform :data-not-set)))
(defclass kill-packet (packet)
  ()
  (:documentation "Kill packet doesn't have any special information in it so it just inherits from packet. The reason for its existence is so that methods can dispatch on the class"))

(defmethod print-object ((object server) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~%Address: ~A:~A~%Socket: ~A~%Stream: ~A~%Current-connections: ~A~%Receive-connections-function: ~A~%"
            (ip object)
            (port object)
            (c-socket object)
            (c-stream object)
            (current-connections object)
            (receive-connections-function object))))
(defmethod print-object ((object client) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~%Address: ~A:~A~%Socket: ~A~%Stream: ~A~%packet-processor-functions: ~A~%Processor thread name: ~A~%"
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

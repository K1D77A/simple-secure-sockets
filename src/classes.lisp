(in-package :simple-secure-sockets)

(defclass connection()
  ((connection-name :accessor connection-name :initarg :connection-name :initform :name-not-set)
   (ip :type string :accessor ip :initarg :ip)
   (port :type integer :accessor port :initarg :port)
   (socket :accessor c-socket :initform :socket-not-set)
   (stream :accessor c-stream :initform :stream-not-set)))


(defclass client (connection)
  ((packet-processor-functions :accessor ppf :initform (make-hash-table))
   (packet-download-function :accessor packet-download-function :initform :name-of-processor-thread-not-set))
  (:documentation "class containing the slots required for the client"))


(defclass server ()
  ((name :accessor name :initarg :name)
   (ip :type string :accessor ip :initarg :ip)
   (port :type integer :accessor port :initarg :port)
   (current-connections :accessor current-connections :initform (make-hash-table))
   (receive-connections-function :accessor receive-connections-function :initform :connections-function-not-set)
   (packet-queue :accessor packet-queue :type lparallel.cons-queue:cons-queue
                 :initform (lparallel.queue:make-queue))
   (process-packets-function :accessor process-packets-function :initform :Process-packets-function-not-set)
   (current-listening-socket :accessor current-listening-socket :initform
                             :current-listening-socket-not-set))
  ;;might at some point need a mutex here, however nothing modified this simultaneously, the
  ;;thread that modifies is killed before another thread attempts to alter it
  (:documentation "Class that manages the server"))



(defclass packet ()
  ((recipient :accessor recipient :initarg :recipient :initform :recipient-not-set)
   (header :accessor header :initarg :header :initform :header-not-set)
   (footer :accessor footer :initarg :footer :initform :footer-not-set)
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
    (format stream "~%Name: ~A~%Receive-connections-function: ~A~%Process packet Function: ~A~%Process packets function: ~A~%Current-connections: ~%"
            (name object)
            (receive-connections-function object)
            (process-packets-function object)
            (packet-queue object))
    (maphash (lambda (key val)
               (declare (ignore key))
               (format stream "~A~%" val))
             (current-connections object))))
(defmethod print-object ((object client) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "Name: ~A~%Address: ~A:~A~%Socket: ~A~%Stream: ~A~%packet-processor-functions: ~A~%Processor thread name: ~A~%"
            (connection-name object)
            (ip object)
            (port object)
            (c-socket object)
            (c-stream object)
            (ppf object)
            (processor-name object))))
(defmethod print-object ((object connection) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~%Name: ~A~%Address: ~A:~A~%Socket: ~A~%Stream: ~A~%"
            (connection-name object)
            (ip object)
            (port object)
            (c-socket object)
            (c-stream object))))


(defmethod print-object ((object data-packet) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~%Data: ~s~%"
            (convert-to-string (data object)))))
(defmethod print-object ((object identify-packet) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~%id: ~s~%"
            (convert-to-string (id object)))))
(defmethod print-object ((object packet) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "Header: ~A~%Recipient: ~A~%OP: ~A~%Footer: ~A~%"
            (header object)
            (recipient object)
            (op object)
            (footer object))))

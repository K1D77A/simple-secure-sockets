(in-package :simple-secure-sockets)


#| due to the nature of the program packets are sent as byte arrays, and then they are read by the server which sends them back off to wherever they are supposed to be, so they are not changed from byte arrays to a nicer type like strings, this means when it comes to actually using the data its a pain as each access requires a conversion back to a nice useable type like a string, so an alternative accessor that is marked with a star has been provided, this will attempt to handle conversion to a string automatically|#

(defclass packet ()
  ((recipient
    :accessor recipient
    :initarg :recipient
    :initform :recipient-not-set)
   (sender
    :accessor sender
    :initarg :sender
    :initform :sender-not-set)
   (header
    :accessor header
    :initarg :header
    :initform :header-not-set)
   (footer
    :accessor footer
    :initarg :footer
    :initform :footer-not-set)
   (op
    :accessor op
    :initarg :op
    :initform :op-not-set)))
(defmethod recipient* (object)
  (c2s-c (slot-value object 'recipient)))
(defmethod header* (object)
  (c2s-c (slot-value object 'header)))
(defmethod footer* (object)
  (c2s-c (slot-value object 'footer)))
(defmethod op* (object)
  (let ((op (slot-value object 'op)))
    (c2s-c (typecase op
             (keyword op)
             (vector (code-char (aref op 0)))
             (t op)))))
(defclass data-packet (packet)
  ((data-length
    :accessor d-len
    :initform :data-length-not-set)
   (data
    :accessor data
    :initform :data-not-set)))
(defmethod d-len* (object)
  "d-len is supposed to be an actual number, so this handles converting from a byte array to an int"
  (let ((len (slot-value object 'data-length)))
    (c2s-c (typecase len
             (keyword len)
             (vector (aref len 0))
             (t len)))))
(defmethod data* (object)
  (c2s-c (slot-value object 'data)))
(defmethod sender* (object)
  (c2s-c (slot-value object 'sender)))

(defclass identify-packet (packet)
  ((id
    :accessor id
    :initform :id-not-set)))
(defmethod id* (object)
  (c2s-c (slot-value object 'id)))
(defclass ack-packet (packet)
  ()
  (:documentation "Ack packet"))
(defclass kill-packet (packet)
  ()
  (:documentation "Kill packet doesn't have any special information in it so it just inherits from packet. The reason for its existence is so that methods can dispatch on the class"))

(defclass clients-packet (packet)
  ((client-name
    :accessor client-name
    :initform :client-not-set)
   (connected?
    :accessor connected?
    :initform :connected?-not-set))
  (:documentation "Packet that indicates when a new client has connected or disconnected"))
(defmethod client-name* (object)
  (c2s-c (slot-value object 'client-name)))
(defmethod connected?* (object)
  (let ((con? (slot-value object 'connected?)))
    (c2s-c (typecase con?
             (keyword con?)
             (array (aref con? 0))
             (t con?)))))



(defmethod print-object ((object data-packet) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (print-packet-superclass stream object)
    (format stream "Sender: ~s~%Recipient: ~s~%Length: ~s~%Data: ~s~%"
            (sender* object)
            (recipient* object)
            (d-len* object)           
            (data* object))))


(defmethod print-object ((object kill-packet) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (print-packet-superclass stream object)))

  (defmethod print-object ((object ack-packet) stream)
    (print-unreadable-object (object stream :type t :identity t)
      (print-packet-superclass stream object)))
(defmethod print-object ((object identify-packet) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (print-packet-superclass stream object)
    (format stream "~%id: ~s~%"
            (id* object))))

(defmethod print-object ((object clients-packet) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (print-packet-superclass stream object)
    (format stream "~%clients-name: ~s~%connected?: ~s~%"
            (client-name* object)
            (connected?* object))))


(defmethod print-object ((object packet) stream)
  (print-unreadable-object (object stream)
    (format stream "Header: ~s~%Recipient: ~s~%OP: ~s~%Footer: ~s~%"
            (header* object)
            (recipient* object)
            (op* object)
            (footer* object))))

(defun print-packet-superclass (stream packet)
  (when (closer-mop:subclassp  (find-class (type-of packet))
                               (find-class 'packet))
    (print-unreadable-object (packet stream)
      (format stream "~%Header: ~s~%Recipient: ~s~%OP: ~s~%Footer: ~s~%"
              (header* packet)
              (recipient* packet)
              (op* packet)
              (footer* packet))))
  nil)

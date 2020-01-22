
(in-package :simple-secure-sockets)

(define-condition wrong-packet-received (error)
  ((message
    :initarg :e-a-message
    :accessor e-a-message
    :initform :e-a-message-not-set
    :documentation "Message indicating what when wrong")
   (packet-expected
    :initarg :e-a-expected
    :accessor e-a-expected
    :initform :e-a-expected-not-set
    :documentation "The type of packet that was expected")
   (packet-received
    :initarg :e-a-received
    :accessor e-a-received
    :initform :e-a-received-not-set
    :documentation "The packet that was received")
   (type-of-packet
    :initarg :e-a-type
    :accessor e-a-type
    :initform :e-a-type-not-set
    :documentation "The type of packet that was received")))

(defmethod print-object ((object wrong-packet-received) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~s~%Expected the type ~s~%Received the type: ~s~%Packet received: ~s~%"
            (e-a-message object)
            (e-a-expected object)
            (e-a-type object)
            (e-a-received object))))
(defun wrong-packet-received-error (message packet-type-expected packet-received)
  (error 'wrong-packet-recieved
         :e-a-message message
         :e-a-expected packet-type-expected
         :e-a-received packet-received
         :e-a-type (type-of packet-received)))


(define-condition broken-packet (error)
  ((message
    :initarg :b-p-message
    :accessor b-p-message
    :initform :b-p-message-not-set
    :documentation "Message indicating what when wrong")
   (packet
    :initarg :b-p-packet
    :accessor b-p-packet
    :initform :b-p-packet
    :documentation "The broken packet")))

(defmethod print-object ((object broken-packet) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~s~% Packet that was received: ~S~%"
            (b-p-message object)
            (b-p-packet object))))

(defun broken-packet-error (message packet)
  (error 'broken-packet
         :b-p-message message
         :b-p-packet packet))



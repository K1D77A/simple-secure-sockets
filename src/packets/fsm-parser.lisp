;;;;THis file contains a version of receive.lisp which instead utilizes the fsm implementation
;;;;that is in fsm.lisp
(in-package :simple-secure-sockets)
(declaim (optimize (speed 3) (safety 1)))
(defmethod download-sequence-fsm ((obj connection))
  "Method that handles downloading a complete sequence. If an EOF is reached, ie the client shuts down the connection on their end, this will mean and EOF is thrown, in this case download-sequence will return the symbol :EOF. "
  (declare (optimize (speed 3) (safety 1)))  
  (handler-case
      (bt:with-lock-held ((stream-lock obj))
        (tlet ((packet packet (make-instance 'packet)))
          (read-fsm-header obj packet)
          (read-fsm-op obj packet)
          (read-fsm-recipient obj packet)
          (read-fsm-sender obj packet)          
          (handle-fsm-op obj packet)
          (read-fsm-footer obj packet)
          ;;(print-object packet t)
          packet))
    (stream-error ()
      ;;(write-error c)
      :EOF)
    (SB-INT:SIMPLE-STREAM-ERROR ()
      :EOF)
    (broken-packet (c)
      (push c (car oofs))
      :EOF)))

(defparameter oofs (cons 0 nil))

(defun execute-mfsm (fsm stream error-message packet)
  (if (successful-execution-p (read-from-stream fsm stream))
      (result fsm)
      (broken-packet-error error-message packet)))

(defmethod read-fsm-header :before ((obj connection) (packet packet))
  (f-format :debug :packet-read  "New packet start~s" (get-universal-time))
  (f-format :debug :packet-read  "-Reading header"))

(defmethod read-fsm-header :after ((obj connection) (packet packet))
  (f-format :debug :packet-read  "-Header read ~S" (header* packet)))
(defmethod read-fsm-header ((obj connection) (packet packet))
  (setf (header packet)
        (the byte-array (execute-mfsm
                         (make-micro-fsm-for-string %start-header)
                         (c-stream obj)
                         "header is broken in packet"
                         packet))))

(defmethod read-fsm-op :before ((obj connection)(packet packet))
  (f-format :debug :packet-read  "--Reading op"))
(defmethod read-fsm-op :after ((obj connection)(packet packet))
  (f-format :debug :packet-read  "--OP read. OP: ~A" (op* packet)))
(defmethod read-fsm-op ((obj connection)(packet packet))
  (tlet* ((fsm micro-finite-state-machine (make-micro-fsm-from-form *valid-ops-form*))
          (result byte-array (execute-mfsm fsm (c-stream obj) "op is invalid" packet))
          (op integer (aref result 0)))
    (setf (op packet) result)
    ;;(print-object packet t)
    (cond ((eq %op-data-n op)
           (change-class packet 'data-packet))
          ((eq %op-ack-n op)
           (change-class packet 'ack-packet))
          ((eq %op-kill-n op)
           (change-class packet 'kill-packet))
          ((eq %op-identify-n op)
           (change-class packet 'identify-packet))
          ((eq %op-clients-n op)
           (change-class packet 'clients-packet)))))
;;no need for a t clause as exceptions are already checked for by the fsm


(defmethod read-fsm-recipient :before ((obj connection) (packet kill-packet))
  (f-format :debug :packet-read  "-Reading recipient "(get-universal-time)))
(defmethod read-fsm-recipient ((obj connection) (packet kill-packet))
  (setf (recipient packet)
        (the byte-array
             (execute-mfsm (make-micro-fsm-for-string %kill-recipient)
                           (c-stream obj)
                           "kill-packet recipient is invalid" packet))))

(defmethod read-fsm-recipient :before ((obj connection) (packet ack-packet))
  (f-format :debug :packet-read  "-Reading recipient "(get-universal-time)))
(defmethod read-fsm-recipient ((obj connection) (packet ack-packet))
  (setf (recipient packet)
        (the byte-array
             (execute-mfsm (make-micro-fsm-for-string %ack-recipient)
                           (c-stream obj)
                           "ack-packet recipient is invalid" packet))))


(defmethod read-fsm-recipient :before ((obj connection) (packet clients-packet))
  (f-format :debug :packet-read  "-Reading recipient "(get-universal-time)))
(defmethod read-fsm-recipient ((obj connection) (packet clients-packet))
  (setf (recipient packet)
        (the byte-array
             (execute-mfsm (make-micro-fsm-for-string %clients-recipient)
                           (c-stream obj)
                           "clients-packet recipient is invalid" packet))))


(defmethod read-fsm-recipient :before ((obj connection) (packet identify-packet))
  (f-format :debug :packet-read  "-Reading recipient "(get-universal-time)))
(defmethod read-fsm-recipient ((obj connection) (packet identify-packet))
  (setf (recipient packet)
        (the byte-array
             (execute-mfsm (make-micro-fsm-for-string %identify-recipient)
                           (c-stream obj)
                           "identify-packet recipient is invalid" packet))))


(defmethod read-fsm-recipient :before ((obj connection) (packet data-packet))
  (f-format :debug :packet-read  "-Reading recipient "(get-universal-time)))
(defmethod read-fsm-recipient ((obj client)(packet data-packet))
  "This is a special method for clients only that checks the packet the data packet they receive 
has a recipient the same as their name"
  (when (connectedp obj)
    (tlet ((mfsm micro-finite-state-machine (generate-fsm-for-con-name obj)))
      (setf (recipient packet)
            (the byte-array
                 (execute-mfsm mfsm (c-stream obj)
                               "data-packet recipient is not equal to client name" packet))))))

(defmethod read-fsm-recipient ((obj connection)(packet data-packet))
  "This is a special method for clients only that checks the packet the data packet they receive 
has a recipient the same as their name"
  (setf (recipient packet)
        (the byte-array
             (execute-mfsm (make-micro-fsm-to-read-n-chars %connection-name-len)
                           (c-stream obj)
                           "data-packet recipient is bad" packet))))

(defmethod read-fsm-recipient :after ((obj connection) (packet packet))
  (f-format :debug :packet-read  "-Recipient read. Recipient: ~A" (recipient* packet)))


;;for the sender, when sending to the server, the sender needs to be the connection-name obj
;;unless they haven't connected yet, which is only true of a single identify packet
;;all others will have a valid connection
;;in the case of a server to client connection the client should check the sender is the same as
;;their connection name

(defmethod read-fsm-sender :before ((obj connection)(packet packet))
  (f-format :debug :packet-read  "-Reading sender"))
(defmethod read-fsm-sender :after ((obj connection)(packet packet))
  (f-format :debug :packet-read  "-sender read: ~A "(sender* packet)))

(defparameter x nil)

(defmethod generate-fsm-for-con-name ((obj connection))
  (tlet ((name string (connection-name obj)))
    ;;possibility for memoization
    (make-micro-fsm-for-string (make-string-len name %connection-name-len))))

(defmethod read-fsm-sender ((obj con-to-server) (packet identify-packet))
  "This is special as there is no connection name yet"
  (setf (sender packet)
        (execute-mfsm (make-micro-fsm-to-read-n-chars %connection-name-len)
                      (c-stream obj)
                      "sender either failed to download or is not the same as connection name"
                      packet)))

(defmethod read-fsm-sender ((obj connection) (packet identify-packet))
  "This is special as there is no connection name yet"
  (execute-mfsm (make-micro-fsm-to-read-n-chars %connection-name-len)
                (c-stream obj)
                "The only time an identify-packet should be sent is from client to server"
                packet))


(defmethod read-fsm-sender ((obj con-to-server) (packet packet))
  (setf (sender packet)
        (execute-mfsm (generate-fsm-for-con-name obj)
                      (c-stream obj)
                      "sender either failed to download or is not the same as connection name"
                      packet)))

(defmethod read-fsm-sender ((obj client) (packet packet))
  (setf (sender packet)
        (execute-mfsm (make-micro-fsm-to-read-n-chars %connection-name-len)
                      (c-stream obj)
                      "sender either failed to download or is not the same as connection name"
                      packet)))

(defmethod read-fsm-sender ((obj connection) (packet packet))
  (setf (sender packet)
        (execute-mfsm (generate-fsm-for-con-name obj)
                      (c-stream obj)
                      "sender either failed to download or is not the same as connection name"
                      packet)))





(defmethod handle-fsm-op :before ((obj connection)(packet packet))
  (f-format :debug :packet-read  "--Handling OP"))
(defmethod handle-fsm-op :after ((obj connection)(packet packet))
  (f-format :debug :packet-read  "--OP Handled. Type of packet: ~A" (type-of packet)))
(defmethod handle-fsm-op ((obj connection)(packet kill-packet))
  :SHUTDOWN)
(defmethod handle-fsm-op ((obj connection)(packet ack-packet))
  :ACKNOWLEDGE)
(defmethod handle-fsm-op :after ((obj connection)(packet data-packet))
  (f-format :debug :packet-read  "---Data: ~s" (data* packet)))


(defmethod handle-fsm-op ((obj connection)(packet data-packet))
  "Thisn here handles the op code 'd' by downloading the correct amount of data and placing it in the 
correct place in the packet. First it uses a predefined mfsm to download 1 byte for the len, then 
generates a new mfsm of that length to download the rest of the data"
  (tlet* ((stream stream (c-stream obj))
          (result byte-array
                  (execute-mfsm (make-micro-fsm-from-form *valid-len-form*)
                                stream
                                "invalid length in data packet"
                                packet))
          (len integer (aref result 0))
          (len-mfsm micro-finite-state-machine (make-micro-fsm-to-read-n-chars len)))  
    (setf (d-len packet) result
          (data packet) (execute-mfsm len-mfsm
                                      stream
                                      (format nil "attempted to download ~d bytes but failed" len)
                                      packet))))



(defmethod handle-fsm-op ((obj connection)(packet clients-packet))
  "Has to download %connection-name-len in bytes then 1 more which is a boolean saying whether the 
name downloaded is connected or not"
  (tlet* ((stream stream (c-stream obj))
          (client-fsm micro-finite-state-machine (make-micro-fsm-to-read-n-chars %connection-name-len))
          (con-fsm micro-finite-state-machine (make-micro-fsm-from-form *clients-valid-con-form*))
          (client byte-array
                  (execute-mfsm client-fsm stream "client-packet invalid, error reading name" packet))
          (con? byte-array (execute-mfsm con-fsm stream
                                         "client-packet invalid, error reading con bool" packet))
          (connected integer (aref con? 0)))
    (setf (client-name packet) client
          (connected? packet) connected)))

(defmethod handle-fsm-op ((obj connection) (packet identify-packet))
  (when (connectedp obj)
    (broken-packet-error "Client attempted to send an identify when already connected" packet))
  (setf (id packet)
        (the byte-array
             (execute-mfsm  (make-micro-fsm-to-read-n-chars %connection-name-len)
                            (c-stream obj)
                            "failure to handle identify packet, couldn't download all the bytes"
                            packet))))


(defmethod read-fsm-footer :before ((obj connection)(packet packet))
  (f-format :debug :packet-read  "-reading footer"))
(defmethod read-fsm-footer :after ((obj connection)(packet packet))
  (f-format :debug :packet-read  "-footer read ~S" (footer* packet))
  (f-format :debug :packet-read  "Packet End!"))
(defmethod read-fsm-footer ((obj connection)(packet packet))
  (setf (footer packet)
        (the byte-array
             (execute-mfsm (make-micro-fsm-for-string %stop-footer)
                           (c-stream obj)
                           "footer is invalid in packet"
                           packet))))

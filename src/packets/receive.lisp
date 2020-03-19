;;;;this file contains many of the methods required for receiving packets from a connection and ;;;;processing them
(in-package :simple-secure-sockets)
(declaim (optimize (speed 0) (safety 3)))

(defparameter *read* nil)
(defun clean-read ()
  (mapcar #'code-char (reverse *read*)))

(defun non-block-read-byte (stream &optional (eof-error-p t) eof-value)
  "read byte that is non blocking, if there is nothing to read it simply returns nil"
  (declare (optimize (speed 3)(safety 0)))
  (the (or u-byte boolean)
       (if (the boolean (listen stream))
           (let ((byte (the u-byte (read-byte stream eof-error-p eof-value))))
             (push byte *read*)
             ;;(forced-format t "~A" byte)
             byte)
           nil)))

(defun timed-non-block-read-byte (time stream &optional (eof-error-p t) eof-value (sleep-time 0.001))
  "This is a non blocking version of read byte which is timed. basically you give it a time and it 
will loop and check if there is anything on the stream to be read, if not it'll sleep for sleep-time
if it eventually finds something to read it'll read and then return that byte. if it reaches the 
timeout it'll signal and error"
  (let ((byte (non-block-read-byte stream eof-error-p eof-value)))
    (if byte
        byte
        (loop :with increment := (ceiling (/ time sleep-time))
              :for x :from 1 :to increment
              :for byte? := (non-block-read-byte stream t) :then (non-block-read-byte stream t)
              :if byte?
                :do (return byte?)                    
              :else
                :do (sleep sleep-time)
              :finally (error 'stream-error :stream stream)))))

(defun read-n-bytes (n stream)
  "Reads n bytes from stream and puts them into an array of length n and type unsigned-byte 8"
  (declare (optimize (speed 3)(safety 0)))
  (let ((data (the byte-array (make-array n :element-type 'u-byte))))
    (declare (type byte-array data))
    (dotimes (i n data)
      (declare (type fixnum i)
               (type fixnum n))
      (setf (aref data i) (the u-byte (timed-non-block-read-byte 5 stream t))))))

;;if a half complete packet is sent, this will simply block...
;;if only one thread processes the connections then this means the entire server blocks...
(defmethod download-sequence ((obj connection))
  "Method that handles downloading a complete sequence. If an EOF is reached, ie the client shuts down the connection on their end, this will mean and EOF is thrown, in this case download-sequence will return the symbol :EOF. "
  (declare (optimize (speed 3) (safety 1)))  
  (handler-case
      (bt:with-lock-held ((stream-lock obj))
        (let ((packet (make-instance 'packet)))
          (read-header obj packet)
          (read-recipient obj packet)
          (read-sender obj packet)
          (read-op obj packet)
          (handle-op obj packet)
          (read-footer obj packet)
          (print-object packet t)
          packet))
    (stream-error ()
      ;;(write-error c)
      :EOF)
    (SB-INT:SIMPLE-STREAM-ERROR ()
      :EOF)
    (broken-packet ()
      (sb-ext:atomic-incf (car oofs))
      :EOF)))

(defparameter oofs (cons 0 nil))

(defmethod read-header :before ((obj connection) (packet packet))
  (f-format :debug :packet-read  "New packet start~s" (get-universal-time))
  (f-format :debug :packet-read  "-Reading header"))
(defmethod read-header :after ((obj connection) (packet packet))
  (f-format :debug :packet-read  "-Header read"))
(defmethod read-header ((obj connection) (packet packet))
  (setf (header packet)
        (the byte-array (read-n-bytes
                         (length %start-header) (c-stream obj)))))

(defmethod read-recipient :before ((obj connection) (packet packet))
  (f-format :debug :packet-read  "-Reading recipient "(get-universal-time)))
(defmethod read-recipient :after ((obj connection) (packet packet))
  (f-format :debug :packet-read  "-Recipient read. Recipient: ~A" (recipient* packet)))
(defmethod read-recipient ((obj connection) (packet packet))
  (setf (recipient packet)
        (the byte-array (read-n-bytes
                         %connection-name-len (c-stream obj)))))

(defmethod read-recipient :after ((obj client)(packet data-packet))
  "This :after method is used to verify that when a packet is read by a client, the recipient is the
same as the clients name"
  (when (connectedp obj)
    (let ((reci (recipient* packet))
          (name (connection-name obj)))
      (unless (string= reci name)
        (broken-packet-error "recipient* and connection-name are not equal" packet)))))

(defmethod read-sender :before ((obj connection)(packet packet))
  (f-format :debug :packet-read  "-Reading sender"))
(defmethod read-sender :after ((obj connection)(packet packet))
  (f-format :debug :packet-read  "-sender read: ~A "(sender* packet)))

(defparameter x nil)

(defmethod read-sender :after ((obj con-to-server)(packet data-packet))
  "This :after method is used to verify that when a packet is sent to the server from a client, the
sender of the packet is the same as the associated connections-name"
  (when (connectedp obj)
    (let ((send (sender* packet))
          (name (connection-name obj)))
      ;;(push (cons send name) x)
      (unless (string= send name)
        (broken-packet-error "sender* and connection-name are not equal" packet)))))
;;hilarious cock up right here.... trying to validate only data packets??? WHY but the packet
;;doesn't have a type passed packet yet... doesn't happen until handle-op...
(defmethod read-sender ((obj connection) (packet packet))
  (let ((sender (the byte-array (read-n-bytes %connection-name-len (c-stream obj)))))
    (setf (sender packet) sender)))    


(defmethod read-op :before ((obj connection)(packet packet))
  (f-format :debug :packet-read  "--Reading op"))
(defmethod read-op :after ((obj connection)(packet packet))
  (f-format :debug :packet-read  "--OP read. OP: ~A" (op* packet)))
(defmethod read-op ((obj connection)(packet packet))
  (let* ((op (the single-byte (read-n-bytes 1 (c-stream obj))))
         (op-as-string (c2s-c (code-char (aref op 0)))))
    (setf (op packet) op)
    ;;(print-object packet t)
    (cond ((string=  %op-data op-as-string)
           (change-class packet 'data-packet))
          ((string= %op-ack op-as-string)
           (change-class packet 'ack-packet))
          ((string=  %op-kill op-as-string)
           (change-class packet 'kill-packet))
          ((string= %op-identify op-as-string)
           (change-class packet 'identify-packet))
          ((string= %op-clients op-as-string)
           (change-class packet 'clients-packet))
          (t (broken-packet-error "Packet received is invalid." packet)))))
;;when packet is wrong it needs to be dropped, this needs to be written


(defmethod handle-op :before ((obj connection)(packet packet))
  (f-format :debug :packet-read  "--Handling OP"))
(defmethod handle-op :after ((obj connection)(packet packet))
  (f-format :debug :packet-read  "--OP Handled. Type of packet: ~A" (type-of packet)))
(defmethod handle-op ((obj connection)(packet kill-packet))
  :SHUTDOWN)
(defmethod handle-op ((obj connection)(packet ack-packet))
  :ACKNOWLEDGE)
(defmethod handle-op :after ((obj connection)(packet data-packet))
  (f-format :debug :packet-read  "---Data: ~s" (data* packet)))

(defmethod handle-op ((obj connection)(packet data-packet))
  "Thisn here handles the op code 'd' by downloading the correct amount of data and placing it in the 
correct place in the packet"
  (let* ((stream (c-stream obj))
         ;;op then sender then data
         (len (the u-byte (read-byte stream))))
    (if (<= len %max-data-size)
        (let ((data (the byte-array (read-n-bytes len stream))))
          (setf (d-len packet) (the single-byte
                                    (make-array 1 :element-type 'u-byte :initial-element len))
                (data packet) data))
        (broken-packet-error
         "Packet received is trying to send over 255 bytes in data field" packet))))

(defmethod handle-op ((obj connection)(packet clients-packet))
  (let* ((stream (c-stream obj))
         (client (the byte-array (read-n-bytes %connection-name-len stream)))
         (connected (the single-byte (read-n-bytes 1 stream))))
    (setf (client-name packet) client
          (connected? packet) connected)))

(defmethod handle-op ((obj connection) (packet identify-packet))
  (setf (id packet)
        (the byte-array (read-n-bytes %connection-name-len (c-stream obj)))))


(defmethod read-footer :before ((obj connection)(packet packet))
  (f-format :debug :packet-read  "-reading footer"))
(defmethod read-footer :after ((obj connection)(packet packet))
  (f-format :debug :packet-read  "-footer read")
  (f-format :debug :packet-read  "Packet End!"))
(defmethod read-footer ((obj connection)(packet packet))
  (setf (footer packet)
        (the byte-array (read-n-bytes (length %stop-footer) (c-stream obj)))))


;;;;gonna change the send to a generic function that accepts the packet types as
;;;;argument and a connection



;;okay so this isn't working properly, I think a better idea is to create a queue for the client
;;and then push all packets to the queue then after its easier to just process them
;; (defmethod packet-process ((obj client) (packet packet) keyword)
;;   (when (not (keywordp keyword))
;;     (error "keyword is not a keyword: ~A" (type-of keyword)))
;;   (with-accessors ((functions-hash ppf))
;;       obj
;;     (let ((function-n-args (gethash keyword functions-hash)))
;;       (mapcar (lambda (func-n-args)
;;                 (let ((function (first func-n-args))
;;                       (args-list (rest function-n-args)))
;;                   (funcall function packet args-list)))
;;               function-n-args))))
;; ;;oof the above doesn't exist ^
;; (defmethod process-packet ((obj connection)(packet data-packet))
;;   "Processes the data-packets for connection. It calls all the functions that are contained within a list under the key :DATA in the slot 'packet-processor-functions' with the argument packet. If you destructively modify packet then any functions after will be passed the modified version of packet"
;;   (packet-process obj packet :DATA))

;; (defmethod process-packet ((obj connection)(packet packet))
;;   "processes all the packets that are received by Connection. It calls the functions  that are stored within a list under the key :ALL in the slot 'packet-processor-functions' with the argument packet and an list-of-arguments which is also stored" 
;;   (packet-process obj packet :ALL))
;; (defmethod process-packet ((obj connection)(packet identify-packet))
;;   (f-format :debug :packet-process "IDENTITY RECEIVED~A"))
;; (defmethod process-packet ((obj connection)(packet kill-packet))
;;   "Processes a received kill-packet and shuts down the connection"
;;   (shutdown obj))

;; (defmethod dispatch-on-op ((obj connection) op function args-in-a-list)
;;   (if (and (keywordp op) (functionp function) (find op *op-keywords*))
;;       (push (cons function args-in-a-list) (gethash op (ppf obj)))
;;       (error "Either op is not a keyword or is not valid see *op-keywords* or function is not a function like #'. OP: ~s~%Func: ~s~%Valid OPs: ~S"
;;              (type-of op)
;;              (type-of function)
;;              *op-keywords*)))

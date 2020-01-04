;;;;this file contains many of the methods required for receiving packets from a connection and ;;;;processing them
(in-package :simple-secure-sockets)


(defun read-n-bytes (n stream)
  "Reads n bytes from stream and puts them into an array of length n and type unsigned-byte 8"
  (let ((data (make-array n :element-type '(unsigned-byte 8))))
    (dotimes (i n data)
      (setf (aref data i) (read-byte stream t)))))

(defmethod download-sequence ((obj connection))
  "Method that handles downloading a complete sequence. If an EOF is reached, ie the client shuts down the connection on their end, this will mean and EOF is thrown, in this case download-sequence will return the symbol :EOF. "
  (handler-case (let ((packet (make-instance 'packet)))
                  (read-header obj packet)
                  (read-recipient obj packet)
                  (read-op obj packet)
                  (handle-op obj packet)
                  (read-footer obj packet)
                  packet)
    (end-of-file () :EOF))); if the stream is broken return :EOF

(defmethod read-header :before ((obj connection) (packet packet))
  (f-format t "New packet start~s~%-Reading header~%"(get-universal-time)))
(defmethod read-header :after ((obj connection) (packet packet))
  (f-format t "-Header read~%"))
(defmethod read-header ((obj connection) (packet packet))
  (setf (header packet)
        (convert-to-string (read-n-bytes
                            (length %start-header) (c-stream obj)))))
(defmethod read-recipient :before ((obj connection) (packet packet))
  (f-format t "-Reading recipient~%"(get-universal-time)))
(defmethod read-recipient :after ((obj connection) (packet packet))
  (f-format t "-Recipient read. Recipient: ~A~%" (recipient packet)))
(defmethod read-recipient ((obj connection) (packet packet))
  (setf (recipient packet)
        (convert-to-string (read-n-bytes
                            %connection-name-len (c-stream obj)))))

(defmethod read-op :before ((obj connection)(packet packet))
  (f-format t "--Reading op~%"))
(defmethod read-op :after ((obj connection)(packet packet))
  (f-format t "--OP read. OP: ~A~%" (op packet)))
(defmethod read-op ((obj connection)(packet packet))
  (let* ((op (read-byte (c-stream obj)))
         (op-as-string (convert-to-string (code-char op))))
    (setf (op packet) op-as-string)
    ;;(print-object packet t)
    (cond ((string=  %op-data op-as-string)
           (change-class packet 'data-packet))
          ((string= %op-ack op-as-string)
           (change-class packet 'ack-packet))
          ((string=  %op-kill op-as-string)
           (change-class packet 'kill-packet))
          ((string= %op-identify op-as-string)
           (change-class packet 'identify-packet))
          (t (f-format t "Packet taken in is not a valid packet ~A" packet)))))
;;when packet is wrong it needs to be dropped, this needs to be written


(defmethod handle-op :before ((obj connection)(packet packet))
  (f-format t "--Handling OP~%"))
(defmethod handle-op :after ((obj connection)(packet packet))
  (f-format t "--OP Handled. Type of packet: ~A~%" (type-of packet)))
(defmethod handle-op ((obj connection)(packet kill-packet))
  :SHUTDOWN)
(defmethod handle-op ((obj connection)(packet ack-packet))
  :ACKNOWLEDGE)
(defmethod handle-op :after ((obj connection)(packet data-packet))
  (f-format t "---Data: ~s~%" (data packet)))
(defmethod handle-op ((obj connection)(packet data-packet))
  "Thisn here handles the op code 'd' by downloading the correct amount of data and placing it in the 
correct place in the packet"
  (let* ((stream (c-stream obj))
         (len (read-byte stream))
         (bytes (read-n-bytes len stream))
         (data-string (convert-to-string bytes)))
    (setf (d-len packet) len
          (data packet) data-string)))
(defmethod handle-op ((obj connection) (packet identify-packet))
  (setf (id packet)
        (convert-to-string (read-n-bytes %connection-name-len (c-stream obj)))))
(defmethod read-footer :before ((obj connection)(packet packet))
  (f-format t "-reading footer~%"))
(defmethod read-footer :after ((obj connection)(packet packet))
  (f-format t "-footer read~%Packet End!~%"))
(defmethod read-footer ((obj connection)(packet packet))
  (setf (footer packet)
        (convert-to-string (read-n-bytes (length %stop-footer) (c-stream obj)))))


;;;;gonna change the send to a generic function that accepts the packet types as
;;;;argument and a connection


(defmethod packet-download-function ((obj client))
  "Keeps calling the function download-sequence until the thread is manually killed. If the thread receives an :EOF from download-sequence it will simply return :DONE"
  (loop :for packet := (download-sequence obj) :then (download-sequence obj)
        :if (equal packet :EOF)
          :do  (return :DONE)
        :else
          :do (push-to-queue packet (list (packet-queue obj)))))
;;okay so this isn't working properly, I think a better idea is to create a queue for the client
;;and then push all packets to the queue then after its easier to just process them
(defmethod packet-process ((obj client) (packet packet) keyword)
  (when (not (keywordp keyword))
    (error "keyword is not a keyword: ~A" (type-of keyword)))
  (with-accessors ((functions-hash ppf))
      obj
    (let ((function-n-args (gethash keyword functions-hash)))
      (mapcar (lambda (func-n-args)
                (let ((function (first func-n-args))
                      (args-list (rest function-n-args)))
                  (funcall function packet args-list)))
              function-n-args))))
;;oof the above doesn't exist ^
(defmethod process-packet ((obj connection)(packet data-packet))
  "Processes the data-packets for connection. It calls all the functions that are contained within a list under the key :DATA in the slot 'packet-processor-functions' with the argument packet. If you destructively modify packet then any functions after will be passed the modified version of packet"
  (packet-process obj packet :DATA))

(defmethod process-packet ((obj connection)(packet packet))
  "processes all the packets that are received by Connection. It calls the functions  that are stored within a list under the key :ALL in the slot 'packet-processor-functions' with the argument packet and an list-of-arguments which is also stored" 
  (packet-process obj packet :ALL))
(defmethod process-packet ((obj connection)(packet identify-packet))
  (f-format t "IDENTITY RECEIVED~A"))
(defmethod process-packet ((obj connection)(packet kill-packet))
  "Processes a received kill-packet and shuts down the connection"
  (shutdown obj))

(defmethod dispatch-on-op ((obj connection) op function args-in-a-list)
  (if (and (keywordp op) (functionp function) (find op *op-keywords*))
      (push (cons function args-in-a-list) (gethash op (ppf obj)))
      (error "Either op is not a keyword or is not valid see *op-keywords* or function is not a function like #'. OP: ~s~%Func: ~s~%Valid OPs: ~S"
             (type-of op)
             (type-of function)
             *op-keywords*)))

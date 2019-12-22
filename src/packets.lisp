
(in-package :simple-secure-sockets)

(defparameter *var-hashtable* (make-hash-table))
(defvar-hash %start-header "start")
(defvar-hash %op-data "d")
(defvar-hash %op-kill "k")
(defvar-hash %op-identify "i")
(defvar-hash %stop-footer "stop")
(defvar-hash %connection-name-len 16);bytes
(defvar-hash %kill-recipient "iwanttodieplease")
(defvar-hash %identify-recipient "letmeidentifyplz")
(defvar-hash %max-data-size 255);1byte len
(defmacro defvar-hash (symbol val)
  "Defines a variable with defvar named with symbol and given the value val. This is also put into the hashtable *var-hashtable* with the same symbol and val"
  `(progn (defvar ,symbol ,val)
          (unless (gethash ,symbol *var-hashtable*)
            (setf (gethash ,symbol *var-hashtable*) ,val))))
  

  (defun read-n-bytes (n stream)
    "Reads n bytes from stream and puts them into an array of length n and type unsigned-byte 8"
    (let ((data (make-array n :element-type '(unsigned-byte 8))))
      (dotimes (i n data)
        (setf (aref data i) (read-byte stream)))))

(defmethod download-sequence ((obj connection))
  "Method that handles downloading a complete sequence"
  (let ((packet (make-instance 'packet)))
    (read-header obj packet)
    (read-recipient obj packet)
    (read-op obj packet)
    (handle-op obj packet)
    (read-footer obj packet)
    packet))

(defmethod read-header :before ((obj connection) (packet packet))
  (f-format t "New packet start~s~%-Reading header~%"(get-universal-time)))
(defmethod read-header :after ((obj connection) (packet packet))
  (f-format t "-Header read~%"))
(defmethod read-header ((obj connection) (packet packet))
  (setf (header packet)
        (byte-vector-to-string (read-n-bytes
                                (length %start-header) (c-stream obj)))))
(defmethod read-recipient :before ((obj connection) (packet packet))
  (f-format t "-Reading recipient~%"(get-universal-time)))
(defmethod read-recipient :after ((obj connection) (packet packet))
  (f-format t "-Recipient read~%"))
(defmethod read-recipient ((obj connection) (packet packet))
  (setf (recipient packet)
        (byte-vector-to-string (read-n-bytes
                                %connection-name-len (c-stream obj)))))

(defmethod read-op :before ((obj connection)(packet packet))
  (f-format t "--Reading op~%"))
(defmethod read-op :after ((obj connection)(packet packet))
  (f-format t "--OP read~%"))
(defmethod read-op ((obj connection)(packet packet))
  (let* ((op (read-byte (c-stream obj)))
         (interned (intern (string (code-char op)) :keyword)))
    (setf (op packet) interned)
    (cond ((equal (string-to-keyword %op-data) op)
           (change-class packet 'data-packet))
          ((equal (string-to-keyword %op-kill) op)
           (change-class packet 'kill-packet))
          ((equal (string-to-keyword %op-identify) op)
           (change-class packet 'identify-packet)))))


(defmethod handle-op :before ((obj connection)(packet packet))
  (f-format t "--Handling OP~%"))
(defmethod handle-op :after ((obj connection)(packet packet))
  (f-format t "--OP Handled~%"))
(defmethod handle-op ((obj connection)(packet kill-packet))
  :SHUTDOWN)
(defmethod handle-op :after ((obj connection)(packet data-packet))
  (f-format t "---Data: ~s~%" (data packet)))
(defmethod handle-op ((obj connection)(packet data-packet))
  "Thisn here handles the op code 'd' by downloading the correct amount of data and placing it in the 
correct place in the packet"
  (let* ((stream (c-stream obj))
         (len (read-byte stream))
         (bytes (read-n-bytes len stream))
         (data-string (byte-vector-to-string bytes)))
    (setf (d-len packet) len
          (data packet) data-string)))
(defmethod handle-op ((obj connection) (packet identify-packet))
  (setf (id packet)
        (byte-vector-to-string (read-n-bytes 16 obj))))



(defmethod read-footer :before ((obj connection)(packet packet))
  (f-format t "-reading footer~%"))
(defmethod read-footer :after ((obj connection)(packet packet))
  (f-format t "-footer read~%Packet End!~%"))
(defmethod read-footer ((obj connection)(packet packet))
  (setf (footer packet)
        (byte-vector-to-string (read-n-bytes (length %stop-footer) (c-stream obj)))))


;;;;gonna change the send to a generic function that accepts the packet types as
;;;;argument and a connection

(defun build-packet (recipient op)
  "takes in a recipient and creates an instance of packet"
  (unless (n-or-lessp %connection-name-len recipient)
    (error "recipient does not satisfy predicate n-or-lessp. Recipient: ~A~%Length: ~A" recipient (length recipient)))
  (make-instance 'packet :recipient recipient :op op))
(defun build-kill-packet ()
  (let ((packet (build-packet %kill-recipient %op-kill)))
    (change-class packet 'kill-packet)))
(defun build-identify-packet (id)
  (unless (n-or-lessp %connection-name-len id)
    (error "id does not satisfy predicate n-or-lessp. ID: ~A~%Length: ~A" id (length id)))
  (let ((packet (build-packet %identify-recipient %op-identify)))
    (change-class packet 'identify-packet)
    (setf (id packet) id)
    packet))
(defun build-data-packet (recipient data)
  (let ((packet (build-packet recipient %op-data))
        (len (length data)))
    (unless (<= len %max-data-size)
      (error "Data is longer than ~A which is the current hard limit on data size. Length: ~A" %max-data-size (length data)))
    (change-class packet 'data-packet)
    (setf (d-len packet) len)
    (setf (data packet) data)
    packet))


;;;;
(defmethod build-data-packets (recipient (data string))
  (let* ((start (vectorize-data (concatenate 'string %start-header %op-data)))
         (recipient (vectorize-data recipient  %connection-name-len))
         (len (make-array 1 :element-type '(unsigned-byte 8) :initial-element (length data)))
         (end (vectorize-data (concatenate 'string data %stop-footer)))         
         (arr (concatenate '(vector (unsigned-byte 8)) start recipient len end)))
    (f-format t "data: ~s~%" arr)
    (if (validate-length arr)
        arr
        (error "Packet is too large so dropping. Length: ~A~%" (length arr)))))

(defmethod build-data-packets (recipient (data list))
  (build-data-packets recipient (list-to-string data)))
(defmethod build-data-packets (recipient data)
  (error "No generic method exists for the type of data supplied: ~A~%" (type-of data)))

(defun validate-length (data)
  "max length is 255 plus the length of the headers and op code. 255 is because only one byte is used to tell the client the length of the data coming.";;if I wanted to have more than 255 I could
  (<= (length data) 255))



(defmethod packet-download-function ((obj client))
  "Keeps calling the function download-sequence until the thread is manually killed"
  (with-accessors ((functions packet-processors-functions))
      obj
    (loop :for packet := (download-sequence obj) :then (download-sequence obj)
          :do (process-packet obj packet))))
(defmethod packet-process ((obj client) (packet connection) keyword)
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

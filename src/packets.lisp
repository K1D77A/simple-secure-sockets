
(in-package :simple-secure-sockets)


(defvar %start-header "start")
(defvar %op-data "d")
(defvar %op-kill "k")
(defvar %stop-footer "stop")



(defmethod download-sequence ((obj client))
  "Method that handles downloading a complete sequence"
  (let ((packet (make-instance 'packet)))
    (read-header obj packet)
    (read-op obj packet)
    (handle-op obj packet)
    (read-footer obj packet)
    packet))

(defmethod read-header :before ((obj client) (packet packet))
  (f-format t "New packet start~s~%-Reading header~%"(get-universal-time)))
(defmethod read-header :after ((obj client) (packet packet))
  (f-format t "-Header read~%"))
(defmethod read-header ((obj client) (packet packet))
  (setf (header packet)
        (byte-vector-to-string (read-n-bytes-with-leds
                                (length %start-header) (c-stream obj)))))
(defmethod read-op :before ((obj client)(packet packet))
  (f-format t "--Reading op~%"))
(defmethod read-op :after ((obj client)(packet packet))
  (f-format t "--OP read~%"))
(defmethod read-op ((obj client)(packet packet))
  (let ((op (read-byte-with-leds (c-stream obj))))
    (setf (op packet)(intern (string (code-char op)) :keyword))
    (if (equal (op packet) (intern %op-data :keyword))
        (change-class packet 'data-packet)
        (change-class packet 'kill-packet))))

(defmethod handle-op :before ((obj client)(packet packet))
  (f-format t "--Handling OP~%"))
(defmethod handle-op :after ((obj client)(packet packet))
  (f-format t "--OP Handled~%"))
(defmethod handle-op ((obj client)(packet kill-packet))
  :SHUTDOWN)
(defmethod handle-op :after ((obj client)(packet data-packet))
  (f-format t "---Data: ~s~%" (data packet)))
(defmethod handle-op ((obj client)(packet data-packet))
  "Thisn here handles the op code 'd' by downloading the correct amount of data and placing it in the 
correct place in the packet"
  (let* ((stream (c-stream obj))
         (len (read-byte-with-leds stream))
         (bytes (read-n-bytes-with-leds len stream))
         (data-string (byte-vector-to-string bytes)))
    (setf (d-len packet) len
          (data packet) data-string)))


(defmethod read-footer :before ((obj client)(packet packet))
  (f-format t "-reading footer~%"))
(defmethod read-footer :after ((obj client)(packet packet))
  (f-format t "-footer read~%Packet End!~%"))
(defmethod read-footer ((obj client)(packet packet))
  (setf (footer packet)
        (byte-vector-to-string (read-n-bytes-with-leds (length %stop-footer) (c-stream obj)))))

(defmethod build-data-packets ((data string))
  (let* ((start (vectorize-data (concatenate 'string %start-header %op-data)))
         (len (make-array 1 :element-type '(unsigned-byte 8) :initial-element (length data)))
         (end  (vectorize-data (concatenate 'string data %stop-footer)))         
         (arr (concatenate '(vector (unsigned-byte 8)) start len end)))
    (f-format t "data: ~s~%" arr)
    (if (validate-length arr)
        arr
        (error "Packet is too large so dropping. Length: ~A~%" (length arr)))))

(defmethod build-data-packets ((data list))
  (build-data-packets (list-to-string data)))
(defmethod build-data-packets (data)
  (error "No generic method exists for the type of data supplied: ~A~%" (type-of data)))

(defun validate-length (data)
  "max length is 255 plus the length of the headers and op code. 255 is because only one byte is used to tell the client the length of the data coming.";;if I wanted to have more than 255 I could
  (<= (length data)
      (+ 255
         (length %start-header)
         (length %op-kill)
         (length %stop-footer))))

(defun build-kill-packets ()
  (vectorize-data (concatenate 'string %start-header %op-kill  %stop-footer)))

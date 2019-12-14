;;;; this file contains the client which connects to the discord bot over a websocket
;;;;it reads the data and then works the leds and the oled display
(in-package #:simple-secure-sockets)
;;;only need to support a single connection as there is only 1 rasp pi


(defvar %start-header "start")
(defvar %op-data "d")
(defvar %op-kill "k")
(defvar %stop-footer "stop")
(defparameter *op-keywords* (list :DATA :KILL))
(defparameter *DEBUG-OUTPUT* t)

(defparameter *ip* "")
(defparameter *port* 12345)

(defvar *current-clients* (make-hash-table))

(defclass client ()
  ((ip :type string :accessor ip :initarg :ip)
   (port :type integer :accessor port :initarg :port)
   (socket :accessor c-socket :initform :socket-not-set)
   (stream :accessor c-stream :initform :stream-not-set)
   (packet-processor-functions :accessor ppf :initform (make-hash-table))
   (processor-name :accessor processor-name :initform :name-of-processor-thread-not-set))
  (:documentation "class containing the slots required for the client"))
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
(defun start (name ip port)
  "Short version of make client"
  (make-client name ip port))
(defun stop (name)
  "Disconnects a client based on its name"
  (let ((client (gethash name *current-clients*)))
    (shutdown-client client)))

(defun get-client (name)
  (gethash name *current-clients*))

(defmethod packet-download-function ((obj client))
  "Keeps calling the function download-sequence until the thread is manually killed"
  (with-accessors ((functions packet-processors-functions))
      obj
    (loop :for packet := (download-sequence obj) :then (download-sequence obj)
          :do (process-packet obj packet))))

(defmethod process-packet ((obj client)(packet data-packet))
  "Processes the data-packets for client. It calls all the functions that are contained within a list under the key :DATA in the slot 'packet-processor-functions' with the argument packet. If you destructively modify packet then any functions after will be passed the modified version of packet"
  (with-accessors ((functions-hash ppf))
      obj
    (let ((functions (gethash :DATA functions-hash)));;this possible here that instead of having
      (mapcar (lambda (func);;functions that dispatch on an OP code I could instead dispatch on
                (funcall func packet));;the class name ie (gethash 'data-packet ..) then when
              functions))));;a new function needs to be added only the class name needs to be
;;entered into the function 

(defmethod process-packet ((obj client)(packet kill-packet))
  (shutdown-client obj))

(defmethod dispatch-on-op ((obj client) op function)
  (if (and (keywordp op) (functionp function) (find op *op-keywords*))
      (push function (gethash op (ppf obj)))
      (error "Either op is not a keyword or is not valid see *op-keywords* or function is not a function like #'. OP: ~s~%Func: ~s~%Valid OPs: ~S"
             (type-of op)
             (type-of function)
             *op-keywords*)))


(defmethod print-object ((object client) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~%Address: ~A:~A~%Socket: ~A~%Stream: ~A~%"
            (ip object)
            (port object)
            (c-socket object)
            (c-stream object))))
(defmethod print-object ((object data-packet) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~%Data: ~s~%"
            (data object))))
(defun f-format (destination control-string &rest format-arguments)
  "just a normal format function that forces output afterwards"
  (when *debug-output*    
    (format destination control-string format-arguments)
    (force-output destination)))

(defun make-client (name ip port)
  (unless (keywordp name)
    (error "Name should be a keyword: ~s" name))
  (setup-i2c)
  (let ((client (make-instance 'client :ip ip :port port)))
    (handler-case (connect-to-server client)
      (serious-condition (c) (progn (format t "Error of some sort oof: ~s~%" c )
                                    (display-i2c "Connection Failure!")
                                    (unless (equal (c-socket client) :socket-not-set)
                                      (usocket:socket-close (c-socket client)))
                                    client)))
    (setf (gethash name *current-clients*) client)
    (setf (processor-name client) name)
    (let ((bt:*default-special-bindings*(acons '*standard-output* *standard-output*
                                               bt:*default-special-bindings*)))
      (bt:make-thread (lambda () (packet-download-function client))
                      :name (processor-name client)))
    (packet-download-function client)
    client))
(defun set-threads-to-std-out ()
  (setf bt:*default-special-bindings*;;this sets the var of standard out for the threads
        (acons '*standard-output* *standard-output*
               bt:*default-special-bindings*)))
(defmethod connect-to-server :before ((obj client))
  (f-format t "Attempting to connect to host: ~s ~%" (ip obj)))
(defmethod connect-to-server :after ((obj client))  
  (f-format t "Connection successful~%"))

(defmethod connect-to-server ((obj client))
  (with-accessors ((ip ip)
                   (port port)
                   (socket c-socket)
                   (stream c-stream))
      obj
    (let ((connect (usocket:socket-connect ip port
                                           :protocol :stream
                                           :element-type '(unsigned-byte 8))))
      (setf socket connect)
      (setf stream (usocket:socket-stream connect)))))

(defun find-and-kill-thread (name)
  "finds and kills the thread 'name'"
  (let ((threads (bt:all-threads)))
    (mapcar (lambda (thread)
              (when (equal (bt:thread-name thread)
                           name)
                (bt:destroy-thread thread)))
            threads)))
(defmethod shutdown-client :before ((obj client))
  (display-i2c "Shutting down")
  (f-format t "Attempting to shutdown client connection to ~s~%" (ip obj)))
(defmethod shutdown-client :after ((obj client))
  (display-i2c "Shutdown complete")
  (f-format t "Shutdown complete~%"))
(defmethod shutdown-client ((obj client))
  "shuts down the connection on server"
  (find-and-kill-thread (processor-name obj))
  (usocket:socket-close (c-socket obj)))


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


(defun byte-vector-to-chars (byte-vector)
  "Takes in a byte vector of char codes and converts it to the original list. displays each byte on the leds by default"
  (let ((array (make-array (length byte-vector))))
    (map-into array (lambda (byte)                      
                      (code-char byte))
              byte-vector)
    array))
(defun byte-vector-to-string (byte-vector)
  (coerce (byte-vector-to-chars byte-vector) 'string))

(defun chars-sequence-to-list (chars-sequence)
  (read-from-string (coerce chars-sequence 'string)))








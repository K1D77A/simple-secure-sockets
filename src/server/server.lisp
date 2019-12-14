;;;;this file contains the server for the bot
(in-package :simple-secure-sockets)
;;(defparameter *chat-handler* (clack:clackup #'bot-server
;;:address "172.31.87.246";aws local ip
;;:port 12345))
;;;need to store the people who be connected n shiz although there will only be one
;;;for now will just use a single connection

(defvar %start-header "start")
(defvar %op-data "d")
(defvar %op-kill "k")
(defvar %stop-footer "stop")

(defparameter *DEBUG-OUTPUT* t)

(defparameter *ip* "127.0.0.1")
(defparameter *port* 12345)
(defparameter *current-servers* (make-hash-table))

(defclass server ()
  ((ip :type string :accessor ip :initarg :ip)
   (port :type integer :accessor port :initarg :port)
   (socket :accessor s-socket :initform :server-socket-not-set)
   (stream :accessor s-stream :initform :server-stream-not-set)))
;;we will associate an instance of the bot with our server
;;however the bots from discord can support thousands, this just makes it easy to shut it down
;;when the server shuts down
(defun make-server (name ip port)
  (unless (keywordp name)
    (error "Name should be a keyword: ~s" name))
  (let ((server (make-instance 'server :ip ip :port port)))
    (handler-case (progn (set-server-socket server)
                         (wait-for-client server))
      (serious-condition (c) (progn (format t "Error of some sort oof: ~s~%" c)
                                    (unless
                                        (equal (s-socket server)
                                               :server-socket-not-set)
                                      (usocket:socket-close (s-socket server)))
                                    server)))
    (start-klambda-bot server)
    (add-bot-in-hook server)
    (print-object server t)
    (setf (gethash name *current-servers*) server)
    server))
(defun start (name ip port)
  (make-server name ip port))
(defun stop (name)
  "stops a server based on its name"
  (let ((server (gethash name *current-servers*)))
    (shutdown-server server)))
(defun f-format (destination control-string &rest format-arguments)
  "just a normal format function that forces output"
  (when *DEBUG-OUTPUT*
    (format destination control-string format-arguments)
    (force-output destination)))

(defmethod print-object ((object server) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~%Address: ~A:~ABot: ~A~%Socket: ~A~%Stream: ~A~%"
            (ip object)
            (port object)
            (bot object)
            (s-socket object)
            (s-stream object))))

(defmethod shutdown-server :before ((obj server))
  (f-format t "Attempting to shutdown server and Klambda bot~%"))
(defmethod shutdown-server :after ((obj server))
  (f-format t "Shutdown complete~%"))
(defmethod shutdown-server ((obj server))
  "shuts down the connection on server"
  (send-kill obj)
  (sleep 1)
  (usocket:socket-close (s-socket obj)))

(defmethod set-server-socket :before ((obj server))
  (f-format t "Creating socket~%"))
(defmethod set-server-socket :after ((obj server))
  (f-format t "Socket created: ~A~%" (s-socket obj)))
(defmethod set-server-socket ((obj server))
  (setf (s-socket obj)
        (usocket:socket-listen  (ip obj)
                                (port obj)
                                :element-type '(unsigned-byte 8)
                                :reuse-address t
                                :reuseaddress t)))

(defmethod wait-for-client :before ((obj server))
  (f-format t "Waiting on socket: ~A for a connection from the client~%" (s-socket obj)))
(defmethod wait-for-client :after ((obj server))
  (f-format t "Completed the connection with: ~A on port: ~A" 
            (usocket:get-peer-address (s-socket obj))
            (usocket:get-peer-port (s-socket obj))))
(defmethod wait-for-client ((obj server))
  "takes the obj and waits until it has a connection and then sets the stream"
  (let ((wait (usocket:socket-accept (s-socket obj))))
    (setf (s-stream obj) (usocket:socket-stream wait))))


;;;very simple protocol to send data across the network
;;;send message saying starting then data type first either data or kill then length first and then the data then a stopping
;;;ie
;;;start = #(115 116 97 114 116)
;;;op data = #(100)
;;;op kill = #(107)
;;;len data = #(4)
;;;data = #(1 2 3 4)
;;;stop =  #(115 116 111 112)
;;;complete transfer = #(114 116 97 114 116 100 4 1 2 3 4 115 116 111 112)
;;;couple examples if data is '(1 2 3 4) then as a string the data is = "startd9(1 2 3 4)stop"
;;;as a byte-array #(115 116 97 114 116 100 57 40 49 32 50 32 51 32 52 41 115 116 111 112)
;;;kill is "startkstop"
;;;as a sequence: #(115 116 97 114 116 107 115 116 111 112)
;;;hard limit on sequence size is going to be 255 + header and footer bytes



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

(defmethod vectorize-data ((data string))
  "takes in a string and converts it to an array of type '(unsigned-byte 8)"
  (let ((arr (make-array (length data) :element-type '(unsigned-byte 8))))
    (map-into arr #'char-code data)))

(defun validate-length (data)
  "max length is 255 plus the length of the headers and op code. 255 is because only one byte is used to tell the client the length of the data coming.";;if I wanted to have more than 255 I could
  (<= (length data)
      (+ 255
         (length %start-header)
         (length %op-kill)
         (length %stop-footer))))

(defun build-kill-packets ()
  (vectorize-data (concatenate 'string %start-header %op-kill  %stop-footer)))

(defmethod send-data (data (obj server))
  "sends the data in the form of a byte array over the network to the client"
  (with-accessors ((connection s-stream))
      obj
    (let ((seq (build-data-packets data)));;build data handles converting between data types
      (write-sequence seq connection)
      (force-output connection))))
(defmethod send-kill ((obj server))
  (with-accessors ((connection s-stream))
      obj
    (write-sequence (build-kill-packets) connection)
    (force-output connection)))

(defun list-to-string (lst)
  "converts a list to a string"
  (format nil "~s" lst))

;;;; this file contains the client which connects to the discord bot over a websocket
;;;;it reads the data and then works the leds and the oled display
(in-package #:simple-secure-sockets)
;;;only need to support a single connection as there is only 1 rasp pi





(defparameter *ip* "")
(defparameter *port* 12345)

(defvar *current-clients* (make-hash-table))

;; (defun start (name ip port)
;;   "Short version of make client"
;;   (make-client name ip port))
;; (defun stop (name)
;;   "Disconnects a client based on its name"
;;   (let ((client (gethash name *current-clients*)))
;;     (shutdown-client client)))

(defun start-client (name ip &optional (connect-port 55555))
  (if (unique-key-p *current-clients* name)
      (setf (gethash name *current-clients*)
            (make-client name ip connect-port))
      (error "name is not a unique name")))
(defun stop-client (name)
  (let ((client (gethash name *current-clients*)))
    (shutdown client)
    (remhash client *current-clients*)))




(defun make-client (name ip port)
  (unless (stringp name)
    (error "Name should be a keyword: ~s" name))
  (let ((client (make-instance 'client :ip ip :port port :connection-name name)))
    (handler-case (connect client)
      (serious-condition (c) (progn (format t "Error of some sort oof: ~s~%" c )
                                    (print (c-socket client))
                                    (unless (equal (c-socket client) :socket-not-set)
                                      (usocket:socket-close (c-socket client)))
                                    client)))
    (set-threads-to-std-out)
    (setf (packet-download-function client)
          (bt:make-thread (lambda () (packet-download-function client))
                          :name  (format nil "client-~A-download-function" (processor-name client))))
    client))

(defmethod connect :before ((obj client))
  (f-format t "Attempting to connect to host: ~s ~%" (ip obj)))
(defmethod connect :after ((obj client))  
  (f-format t "Connection successful~%"))

(defmethod connect ((obj client))
  (with-accessors ((ip ip)
                   (port port)
                   (socket c-socket)
                   (stream c-stream)
                   (name connection-name))
      obj
    (print-object obj t)
    (let ((connect (usocket:socket-connect ip port
                                           :protocol :stream
                                           :element-type '(unsigned-byte 8))))
      (setf socket connect)
      (setf stream (usocket:socket-stream connect))
      (sleep 1)
      (f-format t "CLIENT SENDING IDENTIFY~%")
      (send obj (build-identify-packet name))
      (f-format t "CLIENT SENT IDENTIFY~%"))));send identify packet
;; (let* ((packet (download-sequence obj)) ;get back confirmation
;;        (data (data packet)))
;;   (if (string= data "Success")
;;       (f-format t "Connection successful~%")
;;       (progn (f-format t "Connection failed~%")
;;              (shutdown obj)))))))


(defmethod shutdown :before ((obj client))
  (f-format t "Attempting to shutdown client connection to ~s~%" (ip obj)))
(defmethod shutdown :after ((obj client))
  (f-format t "Shutdown complete~%"))
(defmethod shutdown ((obj client))
  "shuts down the connection on server"
  (bt:destroy-thread (packet-download-function obj))
  ;;need to send a kill packet
  (usocket:socket-close (c-socket obj))
  (remhash (connection-name obj) *current-clients*))







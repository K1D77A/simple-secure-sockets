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

(defun get-client (name)
  (gethash name *current-clients*))



(defun make-client (name ip port)
  (unless (stringp name)
    (error "Name should be a keyword: ~s" name))
  (let ((client (make-instance 'client :ip ip :port port)))
    (handler-case (connect client)
      (serious-condition (c) (progn (format t "Error of some sort oof: ~s~%" c )
                                    (print (c-socket client))
                                    (unless (equal (c-socket client) :socket-not-set)
                                      (usocket:socket-close (c-socket client)))
                                    client)))
    (setf (gethash name *current-clients*) client
          (connection-name client) name
          (processor-name client) name)
    (let ((bt:*default-special-bindings*(acons '*standard-output* *standard-output*
                                               bt:*default-special-bindings*)))
      (bt:make-thread (lambda () (packet-download-function client))
                      :name  (processor-name client)))   
    client))
(defun set-threads-to-std-out ()
  (setf bt:*default-special-bindings*;;this sets the var of standard out for the threads
        (acons '*standard-output* *standard-output*
               bt:*default-special-bindings*)))
(defmethod connect :before ((obj client))
  (f-format t "Attempting to connect to host: ~s ~%" (ip obj)))
(defmethod connect :after ((obj client))  
  (f-format t "Connection successful~%"))

(defmethod connect ((obj client))
  (with-accessors ((ip ip)
                   (port port)
                   (socket c-socket)
                   (stream c-stream))
      obj
    (print-object obj t)
    (let ((connect (usocket:socket-connect ip port
                                           :protocol :stream
                                           :element-type '(unsigned-byte 8))))
      (setf socket connect)
      (setf stream (usocket:socket-stream connect))
      (send-identify (connection-name obj) obj);send identify packet
      (let* ((packet (download-sequence obj)) ;get back confirmation
             (data (data packet)))
        (if (string= data "Success")
            (f-format t "Connection successful~%")
            (progn (f-format t "Connection failed~%")
                   (shutdown obj)))))))

(defun find-and-kill-thread (name)
  "finds and kills the thread 'name'"
  (let ((threads (bt:all-threads)))
    (mapcar (lambda (thread)
              (when (equal (bt:thread-name thread)
                           name)
                (bt:destroy-thread thread)))
            threads)))
(defmethod shutdown :before ((obj client))
  (f-format t "Attempting to shutdown client connection to ~s~%" (ip obj)))
(defmethod shutdown :after ((obj client))
  (f-format t "Shutdown complete~%"))
(defmethod shutdown ((obj client))
  "shuts down the connection on server"
  (find-and-kill-thread (processor-name obj))
  (usocket:socket-close (c-socket obj))
  (remhash (processor-name obj) *current-clients*))







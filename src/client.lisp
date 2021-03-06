;;;; this file contains the client which connects to the discord bot over a websocket
;;;;it reads the data and then works the leds and the oled display
(in-package #:simple-secure-sockets)
;;;only need to support a single connection as there is only 1 rasp pi


(defparameter *ip* "")
(defparameter *port* 12345)
(defparameter *client-fails* nil)


(defvar *current-clients* (make-hash-table :test #'equal))

;; (defun start (name ip port)
;;   "Short version of make client"
;;   (make-client name ip port))
;; (defun stop (name)
;;   "Disconnects a client based on its name"
;;   (let ((client (gethash name *current-clients*)))
;;     (shutdown-client client)))

(defun start-client (name ip &optional (connect-port 55555))
  "takes a name and ip, then attempts to connect the client to an already running server, if it
fails then will return :NOT-CONNECTED, otherwise returns the client object"
  (if (unique-key-p *current-clients* name)
      (setf (gethash name *current-clients*)
            (make-client name ip connect-port))
      (error "name is not a unique name")))

(defun stop-client (name)
  "Stops the client by 'name' and disconnects it from the server"
  (let ((client (gethash name *current-clients*)))
    (shutdown client t)
    (remhash client *current-clients*)))

(defun stop-all-clients ()
  "Stops all connected clients"
  (maphash (lambda (key val)
             (declare (ignore key))
             (shutdown val))
           *current-clients*)
  (setf *current-clients* (make-hash-table :test #'equal)))

(defmethod try-connect ((obj client) &optional (retry-time 1)(retry-attempts 3))
  "Attempts to connect to a server. Attempts retry-attempts time and sleeps retry-time between 
each attempt"
  (let ((failed-to-connect 0))
    (handler-bind ((USOCKET:CONNECTION-REFUSED-ERROR
                     (lambda (c)
                       (declare (ignore c))
                       (forced-format t "failed to connect~%")
                       (if (= failed-to-connect retry-attempts)
                           (invoke-restart 'use-value :NOT-CONNECTED)
                           (progn (incf failed-to-connect)
                                  (invoke-restart 'sleep-for-x retry-time))))))
      (connect obj))))

(defun make-client (name ip port &optional (retry-time 1)(retry-attempts 3))
  "creates a client object and attempts to connect it to a listening server. if the client 
fails to connect it will attempt retry-attempts more times and sleep retry-time between each attempt"
  (unless (stringp name)
    (error "Name should be a string: ~s" name))
  (let ((client (make-instance 'client :ip ip :port port :connection-name name))
        (con? :NOT-CONNECTED))
    (handler-case (progn (setf con? (try-connect client retry-time retry-attempts))
                         (when (equal con? :NOT-CONNECTED)
                           (shutdown client)))
      (serious-condition (c) (progn (f-format :error :client-start "Client error: ~s" c)
                                    (safe-socket-close (c-socket client))
                                    client)))
    (if (equal con? :NOT-CONNECTED)
        (progn (f-format :info :client-start  "client failed")
               :NOT-CONNECTED)
        (progn (start-packet-download client)
               (setf (connectedp client) t)
               (f-format :debug :client-start "returning client")
               client))))

(defmethod start-packet-download ((obj client))
  (setf (packet-download-thread obj)
        (make-thread (lambda () (packet-download-function obj))
                     :name (format nil "[~A]:packet-download" (connection-name obj)))))

(defmethod packet-download-function ((obj client))
  "Keeps calling the function download-sequence until the thread is manually killed. If the thread receives an :EOF from download-sequence it will simply return :EOF"
  (let ((stream (c-stream obj)))
    (loop
      :if (handler-case (listen stream)
            (stream-error ()
              (return :EOF)))
        :do (let ((packet (download-and-parse obj)))
              ;; (forced-format t "~&client: ~A~%" (connection-name obj))
              (if (equal packet :EOF)
                  (return :EOF)
                  (push-correct-queue obj packet)))
      :else
        :do (sleep 0.001))))

(defmethod push-to-queue (packet queue)
  "pushes all the packets received to the queue"
  ;;(forced-format t "pushing type: ~A to queue~%" (type-of packet))
  (lparallel.queue:push-queue packet queue))

(defmethod push-correct-queue ((obj client)(packet data-packet))
  "Pushes the packet to the correct queue based on the type of packet. data-packets go into (gethash (sender packet) (data-packet-queues obj)), which is a specific queue identified by client names. other packets which are server related packets simply go into the queue in (packet-queue obj)"
  (let* ((c-sender (sender* packet))
         (queue (gethash c-sender (data-packet-queues obj))))
    (if queue ;;when it is not null
        (push-to-queue packet  queue)
        (let ((queue (lparallel.queue:make-queue)))
          (push-to-queue packet queue)
          (setf (gethash c-sender (data-packet-queues obj)) queue)))))

(defmethod push-correct-queue ((obj client) packet)
  ;;(forced-format t "~&packet: ~A~%" packet)
  (handle-packet obj packet))

(defun sleep-for-x (x)
  (invoke-restart 'sleep-for-x x))

(defmethod s-connect ((obj client))
  (with-accessors ((ip ip)
                   (port port))
      obj
    (restart-case
        (usocket:socket-connect ip port
                                :protocol :stream
                                :element-type '(unsigned-byte 8))
      (sleep-for-x (x) (progn (sleep x)(s-connect obj)))
      (use-value (value) value))))

(defmethod connect :before ((obj client))
  (f-format :info :client-connect  "Attempting to connect to host: ~s" (ip obj)))
(defmethod connect :after ((obj client))  
  (f-format :info :client-connect  "Connection successful"))
(defmethod connect ((obj client))
  "Is used to connect the client to the server, returns either :CONNECTED or :NOT-CONNECTED depending
on whether it is successful or not"
  (with-accessors ((ip ip)
                   (port port)
                   (socket c-socket)
                   (stream c-stream)
                   (name connection-name))
      obj
    (let ((connect (s-connect obj)))
      ;; (forced-format t "connect: ~A~%" connect)
      (if (equal connect :NOT-CONNECTED)
          connect
          (progn 
            (setf socket connect)
            (setf stream (usocket:socket-stream connect))
            (send obj (build-identify-packet name))
            (let ((packet (download-and-parse obj)))
              (f-format :debug :client-receive "------ack maybe received ~A-------"
                        (type-of packet))
              (if (equal (type-of packet) 'ack-packet)
                  :CONNECTED
                  :NOT-CONNECTED)))))))

(defmethod shutdown :before ((obj client) &optional send-killp)
  (declare (ignore send-killp))
  (f-format :info :client-stop  "Attempting to shutdown client connection to ~s" (ip obj)))

(defmethod shutdown :after ((obj client) &optional send-killp)
  (declare (ignore send-killp))
  (f-format :info :client-stop  "Shutdown complete"))

(defun shutdown-client (client &optional (send-killp nil))
  "shuts down the connection on server"
  (stop-thread (packet-download-thread client))
  (setf (connectedp client) nil)
  (when send-killp
    (send client (build-kill-packet)))
  ;;need to send a kill packet
  (safe-socket-close (c-socket client));;this throws and end of file for some reason.
  (remhash (connection-name client) *current-clients*))

(defmethod shutdown ((obj client) &optional  (send-killp t))
  "shuts down the connection on server"
  (shutdown-client obj send-killp))
(defmethod shutdown ((obj client) &optional  (send-killp nil))
  "shuts down the connection on server"
  (shutdown-client obj send-killp))






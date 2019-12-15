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


(defun make-client (name ip port)
  (unless (keywordp name)
    (error "Name should be a keyword: ~s" name))
  (let ((client (make-instance 'client :ip ip :port port)))
    (handler-case (connect client)
      (serious-condition (c) (progn (format t "Error of some sort oof: ~s~%" c )
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
(defmethod shutdown :before ((obj client))
  (display-i2c "Shutting down")
  (f-format t "Attempting to shutdown client connection to ~s~%" (ip obj)))
(defmethod shutdown :after ((obj client))
  (display-i2c "Shutdown complete")
  (f-format t "Shutdown complete~%"))
(defmethod shutdown ((obj client))
  "shuts down the connection on server"
  (find-and-kill-thread (processor-name obj))
  (usocket:socket-close (c-socket obj)))







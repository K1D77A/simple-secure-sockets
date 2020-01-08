;;;;this file contains a series of tests I have performed to make sure things are working okay

(in-package :simple-secure-sockets)
(defun test (packet)
  (f-format :debug :testing "TEST~%Packet= ~A~%" packet))
(defun test-sending-to-client (port)
  (let ((server nil)
        (client nil))
    (handler-case (progn (setf server (make-server "server" "127.0.0.1" port))
                                        ; (sleep 0.1)
                         (setf client (make-client "client" "127.0.0.1" port))
                                        ;  (sleep 1)
                         (let ((server-con-to-client
                                 (get-current-connections-object server
                                                                 (connection-name client))))
                           (send server-con-to-client (build-data-packet (connection-name client)
                                                                         "beep boop im groot")))
                         
                         (shutdown server)
                         (sleep 1)
                         (shutdown client)
                         (values client server (packet-queue client)))
      (serious-condition (c)
        (f-format :error :testing "condition: ~A" c)
        (shutdown server)
        (shutdown client)
        (values client server)))))
;;need to catch errors and shut all down etc
(defun test-large-amount-of-connections (n port)
  (let ((server nil)
        (clients nil))
    (handler-case
        (progn (setf server (make-server "server" "127.0.0.1" port))
               (setf clients (loop :for x :from 0 :to n
                                   :collect (make-client (format nil "client~d" x)
                                                         "127.0.0.1"
                                                         port)))
               (let ((x 0))
                 (mapcar (lambda (client)
                           (send (get-current-connections-object server
                                                                 (connection-name client))
                                 (build-data-packet (connection-name client)
                                                    (format nil "here is some data client~d" x)))
                           (incf x))
                         clients))
               (sleep 1)
                                        ; (print (bt:all-threads))
               (shutdown server)
               (mapcar #'shutdown clients)
               server)
                                        ;  (values clients server))
      (serious-condition (c)
        (f-format :error :testing "condition: ~A" c)
        (mapcar #'shutdown clients)
        (shutdown server)
        server))))

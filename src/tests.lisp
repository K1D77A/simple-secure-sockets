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
                           (send server-con-to-client (build-data-packet (name server)
                                                                         (connection-name client)
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

(defun connect-n-clients (n port threads queues)
  (let ((server)
        (clients))
    (handler-case
        (progn 
          (setf server (make-server "server" "127.0.0.1" port threads queues))
          (format t "~&Server created~%")
          (setf clients (loop :for x :from 1 :to n
                              :collect (make-client (format nil "client~d" x)
                                                    "127.0.0.1"
                                                    port)))
          (forced-format t "~&~A Clients connected sleeping 0.5 seconds~%" (length clients))
          (sleep 2)
          (values server clients))
      (serious-condition ()
        (forced-format t "~&Connection failed~%")
        (mapcar #'shutdown clients)
        (shutdown server)
        (values :FAILED :FAILED)))))


;;need to catch errors and shut all down etc
(defun test-large-amount-of-connections (n port threads queues)
  ;; (declare (optimize (speed 3)(safety 1)))
  (setf (first *moved-packets*) 0)
  (multiple-value-bind (server clients)
      (connect-n-clients n port threads queues)
    (unless (equal server :FAILED)
      ;; (mapcar (lambda (client)
      ;;           (mapcar (lambda (available)
      ;;                     (unless (equal available :AVAILABLE-CLIENTS)
      ;;                       ;;   (forced-format t "Sending to: ~A~%" available)
      ;;                       ;;   (forced-format t "from: ~A~%" (connection-name client))
      ;;                       ;;                                    (connection-name client))
      ;;                       (send client (build-data-packet available "oof"))))
      ;;                   (available-clients client)))
      ;;         clients)
      ;; (sleep 2)
      ;; (print (done-processing-p server))
      ;; (print (all-connection-streams-empty-p server))
      (when (shutdown-connected-server-and-clients server clients)
        (values clients server)))))

(defun shutdown-connected-server-and-clients (server clients &optional (time))
  (declare (ignore time))
  (loop :while (not (or (all-connection-streams-empty-p server)
                        (done-processing-p server)))
        :do (sleep 0.01)
        :finally (sleep 1)
                 (handler-case (progn ;;(mapcar #'shutdown clients)
                                 (shutdown server t)
                                 (return t))
                   (error ()
                     (shutdown server)
                     (return t)))))

(defun test-large-amount-of-packets (n-packets n-clients port threads queues)
  ;; (declare (optimize (speed 3)(safety 1)))
  (setf (first *moved-packets*) 0)
  (multiple-value-bind (server clients)
      (connect-n-clients n-clients port threads queues)
    (let ((now (get-internal-run-time)))
      (if (equal server :FAILED)
          :failed
          (progn (gen-and-send-packets clients n-packets)
                 (when (loop :while (not (or (all-connection-streams-empty-p server)
                                             (done-processing-p server)))
                             :do (sleep 0.001)
                             :finally (setf now (- (get-internal-run-time) now))
                                      (sleep 10)
                                      (handler-case
                                          (progn ;;(mapcar #'shutdown clients)
                                            (sleep 1)
                                            (shutdown server t)
                                            ;;(sleep )
                                            (forced-format
                                             t "time: ~d"
                                             (float (/ now internal-time-units-per-second)))
                                            (return t))
                                        (error ()
                                          (shutdown server)
                                          (return t)))))
                 (values clients server))))))

(defmacro clean-shutdown-server-and-clients (server clients &body body)
  `(handler-case ,@body
     (serious-condition ()
       (mapcar #'shutdown ,clients)
       (shutdown ,server)
       ,server)))

(defun test-server (port thread q)
  (make-server "server" "127.0.0.1" port thread q))

(defun test-client (n port)
  (make-client (format nil "client~d" n) "127.0.0.1" port))

(defun send-to-clients-available (client message)
  (mapcar (lambda (cli)
            (unless (equal cli :AVAILABLE-CLIENTS)
              (send client (build-data-packet cli message))))
          (available-clients client)))

(defmethod n-random-recipients ((con client) n)
  "returns a cons whose car is the con and cdr is random list of recipients of length n assuming that
the available clients is length 2 or more"
  (let* ((clients (available-clients con))
         (len (1- (length clients)))) ;;:available-clients is the last client)
    (unless (= len 0)
      (cons con
            (loop :for x :from 1 :to n
                  :collect (elt clients (random len)))))))

(defmethod n-lots-of-x-random-recipients (list-of-clients n x)
  "takes in a list of clients, and creates a new list containing lists created by n-random-recipients"
  (let ((len (length list-of-clients)))
    (loop :for i :from 1 :to n
          :for cl := (elt list-of-clients (random len)) :then (elt list-of-clients (random len))
          ;;:do (forced-format t "~&oof ~A~%" cl)
          :when (equal (type-of cl) 'client)
            :collect (n-random-recipients cl x))))

(defun send-packets-to-recipients (list-genned)
  ;;  (print "sending")
  (lparallel:pmapcar (lambda (cons)
                       (let ((con (car cons))
                             (packets (cdr cons)))
                         ;;  (print "oof")
                         (mapcar (lambda (packet)
                                   (send con packet))
                                 packets)))
                     list-genned)
  t)

(defun gen-and-send-packets (clients n)
  (let* ((n-calc (the integer(ceiling (sqrt n))))
         (lst (generate-packets (n-lots-of-x-random-recipients clients n-calc n-calc))))
    (forced-format t "Sending ~d packets" n)
    (time (send-packets-to-recipients lst))))

(defun generate-packets (random-recipients-list)  
  (let ((data (make-string 10 :initial-element #\o)))
    (forced-format t "~&generating packets~%")
    (lparallel:pmapcar (lambda (lst)
                         (let ((con (car lst))
                               (recips (rest lst)))
                           (cons con
                                 (mapcar (lambda (recip)
                                           (build-data-packet recip data))
                                         recips))))
                       random-recipients-list)))



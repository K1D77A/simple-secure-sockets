(in-package :simple-secure-sockets)
(defun build-packet (recipient op)
  "takes in a recipient and creates an instance of packet"
  (let ((rec-vecced (vectorize-data recipient %connection-name-len))
        ;;(sender-vecced (vectorize-data sender %connection-name-len))
        (op-vecced (vectorize-data op))
        (foot-vecced (vectorize-data %stop-footer))
        (head-vecced (vectorize-data %start-header)))
    (unless (n-or-lessp %connection-name-len rec-vecced)
      (error "recipient does not satisfy predicate n-or-lessp. Recipient: ~A~%Length: ~A" recipient (length recipient)))
    (make-instance 'packet
                   ;;:sender sender-vecced
                   :recipient rec-vecced
                   :op op-vecced
                   :footer foot-vecced
                   :header head-vecced)))

(defun build-kill-packet ()
  (let ((packet (build-packet %kill-recipient %op-kill)))
    (change-class packet 'kill-packet)))
(defun build-ack-packet ()
  (let ((packet (build-packet %ack-recipient %op-ack)))
    (change-class packet 'ack-packet)))

(defun build-identify-packet (id)
  (let ((id-vecced (vectorize-data id %connection-name-len)))
    (unless (n-or-lessp %connection-name-len id)
      (error "id does not satisfy predicate n-or-lessp. ID: ~A~%Length: ~A" id (length id)))
    (let ((packet (build-packet %identify-recipient %op-identify)))
      (change-class packet 'identify-packet)
      (setf (id packet) id-vecced)
      packet)))

(defun build-data-packet (recipient data)
  (let* ((packet (build-packet recipient %op-data))
         (d-vecced (vectorize-data data))
         (len (length d-vecced)))
    (unless (<= len  %max-data-size)
      (error "Data is longer than ~A which is the current hard limit on data size. Length: ~A" %max-data-size len))
    (change-class packet 'data-packet)
    (setf (d-len packet) (make-array 1 :element-type '(unsigned-byte 8) :initial-element len))
    (setf (data packet) d-vecced)
    ;;   (setf (sender packet) sender-vecced)
    packet))
(defun build-clients-packet (client-name connected?)
  (if (or (= connected? 1)
          (= connected? 0))
      (let ((packet (build-packet %clients-recipient %op-clients))
            (client-name-vecced (vectorize-data client-name %connection-name-len)))
        (change-class packet 'clients-packet)
        (setf (connected? packet) (make-array 1 :element-type '(unsigned-byte 8)
                                                :initial-element connected?)
              (client-name packet) client-name-vecced)
        packet)
      (error "connected? is not 1 or 0. ~s" connected?)))

(defun force-write-sequence (seq stream)
  (write-sequence seq stream)
  (force-output stream))
;; (defmethod send :after (connection packet)
;;   (f-format t "~A sent type of packet ~A to ~A"
;;             (connection-name connection)
;;             (type-of packet)
;;             (recipient packet)))
(defmethod add-sender ((connection connection) (packet packet))
  (when (equal (sender packet) :SENDER-NOT-SET)
    ;;want to make sure that it has not be previously set, ie when it is being forwarded
    (let ((sender-vecced (vectorize-data (connection-name connection) %connection-name-len)))
      (setf (sender packet) sender-vecced))))
(defmethod send (connection (packet data-packet))
  (add-sender connection packet)
  (with-accessors ((recipient recipient)
                   (data data)
                   (len d-len)
                   (header header)
                   (footer footer)
                   (sender sender)
                   (op op))
      packet
    (force-write-sequence 
     (concatenate '(vector (unsigned-byte 8))
                  header recipient sender op len data footer)
     (c-stream connection)))
  packet)
(defmethod send (connection (packet kill-packet))
  (add-sender connection packet)
  (with-accessors ((recipient recipient)
                   (header header)
                   (footer footer)
                   (sender sender)
                   (op op))
      packet
    (force-write-sequence 
     (concatenate '(vector (unsigned-byte 8));;a better way to do this is to just
                  ;;map over each accessor which is of type byte 8 and then just send each one
                  ;;down the pipe
                  header recipient sender op footer)
     (c-stream connection))))

  (defmethod send (connection (packet identify-packet))
    (add-sender connection packet)
    (with-accessors ((recipient recipient)
                     (header header)
                     (id id)
                     (sender sender)
                     (footer footer)
                     (op op))
        packet
      (force-write-sequence 
       (concatenate '(vector (unsigned-byte 8))
                    header recipient sender op id footer)
       (c-stream connection))))

  (defmethod send (connection (packet ack-packet))
    (add-sender connection packet)
    (with-accessors ((recipient recipient)
                     (header header)
                     (footer footer)
                     (sender sender)
                     (op op))
        packet
      (force-write-sequence 
       (concatenate '(vector (unsigned-byte 8))
                    header recipient sender op footer)
       (c-stream connection))))
  (defmethod send (connection (packet clients-packet))
    (add-sender connection packet)
    (with-accessors ((recipient recipient)
                     (header header)
                     (footer footer)
                     (sender sender)
                     (op op)
                     (client-name client-name)
                     (connected? connected?))
        packet
      (force-write-sequence 
       (concatenate '(vector (unsigned-byte 8))
                    header recipient sender op client-name connected? footer)
       (c-stream connection))))

  (defmethod send-data-packet ((obj client) recipient data)
    (let ((clients (available-clients obj)))
      (when (member recipient clients :test #'string=)
        (let ((packet (build-data-packet recipient
                                         data)))
          (send obj packet)
          t))
      clients))



  (defmethod send-all-connected-clients ((obj server) connection)
    "sends all the currently connected clients to the client that has just connected"
    (maphash (lambda (key val)
               (declare (ignore val))
               ;;don't need val because the connection name is the same as key
               (send connection (build-clients-packet key 1)))
             (current-connections obj)))
  (defmethod update-all-clients-with-all-connected ((obj server))
    (maphash (lambda (key val)
               (declare (ignore key))
               (let ((con (car val)))
                 (maphash (lambda (key2 val2)
                            (declare (ignore val2))
                            (unless (string= (connection-name con)
                                             key2)
                              (send con (build-clients-packet key2 1))))
                          (current-connections obj))))
             (current-connections obj)))

  ;;map over the connections, then send all 
  
  (defun update-all-clients-with-connected?-client (server connection connectedp)
    (maphash (lambda (key val)
               (declare (ignore key))
               (let ((current-con (car val)))
                 (unless (string= (connection-name current-con) (connection-name connection))
                   (ignore-errors (send current-con (build-clients-packet (connection-name connection)
                                                                          (if connectedp 1 0)))))))
             ;;we do not want the function to fail to send to clients that are still connected, just
             ;;because one has disconnected in quick succession. 
             (current-connections server)))
  (defmethod update-all-clients-with-connected-client ((obj server) connection)
    (update-all-clients-with-connected?-client obj connection t))
  (defmethod update-all-clients-with-disconnected-client ((obj server) connection)
    (update-all-clients-with-connected?-client obj connection nil))

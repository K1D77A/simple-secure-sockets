;;;;THis file contains the methods to send an instance of 'packet and its subclasses over a network
;;;;connection to the server/client. 

(in-package :simple-secure-sockets)
(declaim (optimize (speed 3) (safety 1)))

(defun build-packet (recipient op)
  "takes in a recipient and creates an instance of packet"
  (tlet ((rec-vecced byte-array  (vectorize-data recipient %connection-name-len))
         (op-vecced byte-array (vectorize-data op))
         (foot-vecced byte-array (vectorize-data %stop-footer))
         (head-vecced byte-array (vectorize-data %start-header)))
    (unless (n-or-lessp %connection-name-len rec-vecced)
      (error "recipient does not satisfy predicate n-or-lessp. Recipient: ~A~%Length: ~A"
             recipient (length recipient)))
    (make-instance 'packet
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
  (tlet ((id-vecced  byte-array (vectorize-data id %connection-name-len)))
    (unless (n-or-lessp %connection-name-len id)
      (error "id does not satisfy predicate n-or-lessp. ID: ~A~%Length: ~A" id (length id)))
    (let ((packet (build-packet %identify-recipient %op-identify)))
      (change-class packet 'identify-packet)
      (setf (id packet) id-vecced)
      packet)))

(defun build-data-packet (recipient data)
  (tlet* ((packet packet (build-packet recipient %op-data))
          (d-vecced byte-array (vectorize-data data))
          (len integer (length d-vecced)))
         (unless (<= len  %max-data-size)
           (error "Data is longer than ~A which is the current hard limit on data size. Length: ~A" %max-data-size len))
         (change-class packet 'data-packet)
         (setf (d-len packet) (the single-byte
                                   (make-array 1 :element-type 'u-byte :initial-element len)))
         (setf (data packet) d-vecced)
         packet))

(defun build-clients-packet (client-name connected?)
  (declare (integer connected?))
  (if (or (= connected? 1)
          (= connected? 0))
      (tlet ((packet packet (build-packet %clients-recipient %op-clients))
             (client-name-vecced byte-array (vectorize-data client-name %connection-name-len)))
        (change-class packet 'clients-packet)
        (setf (connected? packet) (the single-byte (make-array 1 :element-type '(unsigned-byte 8)
                                                                 :initial-element connected?))
              (client-name packet) client-name-vecced)
        packet)
      (error "connected? is not 1 or 0. ~s" connected?)))

(defun write-byte-array (seq stream)
  "Takes in a byte-array and writes it to the stream. Each byte written is forced out after"
  (declare (optimize (speed 3)(safety 1)))
  (declare (byte-array seq))
  (tlet ((len fixnum (length seq)))
        (declare (fixnum len))
        (dotimes (i len t)
          (write-byte (the u-byte (aref seq i)) stream)
          (finish-output stream))))

(defun add-sender (connection packet)
  "Takes the sender from connection, turns it into a byte-array and puts into packet"
  (declare (optimize (speed 3)(safety 1)))
  (when (equal (sender packet) :SENDER-NOT-SET)
    ;;want to make sure that it has not be previously set, ie when it is being forwarded
    (tlet ((sender-vecced byte-array (vectorize-data
                                      (connection-name connection) %connection-name-len)))
          (setf (sender packet) sender-vecced)))
  packet)

(defun write-all-to-stream (stream &rest args)
  "Writes all of the args to stream and forces the output after"
  (declare (optimize (speed 3)(safety 1)))
  (if (and *encryption* *cipher*)
      (tlet* ((encrypted byte-array (encrypt-byte-array *cipher* (conc-arrs args)))
              (len fixnum (length encrypted))
              (arr-to-send byte-array (conc-arrs `(#(,len) ,encrypted))))
        ;;(print stream)
        (write-byte-array arr-to-send stream))
      (let ((succ?
              (every (lambda (i)                   
                       (not (null i)))
                     (mapcar (lambda (arg)
                               (declare (byte-array arg))
                               (if (open-stream-p stream)
                                   (write-byte-array arg stream)
                                   (error "stream shut~A~%" stream)))
                             args))))       
        succ?)))

(defmethod send (connection (packet data-packet))
  (with-accessors ((recipient recipient)
                   (data data)
                   (len d-len)
                   (header header)
                   (footer footer)
                   (sender sender)
                   (op op))
      (add-sender connection packet)
    (bt:with-lock-held ((stream-lock connection))
      (values (write-all-to-stream (c-stream connection)
                                   header op recipient sender len data footer)))
    packet))

(defmethod send (connection (packet kill-packet))
  (with-accessors ((recipient recipient)
                   (header header)
                   (footer footer)
                   (sender sender)
                   (op op))
      (add-sender connection packet)
    (bt:with-lock-held ((stream-lock connection))
      (values (write-all-to-stream  (c-stream connection)
                                    header op recipient sender footer)))
    packet))

(defmethod send (connection (packet identify-packet))
  (with-accessors ((recipient recipient)
                   (header header)
                   (id id)
                   (sender sender)
                   (footer footer)
                   (op op))
      (add-sender connection packet)
    (bt:with-lock-held ((stream-lock connection))
      (values (write-all-to-stream (c-stream connection)
                                   header op recipient sender id footer)))
    packet))

(defmethod send (connection (packet ack-packet))
  (with-accessors ((recipient recipient)
                   (header header)
                   (footer footer)
                   (sender sender)
                   (op op))
      (add-sender connection packet)
    (bt:with-lock-held ((stream-lock connection))
      (values (write-all-to-stream (c-stream connection)
                                   header op recipient sender footer)))
    packet))

(defmethod send (connection (packet clients-packet))
  (with-accessors ((recipient recipient)
                   (header header)
                   (footer footer)
                   (sender sender)
                   (op op)
                   (client-name client-name)
                   (connected? connected?))
      (add-sender connection packet)
    (bt:with-lock-held ((stream-lock connection))
      (values (write-all-to-stream (c-stream connection)
                                   header op recipient sender client-name connected? footer)))
    packet))

(defmethod send-data-packet ((obj client) recipient data)
  "Quick wrapper around send to send data packets, requires a recipient that the client is aware of
and the data that is to be sent"
  (let ((clients (available-clients obj)))
    (when (member recipient clients :test #'string=)
      (let ((packet (build-data-packet recipient data)))
        (values (send obj packet) clients))
      nil)))

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
             (maphash (lambda (key2 val2)
                        (declare (ignore val2))
                        (unless (string= (connection-name val)
                                         key2)
                          (sb-ext:atomic-incf (car *moved-packets*))
                          (send val (build-clients-packet key2 1))))
                      (current-connections obj)))
           (current-connections obj)))

;;map over the connections, then send all 

(defun update-all-clients-with-connected?-client (server connection connectedp)
  (maphash (lambda (key val)
             (declare (ignore key))
             (let ((current-con val))
               (unless (equalp (connection-name current-con) (connection-name connection))
                 ;; (ignore-errors
                 (sb-ext:atomic-incf (car *moved-packets*))
                 (send current-con
                       (build-clients-packet (connection-name connection)
                                             (if connectedp 1 0))))))
           ;;we do not want the function to fail to send to clients that are still connected, just
           ;;because one has disconnected in quick succession. 
           (current-connections server)))

(defmethod update-all-clients-with-connected-client ((obj server) connection)
  (update-all-clients-with-connected?-client obj connection t))
(defmethod update-all-clients-with-disconnected-client ((obj server) connection)
  (update-all-clients-with-connected?-client obj connection nil))

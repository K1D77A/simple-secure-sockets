(in-package :simple-secure-sockets)
(defun build-packet (recipient op)
  "takes in a recipient and creates an instance of packet"
  (let ((rec-vecced (vectorize-data recipient %connection-name-len))
        (op-vecced (vectorize-data op))
        (foot-vecced (vectorize-data %stop-footer))
        (head-vecced (vectorize-data %start-header)))
    (unless (n-or-lessp %connection-name-len rec-vecced)
      (error "recipient does not satisfy predicate n-or-lessp. Recipient: ~A~%Length: ~A" recipient (length recipient)))
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
  (let ((id-vecced (vectorize-data id %connection-name-len)))
    (unless (n-or-lessp %connection-name-len id)
      (error "id does not satisfy predicate n-or-lessp. ID: ~A~%Length: ~A" id (length id)))
    (let ((packet (build-packet %identify-recipient %op-identify)))
      (change-class packet 'identify-packet)
      (setf (id packet) id-vecced)
      packet)))

(defun build-data-packet (sender recipient data)
  (let* ((packet (build-packet recipient %op-data))
         (sender-vecced (vectorize-data sender %connection-name-len))
         (d-vecced (vectorize-data data))
         (len (length d-vecced)))
    (unless (<= len  %max-data-size)
      (error "Data is longer than ~A which is the current hard limit on data size. Length: ~A" %max-data-size len))
    (change-class packet 'data-packet)
    (setf (d-len packet) (make-array 1 :element-type '(unsigned-byte 8) :initial-element len))
    (setf (data packet) d-vecced)
    (setf (sender packet) sender-vecced)
    packet))
(defun force-write-sequence (seq stream)
  (write-sequence seq stream)
  (force-output stream))
;; (defmethod send :after (connection packet)
;;   (f-format t "~A sent type of packet ~A to ~A"
;;             (connection-name connection)
;;             (type-of packet)
;;             (recipient packet)))
(defmethod send (connection (packet data-packet))
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
                  header recipient op sender len data footer)
     (c-stream connection))))

(defmethod send (connection (packet kill-packet))
  (with-accessors ((recipient recipient)
                   (header header)
                   (footer footer)
                   (op op))
      packet
    (force-write-sequence 
     (concatenate '(vector (unsigned-byte 8))
                  header recipient op footer)
     (c-stream connection))))

(defmethod send (connection (packet identify-packet))
  (with-accessors ((recipient recipient)
                   (header header)
                   (id id)
                   (footer footer)
                   (op op))
      packet
    (force-write-sequence 
     (concatenate '(vector (unsigned-byte 8))
                  header recipient op id footer)
     (c-stream connection))))

(defmethod send (connection (packet ack-packet))
  (with-accessors ((recipient recipient)
                   (header header)
                   (footer footer)
                   (op op))
      packet
    (force-write-sequence 
     (concatenate '(vector (unsigned-byte 8))
                  header recipient op footer)
     (c-stream connection))))

;;;;this file contains everything required to implement encryption
(in-package :simple-secure-sockets)

(defun hash-password (password digest)
  (ironclad:byte-array-to-hex-string 
   (ironclad:digest-sequence 
    digest
    (ironclad:ascii-string-to-byte-array password))))

(defparameter *prng* (ironclad:make-prng :fortuna))
(ironclad:read-os-random-seed :random *prng*)

(defun rand-data (len)
  "Generates a random byte array after reseeding the prng"  
  (ironclad:random-data len *prng*))

(defparameter *iamasalt* "saltsaltsalttalt")
(defparameter *pass* (concatenate 'string "iamapassword" *iamasalt*))

;;if I have more time I should implement salsa20

;;;because I am taking some liberties and ignoring the asymmetric part of setting up encryption,
;;;im not going to regenerate the IV's. If this system was using asymmetric encryption to perform
;;;a handshake where you would once you have a secure channel, send the IV and the encrypt/decrypt key
;;;to the server, I would randomize the IV for each connection. But it is not.
(defparameter *blowfish-cipher*
  (ironclad:make-cipher :blowfish
                        :mode :cfb8
                        :initialization-vector (rand-data 8)
                        :key (ironclad:ascii-string-to-byte-array (hash-password *pass* :sha3/224))))

(defparameter *twofish-cipher*
  (ironclad:make-cipher :twofish
                        :mode :cfb8
                        :initialization-vector (rand-data 16)
                        :key (ironclad:ascii-string-to-byte-array (hash-password *pass* :ripemd-128))))

(defparameter *threefish256-cipher*
  (ironclad:make-cipher :threefish256
                        :mode :cfb8
                        :initialization-vector (rand-data 32)
                        :key (ironclad:ascii-string-to-byte-array (hash-password *pass* :ripemd-128))))

(defparameter *threefish512-cipher*
  (ironclad:make-cipher :threefish512
                        :mode :cfb8
                        :initialization-vector (rand-data 64)
                        :key (ironclad:ascii-string-to-byte-array (hash-password *pass* :sha256))))

(defparameter *threefish1024-cipher*
  (ironclad:make-cipher :threefish1024
                        :mode :cfb8
                        :initialization-vector (rand-data 128)
                        :key (ironclad:ascii-string-to-byte-array (hash-password *pass* :sha512))))

(defparameter *aes128-cipher*
  (ironclad:make-cipher :aes
                        :mode :cfb8
                        :initialization-vector (rand-data 16)
                        :key (ironclad:ascii-string-to-byte-array
                              (concatenate 'string
                                           (hash-password *pass* :adler32)
                                           (hash-password *pass* :adler32)))))
(defparameter *aes256-cipher*
  (ironclad:make-cipher :aes
                        :mode :cfb8
                        :initialization-vector (rand-data 16)
                        :key (ironclad:ascii-string-to-byte-array (hash-password *pass* :ripemd-128))))

(defparameter *3des-cipher*
  (ironclad:make-cipher :3des
                        :mode :cfb8
                        :initialization-vector (rand-data 8)
                        :key (ironclad:ascii-string-to-byte-array
                              (concatenate 'string
                                           (hash-password *pass* :adler32)
                                           (hash-password *pass* :adler32)))))

(defparameter *des-cipher*
  (ironclad:make-cipher :des
                        :mode :cfb8
                        :initialization-vector (rand-data 8)
                        :key (ironclad:ascii-string-to-byte-array
                              (hash-password *pass* :adler32))))

(defparameter *all-ciphers* (list *des-cipher* *3des-cipher* *aes256-cipher*
                                  *aes128-cipher* *threefish1024-cipher* *threefish256-cipher*
                                  *threefish256-cipher* *twofish-cipher* *blowfish-cipher*))

(defun encrypt-string (cipher string)
  (ironclad:encrypt-in-place cipher (ironclad:ascii-string-to-byte-array string)))

(defvar *ciphers-keysize* (make-hash-table :test #'equal))

(defun all-ciphers-and-key-sizes ()
  (mapcar (lambda (cipher)
            (setf (gethash cipher *ciphers-keysize*) (ironclad:key-lengths cipher)))
          (ironclad:list-all-ciphers)))

(defvar *digest-length* (make-hash-table :test #'equal))

(defun all-digest-lengths ()
  (mapcar (lambda (digest)
            (setf (gethash digest *digest-length*)  (ironclad:digest-length digest)))
          (ironclad:list-all-digests)))

(defun find-sized-digest-len (len)
  (let ((result))
    (maphash (lambda (key val)
               (when (= val len)
                 (push key result)))
             *digest-length*)
    result))

(defun find-cipher-digest (cipher)
  (let ((keylen (gethash cipher *ciphers-keysize*)))
    (find-sized-digest-len keylen)))

(defun encrypt-byte-array (cipher byte-array)
  ;; (reinitialize-instance cipher)
  (let ((byte (conc-arrs (list (rand-data (ironclad:block-length cipher))  byte-array))))
    (ironclad:encrypt-in-place cipher byte)
    byte))

(defun encrypt-string-to-byte-array (cipher string)
  ;; (reinitialize-instance cipher)
  (let ((str (ironclad:ascii-string-to-byte-array string)))
    (ironclad:encrypt-in-place cipher str)
    str))

(defun decrypt-byte-array-to-string (cipher byte-array)
  ;;  (reinitialize-instance cipher)
  (ironclad:decrypt-in-place cipher byte-array)
  (setf byte-array (coerce (map 'list #'code-char byte-array) 'string)))

(defun decrypt-byte-array (cipher byte-array)
  (declare (optimize (speed 3)(safety 1)))
  ;;  (reinitialize-instance cipher)
  (tlet ((arr byte-array byte-array))
    (ironclad:decrypt-in-place cipher arr)
    (subseq arr (ironclad:block-length cipher))))

(defun seq-total-len (seqs)
  "returns the total length of all the seqs together"
  (declare (optimize (speed 3)(safety 1)))
  (reduce #'+ (mapcar #'length seqs)))

(defun conc-arrs (arrs)
  (declare (optimize (speed 3)(safety 1)))
  (tlet ((array byte-array (make-array (seq-total-len arrs) :element-type 'u-byte))
         (pos integer 0))    
    (loop :for arr :in (the list arrs)
          :do (loop :for ele :across  arr
                    :do (setf (aref array pos) ele)
                        (incf pos)))
    array))

(defmethod encrypt-packet (connection cipher (packet data-packet))
  (with-accessors ((recipient recipient)
                   (data data)
                   (len d-len)
                   (header header)
                   (footer footer)
                   (sender sender)
                   (op op))
      (add-sender connection packet)
    (let ((b-arr (conc-arrs (list header op recipient sender len data footer))))
      (encrypt-byte-array cipher b-arr))))

(defmethod encrypt-packet (connection cipher (packet kill-packet))
  (with-accessors ((recipient recipient)
                   (header header)
                   (footer footer)
                   (sender sender)
                   (op op))
      (add-sender connection packet)    
    (let ((b-arr (conc-arrs (list header op recipient sender footer))))
      (encrypt-byte-array cipher b-arr))))

(defmethod encrypt-packet (connection cipher (packet identify-packet))
  (with-accessors ((recipient recipient)
                   (header header)
                   (id id)
                   (sender sender)
                   (footer footer)
                   (op op))
      (add-sender connection packet)
    (let ((b-arr (conc-arrs (list header op recipient sender id footer))))
      (encrypt-byte-array cipher b-arr))))

(defmethod encrypt-packet (connection cipher (packet ack-packet))
  (with-accessors ((recipient recipient)
                   (header header)
                   (footer footer)
                   (sender sender)
                   (op op))
      (add-sender connection packet)
    (let ((b-arr (conc-arrs (list header op recipient sender footer))))
      (encrypt-byte-array cipher b-arr))))

(defmethod encrypt-packet (connection cipher (packet clients-packet))
  (with-accessors ((recipient recipient)
                   (header header)
                   (footer footer)
                   (sender sender)
                   (op op)
                   (client-name client-name)
                   (connected? connected?))
      (add-sender connection packet)
    (let ((b-arr (conc-arrs (list header op recipient sender client-name connected? footer))))
      (encrypt-byte-array cipher b-arr))))

(defun send-packet (connection cipher packet)
  (tlet* ((encrypted byte-array (encrypt-packet connection cipher packet))
          (len fixnum (length encrypted))
          (arr-to-send byte-array (conc-arrs `(#(,len) ,encrypted))))
    (write-sequence arr-to-send (c-stream connection))))

(defun download-encrypted-packet (connection cipher)
  (declare (optimize (speed 3)(safety 1)))
  (handler-case
      (tlet* ((stream stream (c-stream connection))
              (len u-byte (timed-non-block-read-byte stream))
              (encrypted-packet byte-array (read-n-bytes len stream)))
        (flexi-streams:make-in-memory-input-stream (decrypt-byte-array cipher encrypted-packet)))
    (stream-error (c)
      (write-error c)
      :EOF)
    (SB-INT:SIMPLE-STREAM-ERROR ()
      :EOF)
    (broken-packet ()
      (sb-ext:atomic-incf (car oofs))
      :EOF)))

(defun test-decrypt-time (n cipher)
  (let* ((encrypted (encrypt-string-to-byte-array cipher (make-string n :initial-element #\a)))
         (decrypted (decrypt-byte-array cipher encrypted)))
    (flexi-streams:make-in-memory-input-stream decrypted)))

;;;;this file contains everything required to implement encryption
;;;;the current implementation simply encrypts a complete packet with a known cipher for both
;;;;client and server because there is no handshake to exchange the encryption key

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

;;;have a key here that is constant, with a salt, again because this is just testing the impact
;;;of encryption on parsing times
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
;;;obviously different hashing algorithms have different levels of security but they
;;;also produce different length outputs, so different hashes are used for different algorithms
;;;and in some cases the keys are hashed twice and the result appended together

(defparameter *all-ciphers* (list *des-cipher* *3des-cipher* *aes256-cipher*
                                  *aes128-cipher* *threefish1024-cipher* *threefish256-cipher*
                                  *threefish256-cipher* *twofish-cipher* *blowfish-cipher*))

(defun encrypt-byte-array (cipher byte-array)
  "Takes in a cipher and a byte array and returns a new byte array whose contents has 
been encrypted. The cipher should be in cfb8 mode as this will append a random byte-array
of length (block-length cipher) to the start of the byte-array."
  (let ((byte (conc-arrs (list (rand-data (ironclad:block-length cipher))  byte-array))))
    (ironclad:encrypt-in-place cipher byte)
    byte))

(defun decrypt-byte-array (cipher byte-array)
  "Takes in a cipher and a byte-array and decrypts the byte array. The returned array is not the 
complete decryption but only contains the data that the user wanted encrypted when using 
'encrypt-byte-array', after decryption the array subseq'd (subseq arr (block-length cipher))
to remove the IV block"
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
  "concatenate all the arrays within the list arrs and return 1 new array"
  (declare (optimize (speed 3)(safety 1)))
  (tlet ((array byte-array (make-array (seq-total-len arrs) :element-type 'u-byte))
         (pos integer 0))    
    (loop :for arr :in (the list arrs)
          :do (loop :for ele :across  arr
                    :do (setf (aref array pos) ele)
                        (incf pos)))
    array))
(defgeneric encrypt-packet (connection cipher packet)
  (:documentation "Takes in a connection, cipher and an instance of packet
 (or subclass) then returns the the packet as an encrypted byte-array that can be sent"))

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
  "Takes in a connection, cipher and packet, encrypts the packet using 'encrypt-packet'
and appends its length to the start of the encrypted packet before sending it down
 (c-stream connection)"
  (tlet* ((encrypted byte-array (encrypt-packet connection cipher packet))
          (len fixnum (length encrypted))
          (arr-to-send byte-array (conc-arrs `(#(,len) ,encrypted)))
          (stream stream (c-stream connection)))
    (write-sequence arr-to-send stream)
    (force-output stream)))

(defun download-encrypted-packet (connection cipher)
  "downloads an encrypted packet from the (c-stream connection), decrypts it and returns it as an
in-memory-input-stream for use with a parser. This means that encrypted packets can be downloaded
and dropped straight into a parser used for non encrypted parsers"
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
      :EOF)))

(defun test-decrypt-time (n cipher)
  (let* ((encrypted (encrypt-string-to-byte-array cipher (make-string n :initial-element #\a)))
         (decrypted (decrypt-byte-array cipher encrypted)))
    (flexi-streams:make-in-memory-input-stream decrypted)))

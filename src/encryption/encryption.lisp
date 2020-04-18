;;;;this file contains everything required to implement encryption
(in-package :simple-secure-sockets)


(defparameter *iamasalt* "saltsaltsalttalt")
(defparameter *pass* (concatenate 'string "iamapassword" *iamasalt*))


(defparameter *blowfish-cipher*
  (ironclad:make-cipher :blowfish
                        :mode :ecb
                        :key (ironclad:ascii-string-to-byte-array (hash-password *pass* :sha3/224))))

(defparameter *twofish-cipher*
  (ironclad:make-cipher :twofish
                        :mode :ecb
                        :key (ironclad:ascii-string-to-byte-array (hash-password *pass* :ripemd-128))))

(defparameter *threefish256-cipher*
  (ironclad:make-cipher :threefish256
                        :mode :ecb
                        :key (ironclad:ascii-string-to-byte-array (hash-password *pass* :ripemd-128))))

(defparameter *threefish512-cipher*
  (ironclad:make-cipher :threefish512
                        :mode :ecb
                        :key (ironclad:ascii-string-to-byte-array (hash-password *pass* :sha256))))

(defparameter *threefish1024-cipher*
  (ironclad:make-cipher :threefish1024
                        :mode :ecb
                        :key (ironclad:ascii-string-to-byte-array (hash-password *pass* :sha512))))

(defparameter *aes128-cipher*
  (ironclad:make-cipher :aes
                        :mode :ecb
                        :key (ironclad:ascii-string-to-byte-array
                              (concatenate 'string
                                           (hash-password *pass* :adler32)
                                           (hash-password *pass* :adler32)))))
(defparameter *aes256-cipher*
  (ironclad:make-cipher :aes
                        :mode :ecb
                        :key (ironclad:ascii-string-to-byte-array (hash-password *pass* :ripemd-128))))

(defparameter *des-cipher*
  (ironclad:make-cipher :3des
                        :mode :ecb
                        :key (ironclad:ascii-string-to-byte-array
                              (concatenate 'string
                                           (hash-password *pass* :adler32)
                                           (hash-password *pass* :adler32)))))



(defun make-cipher (key)
  (ironclad:make-cipher :blowfish
                        :mode :ecb
                        :key (ironclad:ascii-string-to-byte-array key)))

(defun encrypt-string (cipher string)
  (ironclad:encrypt-in-place cipher (ironclad:ascii-string-to-byte-array string)))

(defun hash-password (password digest)
  (ironclad:byte-array-to-hex-string 
   (ironclad:digest-sequence 
    digest
    (ironclad:ascii-string-to-byte-array password))))

(defun make-length-string (len))

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


(defun encrypt (cipher plaintext)
  (let ((msg (ironclad:ascii-string-to-byte-array plaintext)))
    (ironclad:encrypt-in-place cipher msg)
    (ironclad:octets-to-integer msg)))

(defun decrypt (cipher ciphertext-int)
  (let ((msg (ironclad:integer-to-octets ciphertext-int)))
    (ironclad:decrypt-in-place cipher msg)
    (coerce (mapcar #'code-char (coerce msg 'list)) 'string)))

(in-package :simple-secure-sockets)

#|
connected-client -> e*16 ; 16 characters and connected to the server
name -> e*16 ; 16 characters that are the client name
header -> #\s #\t #\a #\r #\t ;start
footer -> #\s #\t #\o #\p ;stop


data packet
header -> header
op -> #\d
recipient -> connected-client
sender -> connected-client
length -> e*0-255
data -> e*length
footer -> footer

kill-packet
header -> header
op -> #\k
recipient -> #\i #\w #\a #\n #\t #\t #\o #\d #\i #\e #\p #\l #\e #\a #\s #\e
sender -> client-name
footer -> footer

identify-packet
header -> header
op -> #\i
recipient -> #\l #\e #\t #\m #\e #\i #\d #\e #\n #\t #\i #\f #\y #\p #\l #\z 
connection-name -> e*16
footer -> footer

clients-packet
header -> header
op -> #\c
recipient -> #\h #\e #\r #\e #\a #\r #\e #\n #\e #\w #\c #\l #\i #\e #\n #\t
sender -> client-name
client -> client-name
connected -> 0 | 1
footer -> footer

ack-packet
header -> header
op -> #\a
recipient -> #\a #\c #\k #\n #\o #\w #\l #\e #\d #\g #\e #\m #\e #\p #\l #\z 
sender -> name
footer -> footer

|#


;;;as lisp forms
(defparameter *client* '(:e 16))
(defparameter *name* '(:e 16))
(defparameter *header* '(#\s #\t #\a #\r #\t))
(defparameter *footer* '(#\s #\t #\o #\p))
(defparameter *ops* '(#\a #\k #\d #\c #\i))

(defparameter *data-packet* `(:data
                              (:header ,*header*
                               :op (#\d)
                               :recipient ,*client*
                               :sender ,*client*
                               :extra (:len (:e 0 255)
                                       :data (:e :len))
                               :footer ,*footer*)))

(defparameter *kill-packet* `(:kill
                              (:header ,*header*
                               :op (#\k)
                               :recipient
                               (#\i #\w #\a #\n #\t #\t #\o #\d #\i #\e #\p #\l #\e #\a #\s #\e)
                               :sender ,*client*
                               :footer ,*footer*)))

(defparameter *identify-packet* `(:identify
                                  (:header ,*header*
                                   :op (#\i)
                                   :recipient
                                   (#\l #\e #\t #\m #\e #\i #\d #\e #\n #\t #\i #\f #\y #\p #\l #\z)
                                   :extra (:connection-name ,*client*)
                                   :sender ,*client*
                                   :footer ,*footer*)))

(defparameter *clients-packet* `(:clients
                                 (:header ,*header*
                                  :op (#\c)
                                  :recipient
                                  (#\h #\e #\r #\e #\a #\r #\e #\n #\e #\w #\c #\l #\i #\e #\n #\t)
                                  :extra (:client ,*client*
                                          :connected (:or 0 1))
                                  :footer ,*footer*)))

(defparameter *ack-packet* `(:ack
                             (:header ,*header*
                              :op (#\a)
                              :recipient
                              (#\a #\c #\k #\n #\o #\w #\l #\e #\d #\g #\e #\m #\e #\p #\l #\z)
                              :sender ,*client*
                              :footer ,*footer*)))
;;;have to step over the list and download each byte from the stream checking each char individually
;;;and throwing an error if its wrong, however the problem comes that with data-packet
;;;we have to download the length in order to then download enough bytes from the stream...

(defun map-plist (func plist)
  "maps a plist and calls a func that accepts two arguments. returns a list of
 (list key funcall-result)"
  (loop :with len := (length plist)
        :for x :from 0 :to (1- len) :by 2
        :for y :from 1 :to  len :by 2
        :for key := (nth x plist)
        :for val := (nth y plist)
        ;;  :do (print x)
        :collect (list key (funcall func key val))))

;;;easy macro here but cba
(defun download-and-validate-byte (stream expected)
  "Downloads 1 byte and checks that is is equal to expected, if it is it'll return the byte, if not
it'll throw error of time validation-failed-error"
  (tlet ((byte u-byte (timed-non-block-read-byte stream)));;thisn throws an error
    (if (equal byte (char-code expected))
        byte
        (signal-validation-failed-error "incorrect byte downloaded"
                                        (char-code expected)
                                        byte
                                        '(equal byte (char-code expected))))))

(defun download-and-validate-byte-against-potential (stream list-of-potential-vals)
  "downloads 1 byte and checks if it is equal to any of the numbers in list-of-potential-vals. if
the byte is then it'll return the byte, if not it will signal an error of type 
validation-failed-error"
  (tlet ((byte u-byte (timed-non-block-read-byte stream)));;thisn throws an error
    (if (some (lambda (potential)
                (= byte potential))
              list-of-potential-vals)
        byte
        (signal-validation-failed-error "incorrect byte downloaded"
                                        list-of-potential-vals
                                        byte
                                        '(some (lambda (potential)
                                                 (= byte potential))
                                          list-of-potential-vals)))))

(defun download-and-validate-byte-against-potential-chars (stream list-of-potential-vals)
  (tlet ((byte u-byte (timed-non-block-read-byte stream)));;thisn throws an error
    (if (some (lambda (potential)
                (equal (char-code potential) byte))
              list-of-potential-vals)
        byte
        (signal-validation-failed-error "incorrect byte downloaded"
                                        list-of-potential-vals
                                        byte
                                        '(some (lambda (potential)
                                                 (equal (char-code potential) byte))
                                          list-of-potential-vals)))))

(defun download-val (stream val)
  (tlet ((arr byte-array (make-array (length val) :element-type 'u-byte)))
    (map-into arr
              (lambda (char)
                (download-and-validate-byte stream char))
              val)
    arr))

(defun handle-e (stream sexp)
  (if (equal (first sexp) :e)
      (let ((len (length sexp)))
        (case len
          (2 (read-n-bytes (second sexp) stream))
          (3 (timed-non-block-read-byte stream))))
      (error "not an e sexp")))

(defun handle-or (stream sexp)
  (if (equal (first sexp) :or)
      (let ((or-list (rest sexp)))
        (download-and-validate-byte-against-potential stream or-list))
      (error "not an or sexp")))

(defun download-header (stream val)
  (download-val stream val))

(defun download-footer (stream val)
  (download-val stream val))

(defun download-op (stream val)
  (download-and-validate-byte stream (char-code val)))

(defun download-recipient (stream val)
  (if (equal (first val) :e)
      (handle-e stream val)
      (download-val stream val)))

(defun download-client (stream val)
  (handle-e stream val))

(defun download-sender (stream val)
  (handle-e stream val))

(defun download-clients-extra (stream val)
  (map-plist (lambda (key val)
               (case key
                 (:client (download-client stream val))
                 (:connected (handle-or stream val))))
             val))

(defun download-data-extra (stream val)
  (let ((len 0))
    (map-plist (lambda (key val)
                 (declare (ignore val))
                 (case key
                   (:len (setf len (timed-non-block-read-byte stream)))
                   (:data (handle-e stream `(:e ,len)))))
               val)))

(defun download-identify-extra (stream val)
  (map-plist (lambda (key val)
               (case key
                 (:connection-name (download-client stream val))))
             val))

(defun download-extra (type stream val)
  "downloads the extra data for the packets that have an extra keyword"
  (case type
    (:clients (download-clients-extra stream val))
    (:identify (download-identify-extra stream val))
    (:data (download-data-extra stream val))))


(defun download-from-packet-sexp (stream packet-sexp)
  "If the packet to be downloaded is known, this will parse that packet and return a list containing
all the byte arrays downloaded. If it fails it will return :EOF"
  (let ((type (first packet-sexp))
        (list (first (rest packet-sexp))))
    (map-plist (lambda (key val)
                 (handler-case 
                     (case key
                       (:header (download-header stream val))
                       (:op (download-op stream val))
                       (:recipient (download-recipient stream val))
                       (:extra (download-extra type stream val))
                       (:sender (download-sender stream val))
                       (:footer (download-footer stream val)))
                   (stream-error ()
                     :EOF)
                   (validation-failed-error ()
                     :EOF)))
               list)))
;;;well I just realized how dumb this is
;;;the grammars are useful for security reasons but its stupid and impractical. The server
;;;doesn't know what kind of packet is being sent in most circumstances so it has to read the
;;;header and then the op before it knows, at which point you would then start reading the packets
;;;post op...


(defun select-packet (op)
  (case op
    (#\a *ack-packet*)
    (#\k *kill-packet*)
    (#\d *data-packet*)
    (#\i *identify-packet*)
    (#\c *clients-packet*)))

(defun download-unknown-op (stream)
  "Downloads an op from stream validates it against *ops* and returns it, throws a 
validation-failed error if fails"
  (download-and-validate-byte-against-potential-chars stream *ops*))

(defmethod download-sequence-grammar ((obj connection))
  "downloads a packet from the connection and produces a plist with all the downloaded values
returns :EOF if it fails"
  (let ((stream (c-stream obj)))
    (handler-case
        (let* ((header (download-header stream *header*))
               (op (download-unknown-op stream))
               (packet (select-packet (code-char op)))
               (type (first packet))
               (list-remaining (nthcdr 4 (first (rest packet)))))
          (append  (list (list :header header)
                         (list :op (make-array 1 :element-type 'u-byte :initial-element op)))
                   (map-plist (lambda (key val)
                                (case key
                                  (:recipient (download-recipient stream val))
                                  (:extra (download-extra type stream val))
                                  (:sender (download-sender stream val))
                                  (:footer (download-footer stream val))))
                              list-remaining)))
      (stream-error ()
        :EOF)
      (validation-failed-error ()
        :EOF))))

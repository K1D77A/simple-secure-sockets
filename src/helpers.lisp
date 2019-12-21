(in-package :simple-secure-sockets)

(defvar %start-header "start")
(defvar %op-data "d")
(defvar %op-kill "k")
(defvar %stop-footer "stop")
(defparameter *op-keywords* (list :ALL :IDENTIFY :DATA :KILL))

(defparameter *DEBUG-OUTPUT* t)

(defun f-format (destination control-string &rest format-arguments)
  "just a normal format function that forces output"
  (when *DEBUG-OUTPUT*
    (format destination control-string format-arguments)
    (force-output destination)))

(defun list-to-string (lst)
  "converts a list to a string"
  (format nil "~s" lst))

(defun byte-vector-to-chars (byte-vector)
  "Takes in a byte vector of char codes and converts it to the original list. displays each byte on the leds by default"
  (let ((array (make-array (length byte-vector))))
    (map-into array (lambda (byte)                      
                      (code-char byte))
              byte-vector)
    array))
(defun byte-vector-to-string (byte-vector)
  (coerce (byte-vector-to-chars byte-vector) 'string))

(defun chars-sequence-to-list (chars-sequence)
  (read-from-string (coerce chars-sequence 'string)))


(defmethod vectorize-data ((data string))
  "takes in a string and converts it to an array of type '(unsigned-byte 8)"
  (let ((arr (make-array (length data) :element-type '(unsigned-byte 8))))
    (map-into arr #'char-code data)))

(defun string-to-keyword (string)
  (intern string :keyword))

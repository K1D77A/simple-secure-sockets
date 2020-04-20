(in-package :simple-secure-sockets)

(defparameter *var-hashtable* (make-hash-table))

(defmacro defvar-hash (symbol val)
  "Defines a variable with defvar named with symbol and given the value val. This is also put into the hashtable *var-hashtable* with the same symbol and val"
  `(progn (defvar ,symbol ,val)
          (unless (gethash ,symbol *var-hashtable*)
            (setf (gethash ,symbol *var-hashtable*) ,val))))


(defvar-hash %start-header "start")
(defvar-hash %op-data "d")
(defvar-hash %op-data-n 100)
(defvar-hash %op-kill "k")
(defvar-hash %op-kill-n 107)
(defvar-hash %op-identify "i")
(defvar-hash %op-identify-n 105)
(defvar-hash %op-ack "a")
(defvar-hash %op-ack-n 97)
(defvar-hash %stop-footer "stop")
(defvar-hash %op-clients "c")
(defvar-hash %op-clients-n 99)
;;this is the op that is sent from server, if a client connects
;;then the new client is sent to all connections, if a client disconnects then the disconnect is sent
;;to all the clients ie (("client1" . 0)) disconnect (("client2" . 1)) connected
;;with 500 clients to the server, I wonder if this is gonna cause like a dos effect xD
;;I'm not sure if this is the most efficient way to do this.
(defvar-hash %connection-name-len 16);bytes
(defvar-hash %kill-recipient "iwanttodieplease")
(defvar-hash %identify-recipient "letmeidentifyplz")
(defvar-hash %ack-recipient "acknowledgemeplz")
(defvar-hash %clients-recipient "herearenewclient")
(defvar-hash %max-data-size 255);1byte len

(deftype byte-array () '(simple-array (unsigned-byte 8) (*)))
(deftype single-byte () '(simple-array (unsigned-byte 8) (1)))
(deftype u-byte () '(integer 0 255))


(defparameter *start-header-mfsm* (make-micro-fsm-for-string %start-header))
(defparameter *stop-footer-mfsm* (make-micro-fsm-for-string %stop-footer))
(defparameter *valid-ops-form*
  (generate-character-check-form (mapcar (lambda (string)
                                           (aref string 0))
                                         (list %op-data
                                               %op-ack
                                               %op-kill
                                               %op-clients
                                               %op-identify))))

(defparameter *kill-recipient-mfsm* (make-micro-fsm-for-string %kill-recipient))
(defparameter *identify-recipient-mfsm*
  (make-micro-fsm-for-string %identify-recipient))
(defparameter *data-recipient-mfsm*
  (make-micro-fsm-to-read-n-chars %connection-name-len))
(defparameter *con-name-len-mfsm* *data-recipient-mfsm*)
(defparameter *ack-recipient-mfsm* (make-micro-fsm-for-string %ack-recipient))
(defparameter *clients-recipient-mfsm* (make-micro-fsm-for-string %clients-recipient))
(defparameter *valid-len-form* '((and (numberp :byte)
                                  (<= :byte 255)
                                  (<= 0 :byte))))
(defparameter *clients-valid-con-form* '((or (eq :byte 1)
                                          (zerop :byte))))


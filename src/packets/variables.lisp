(in-package :simple-secure-sockets)

(defparameter *var-hashtable* (make-hash-table))

(defmacro defvar-hash (symbol val)
  "Defines a variable with defvar named with symbol and given the value val. This is also put into the hashtable *var-hashtable* with the same symbol and val"
  `(progn (defvar ,symbol ,val)
          (unless (gethash ,symbol *var-hashtable*)
            (setf (gethash ,symbol *var-hashtable*) ,val))))


(defvar-hash %start-header "start")
(defvar-hash %op-data "d")
(defvar-hash %op-kill "k")
(defvar-hash %op-identify "i")
(defvar-hash %op-ack "a")
(defvar-hash %stop-footer "stop")
(defvar-hash %op-clients "c");;this is the op that is sent from server, if a client connects
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


(defparameter *start-header-mfsm* (make-micro-finite-state-machine-for-string %start-header))
(defparameter *data-header-mfsm* (make-micro-finite-state-machine-for-string %op-data))
(defparameter *kill-header-mfsm* (make-micro-finite-state-machine-for-string %op-kill))
(defparameter *identify-header-mfsm* (make-micro-finite-state-machine-for-string %op-identify))
(defparameter *ack-header-mfsm* (make-micro-finite-state-machine-for-string %op-ack))
(defparameter *stop-footer-mfsm* (make-micro-finite-state-machine-for-string %stop-footer))
(defparameter *clients-header-mfsm* (make-micro-finite-state-machine-for-string %op-clients))

(defparameter *kill-recipient-mfsm* (make-micro-finite-state-machine-for-string %kill-recipient))
(defparameter *identify-recipient-mfsm*
  (make-micro-finite-state-machine-for-string %identify-recipient))
(defparameter *ack-recipient-mfsm* (make-micro-finite-state-machine-for-string %ack-recipient))
(defparameter *clients-recipient-mfsm* (make-micro-finite-state-machine-for-string %clients-recipient))


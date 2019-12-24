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
(defvar-hash %stop-footer "stop")
(defvar-hash %connection-name-len 16);bytes
(defvar-hash %kill-recipient "iwanttodieplease")
(defvar-hash %identify-recipient "letmeidentifyplz")
(defvar-hash %max-data-size 255);1byte len



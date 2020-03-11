;;;;this file contains the implementations of finite state machines that will be used for parsing
(in-package :simple-secure-sockets)
;; (defvar-hash %start-header "start")
;; (defvar-hash %op-data "d")
;; (defvar-hash %op-kill "k")
;; (defvar-hash %op-identify "i")
;; (defvar-hash %op-ack "a")
;; (defvar-hash %stop-footer "stop")
;; (defvar-hash %op-clients "c");;this is the op that is sent from server, if a client connects
;; ;;then the new client is sent to all connections, if a client disconnects then the disconnect is sent
;; ;;to all the clients ie (("client1" . 0)) disconnect (("client2" . 1)) connected
;; ;;with 500 clients to the server, I wonder if this is gonna cause like a dos effect xD
;; ;;I'm not sure if this is the most efficient way to do this.
;; (defvar-hash %connection-name-len 16);bytes
;; (defvar-hash %kill-recipient "iwanttodieplease")
;; (defvar-hash %identify-recipient "letmeidentifyplz")
;; (defvar-hash %ack-recipient "acknowledgemeplz")
;; (defvar-hash %clients-recipient "herearenewclient")
;; (defvar-hash %max-data-size 255);1byte len
;;the above is just here for reference.
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
;; (defvar-hash %clients-recipient "hereditament")
;; (defvar-hash %max-data-size 255);1byte len
;;the above is just here for reference.
(declaim (optimize (speed 3) (safety 1)))






(define-condition validation-failed-error (error)
  ((message
    :initarg :message
    :accessor message
    :documentation "Message indicating what when wrong")
   (form-executed
    :initarg :form-executed
    :accessor form-executed
    :documentation "The form that was executed for comparison")
   (expected
    :initarg :expected
    :accessor expected
    :documentation "the char-code expected")
   (received
    :initarg :received
    :accessor received
    :documentation "The char-code received")))

(defmethod print-object ((object validation-failed-error) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~s~%(code-char Expected)= ~s~%(code-char Received)= ~s~%Form executed: ~s~%"
            (message object)
            (code-char (expected object))
            (code-char (received object))
            (form-executed object))))

(defun signal-validation-failed-error (message expected received form)
  (error 'validation-failed-error
         :message message
         :expected expected
         :received received
         :form-executed form))

(defclass micro-finite-state-machine ()
  ((list-of-states
    :accessor list-of-states
    :initarg :list-of-states)
   (current-state
    :accessor current-state
    :initarg :current-state)
   (len
    :initarg :len
    :accessor len)
   (result
    :accessor result
    :initarg :result)
   (final-condition
    :initform nil
    :accessor final-condition)))

(defmethod print-object ((object micro-finite-state-machine) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "Result: ~s~%Final condition: ~s~%"
            (result object)
            (final-condition object))))


(defun make-micro-finite-state-machine-for-string (string)
  (let ((states (pair-validators-with-states (generate-lambdas-based-on-string string)
                                             (generate-states-from-string string))))
    (make-instance 'micro-finite-state-machine
                   :list-of-states states :current-state "inactive"
                   :len (length states) :result (make-array (length states)
                                                            :initial-element 0
                                                            :element-type '(unsigned-byte 8)))))

(defun generate-lambdas-based-on-list (lst)
  "Takes in a list of char-codes and generates a list functions to validate that same list"
  (mapcar (lambda (char)
            (unless (integerp char)
              (error 'type-error "char is not an integer (char code)"
                     :expected-type 'integer))
            (lambda (stream)
              ;;its possible that we could add the function and stream here, so each fsm
              ;;would be made exactly for its job, ie
              ;;you would parse the func #'read-byte and the stream
              ;;then make a comparison based on those results
              (let ((byte (timed-non-block-read-byte 5 stream nil)))
                (if (= char byte)
                    byte
                    (signal-validation-failed-error "failed to validate" byte char
                                                    '(= char wanted))))))
          lst))
;;this above is a compromise
(defun generate-lambdas-based-on-string (string)
  (generate-lambdas-based-on-list (mapcar #'char-code (coerce string 'list))))

(defun validate-list-of-charcodes (list-of-chars list-of-validator-funcs)
  (every (lambda (char func)
           (funcall func char))
         list-of-chars
         list-of-validator-funcs))

(defun validate-string-with-validators (string validators)
  (validate-list-of-charcodes
   (mapcar #'char-code (coerce string 'list))
   validators))

(defun char-to-reading-read-keywords (char)
  (let ((reading (format nil "reading-~A" char))
        (read (format nil "read-~A"  char)))
    (list reading read)))

(defun generate-states-from-string (string)
  (map 'list (lambda (char)
               (list (string char) (char-to-reading-read-keywords char)))
       string))

(defun pair-validators-with-states (generated-validators generated-states)
  (mapcar (lambda (state validator)            
            (append state (list validator)))
          generated-states generated-validators))

(defun states (state-list)
  (second state-list))
(defun reading-state (state-list)
  (first (states state-list)))
(defun read-state (state-list)
  (second (states state-list)))
(defun func (state-list)
  (first (last state-list)))

(defun test-micro-fsm (fsm)
  (with-open-file (stream "packets/testfsm" :element-type '(unsigned-byte 8))    
    (read-from-stream fsm stream)))

     (defmethod change-state ((fsm micro-finite-state-machine) newstate)
       (setf (current-state fsm) newstate))

(defmethod change-state :before (fsm newstate))
                                        ;(format t "~&changing state ~s" newstate))
(defun reset-micro-finite-state-machine (fsm)
  (change-state fsm "inactive")
  (setf (final-condition fsm) nil)
  (setf (result fsm) (make-array (len fsm) :initial-element 0 :element-type '(unsigned-byte 8))))

(defun read-from-stream (micro-fsm stream)
  (declare (optimize (speed 3)(safety 0)))
  (with-accessors ((parser list-of-states)
                   (result result)
                   (len len)
                   (final-condition final-condition))
      micro-fsm
    (the byte-array result)
    (let ((iter (the fixnum 0)))
      (handler-case
          (progn 
            (mapcar (lambda (state)
                      (the list state)
                      (change-state micro-fsm (reading-state state))
                      (setf (aref result iter)
                            (the u-byte
                                 (funcall (the function (func state)) stream)))
                      (incf iter)
                      (change-state micro-fsm (read-state state)))
                    parser)
            (change-state micro-fsm "Done")
            (setf final-condition "Success"))
        (validation-failed-error (e)
          (change-state micro-fsm "Error")
          (setf final-condition e)))))
  micro-fsm)




#|
a state machine for reading the string "start" would be like so
state = idle reading-s reading-t reading-a reading-r reading-t done errored
condition = any error encountered this will have to be caught and put into the fsm
to transition would be
reading-s then check if it is equal to s, if so then change state to reading t, accumulate
and continue. can check if valid with lambdas ie
(lambda (s)
(equal s "s")) 
or #\s or char-code of #\s etc each can be generated automatically
|#

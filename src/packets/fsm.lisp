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
;;(declaim (optimize (speed 3) (safety 1)))






(define-condition validation-failed-error (error)
  ((message
    :initarg :message
    :accessor message
    :initform :not-set
    :documentation "Message indicating what when wrong")
   (form-executed
    :initarg :form-executed
    :accessor form-executed
    :initform :not-set
    :documentation "The form that was executed for comparison")
   (expected
    :initarg :expected
    :accessor expected
    :initform :not-set
    :documentation "the char-code expected")
   (received
    :initarg :received
    :accessor received
    :initform :not-set
    :documentation "The char-code received")))

(defmethod print-object ((object validation-failed-error) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~s~%(code-char Expected)= ~s~%(code-char Received)= ~s~%Form executed: ~s~%"
            (message object)
            (expected object)
            (received object)
            (form-executed object))))

(defun signal-validation-failed-error (message expected received form)
  (error 'validation-failed-error
         :message message
         :expected expected
         :received received
         :form-executed form))

(defclass micro-finite-state-machine ()
  ((states-and-lambdas
    :reader states-and-lambdas
    :type list
    :initarg :states-and-lambdas)
   (current-state
    :accessor current-state
    :type string
    :initarg :current-state)
   (len
    :initarg :len
    :type integer
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
  (let* ((form (string-to-microfsm-form string))
         (states-and-lambdas (generate-lambdas-and-states-from-forms form)))
    (make-instance 'micro-finite-state-machine
                   :states-and-lambdas states-and-lambdas
                   :current-state "inactive"
                   :len (length states-and-lambdas)
                   :result (make-array (length states-and-lambdas)
                                       :initial-element 0
                                       :element-type '(unsigned-byte 8)))))

(defparameter *data-fsm* '((if (numberp :byte)
                               (and (less-than-or-equal 255 :byte)
                                    (greaterthan-or-equal 0 :byte)))))

(defparameter *next-data-fsm* (make-list 100 :initial-element '(char= :byte)))
(defparameter *header-fsm* ;header is "start"
  '((eq :byte 115)
    (eq :byte 116)
    (eq :byte 97)
    (eq :byte 114)
    (eq :byte 116)))

(defun string-to-microfsm-form (string)
  "convenience function to quickly generate the micro-fsm based on a string, more complicated have 
to be done by hand"
  (map 'list (lambda (char)
               (list 'eq :byte (char-code char)))
       string))

(defun generate-state (form)
  "When given a form like '(eq :byte 115) returns
 (:try 'trying (eq :byte 115)' :tried 'tried (eq :byte 115)')"
  (list :try (format nil "trying ~S" form)
        :tried (format nil "tried ~S" form)))

(defun generate-states (forms)
  "When given a list of forms like '((eq :byte 115)(eq :byte 116)) returns a list of the same length
which would look like 
'((:try 'trying (eq :byte 115)' 
   :tried (eq :byte 115))
  (:try 'trying (eq :byte 116)' 
   :tried 'tried (eq :byte 116)'))"
  (mapcar #'generate-state forms))

(defun change-byte (form to)
  "Changes :byte to the value of to"
  (subst to :byte form))

(defun generate-lambda-based-on-form (form)
  `(lambda (stream)
     (tlet ((byte (or boolean u-byte) (timed-non-block-read-byte stream)))
       (if ,(change-byte form 'byte)
           byte
           (signal-validation-failed-error "failed to validate form" ',form byte
                                           ',form)))))

(defun generate-lambdas-and-states-from-forms (forms)
  "Takes in a list of forms like '((eq :byte 115)(eq :byte 116)) and generates a new plist like
'(:lambda <generate-lambda-based-on-form> "
  (mapcar (lambda (form)
            (append (generate-state form)
                    (list :lambda (compile nil (generate-lambda-based-on-form  form)))))
          forms))


;; "Takes in a form like '(eq :byte 116) and generates a lambda that takes 1 argument, an '(unsigned-byte 8) stream, this lambda will read a byte from that stream and check whether the byte read is , where the keyword :byte is substituted with the value read
;; from the stream, if it is true then the byte is returned, if not a condition of type 
;; 'validation-failed-error is signalled"


;;https://termbin.com/ijj6g


(defun generate-lambdas-based-on-forms (forms)
  "Takes in a list of char-codes and generates a list functions to validate that same list"
  (mapcar #'generate-lambda-based-on-form forms))

(defun get-try (form)
  (second form))
(defun get-tried (form)
  (fourth form))
(defun get-lambda (form)
  (sixth form))

(defun test-micro-fsm (fsm)
  (with-open-file (stream "packets/testfsm" :element-type '(unsigned-byte 8))    
    (read-from-stream fsm stream)))

(defmethod change-state ((fsm micro-finite-state-machine) newstate)
  (setf (current-state fsm) newstate))

(defmethod change-state :before (fsm newstate)
  (format t "~&changing state ~A" newstate))

(defun reset-mfsm (fsm)
  (change-state fsm "inactive")
  (setf (final-condition fsm) nil)
  (setf (result fsm) (make-array (len fsm) :initial-element 0 :element-type '(unsigned-byte 8))))

(defun read-from-stream (micro-fsm stream)
  ;; (declare (optimize (speed 3)(safety 1)))
  (with-accessors ((parser states-and-lambdas)
                   (result result)
                   (len len)
                   (final-condition final-condition))
      micro-fsm
    (the byte-array result)
    (let ((iter (the fixnum 0)))
      (handler-case
          (progn 
            (mapcar (lambda (state-n-lambda)
                      (the list state-n-lambda)
                      (change-state micro-fsm (get-try state-n-lambda))
                      (setf (aref result iter)
                            (the u-byte
                                 (funcall (the function (get-lambda state-n-lambda)) stream)))
                      (incf iter)
                      (change-state micro-fsm (get-tried state-n-lambda)))
                    parser)
            (change-state micro-fsm "done")
            (setf final-condition "success"))
        (validation-failed-error (e)
          (change-state micro-fsm "error")
          (setf final-condition e)
          micro-fsm))))
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

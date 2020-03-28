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



(defconstant +valid-char-form+ '(numberp :byte))

(defparameter *data-fsm* '((and (numberp :byte)
                            (<= :byte 255)
                            (<= 0 :byte))))

(defparameter *next-data-fsm* (make-list 100 :initial-element '(char= :byte)))
(defparameter *header-fsm* ;header is "start"
  '((eq :byte 115)
    (eq :byte 116)
    (eq :byte 97)
    (eq :byte 114)
    (eq :byte 116)))



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

(define-condition failed-to-parse-complete-fsm (error)
  ((message
    :initarg :message
    :accessor message
    :initform :not-set
    :documentation "Message indicating what when wrong")
   (len
    :initarg :len
    :accessor len
    :initform :len-not-set
    :documentation "The total number of read-bytes to be attempted")
   (failed-at
    :initarg :failed-at
    :accessor failed-at
    :initform :failed-at-not-set
    :documentation "How many read-bytes were attempted before failure")
   (received
    :initarg :received
    :accessor received
    :initform :not-set
    :documentation "what was successfully received")))

(defmethod print-object ((object failed-to-parse-complete-fsm) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S~%Total to be attempted: ~A~%Failed at n: ~A~%Total received: ~S~%"
            (message object)
            (len object)
            (failed-at object)
            (received object))))
            

(defun signal-failed-to-parse-complete-fsm (message len failed-at received)
  (error 'failed-to-parse-complete-fsm
         :message message
         :len len
         :received received 
         :failed-at failed-at))

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

(defun make-micro-fsm (states-and-lambdas)
  (make-instance 'micro-finite-state-machine
                 :states-and-lambdas states-and-lambdas
                 :current-state "inactive"
                 :len (length states-and-lambdas)
                 :result (make-array (length states-and-lambdas)
                                     :element-type '(unsigned-byte 8))))

(defun make-micro-fsm-for-string (string)
  (tlet* ((form list (string-to-microfsm-form string))
          (states-and-lambdas list (generate-lambdas-and-states-from-forms form)))
    (make-micro-fsm states-and-lambdas)))

(defun make-micro-fsm-from-form (form)
  (tlet ((states-and-lambdas list (generate-lambdas-and-states-from-forms form)))
    (make-micro-fsm states-and-lambdas)))

(defun make-micro-fsm-to-read-n-chars (n)
  (tlet ((states-and-lambdas list (list  (generate-n-lambda-and-state n +valid-char-form+))))
    (make-micro-fsm states-and-lambdas)))
(defun generate-character-check-form (list-of-chars)
  "takes in a list of characters and generates a form which makes sure that byte downloaded is
one of the chars in list-of-chars"
  (list (cons 'or (mapcar (lambda (char)
                            (list 'eq :byte (char-code char)))
                          list-of-chars))))



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

(defun generate-n-read-state (n form)
  (list :try (format nil "trying ~S ~D times" form n)
        :tried (format nil "tried ~S ~D times" form n)))

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
     (tlet ((byte (or boolean u-byte)
                  (handler-case (timed-non-block-read-byte stream)
                    (stream-error ()
                      nil))))
       (if ,(change-byte form 'byte)
           byte
           (signal-validation-failed-error "failed to validate form" ',form byte
                                           ',form)))))

(defun generate-lambda-do-form-n-times (n form)
  `(lambda (stream)
     (tlet ((results byte-array (make-array ,n :element-type '(unsigned-byte 8))))
       (dotimes (x ,n results)
         (tlet ((byte (or boolean u-byte)
                      (handler-case (timed-non-block-read-byte stream)
                        (stream-error ()
                          nil))))
           ;;this will return nil if it catches the error thrown by
           ;;timed non-block-read-byte and then the if will throw a new error for the fsm to catch
           (if ,(change-byte form 'byte)
               (setf (aref results x) byte)
               (signal-failed-to-parse-complete-fsm "failed to read all bytes" ,n x results)))))))
                                               
                      

(defun generate-lambdas-and-states-from-forms (forms)
  "Takes in a list of forms like '((eq :byte 115)(eq :byte 116)) and generates a new plist like
'(:lambda <generate-lambda-based-on-form> "
  (mapcar (lambda (form)
            (append (generate-state form)
                    (list :lambda (compile nil (generate-lambda-based-on-form  form)))))
          forms))

(defun generate-n-lambda-and-state (n form)
  (append (generate-n-read-state n form)
          (list :lambda (compile nil (generate-lambda-do-form-n-times n form)))))


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
  (with-open-file (stream "testfsm" :element-type '(unsigned-byte 8))    
    (read-from-stream fsm stream)))

(defmethod change-state ((fsm micro-finite-state-machine) newstate)
  (setf (current-state fsm) newstate))

(defmethod change-state :before (fsm newstate))
;;  (f-format t "~&changing state ~A" newstate))

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
                      (tlet* ((func function (get-lambda state-n-lambda))
                              (byte (or boolean u-byte byte-array) (funcall func stream)))
                        (typecase byte
                          (integer (setf (aref result iter) byte))
                          (simple-array (setf result byte))))
                      (incf iter)
                      (change-state micro-fsm (get-tried state-n-lambda)))
                    parser)
            (change-state micro-fsm "done")
            (setf final-condition "success"))
        (validation-failed-error (e)
          (change-state micro-fsm "error")
          (setf final-condition e)
          micro-fsm)
        (failed-to-parse-complete-fsm (e)
          (change-state micro-fsm "error")
          (setf final-condition e)
          micro-fsm))))
  micro-fsm)

(defun successful-execution-p (micro-fsm)
  (if (equal (final-condition micro-fsm) "success")
      t
      nil))
      
;; (defclass macro-finite-state-machine ()
;;   ((list-of-micro-fsm
;;     :accessor list-of-macro-fsm
;;     :initarg :list-of-macro-fsm
;;     :type list)
;;    (current-state
;;     :accessor current-state
;;     :initform "inactive")
;;    (current-micro-fsm
;;     :accessor current-micro-fsm
;;     :type micro-finite-state-machine)
;;    (final-condition
;;     :accessor final-condition
;;     :type condition)
;;    (fsm-results
;;     :accessor fsm-results
;;     :type list))
;;   (:documentation "This class is used to execute many micro-fsm's at once to parse a complete
;; stream"))

;; (defun generate-meta-state (micro-fsm)
;;   (list :macro-try (format nil "trying micro-fsm: ~S" micro-fsm)
;;         :macro-tried (format nil "tried micro-fsm: ~S" micro-fsm)))

;; (defun generate-meta-states (list-of-micro-fsm)
;;   (mapcar (lambda (mi-fsm)
;;             (append (generate-meta-state list-of-micro-fsm)
;;                     (list :micro-fsm mi-fsm)))
;;           list-of-micro-fsm))








;; #|
;; a state machine for reading the string "start" would be like so
;; state = idle reading-s reading-t reading-a reading-r reading-t done errored
;; condition = any error encountered this will have to be caught and put into the fsm
;; to transition would be
;; reading-s then check if it is equal to s, if so then change state to reading t, accumulate
;; and continue. can check if valid with lambdas ie
;; (lambda (s)
;; (equal s "s")) 
;; or #\s or char-code of #\s etc each can be generated automatically
;; |#


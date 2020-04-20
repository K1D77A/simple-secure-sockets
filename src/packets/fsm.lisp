;;;;this file contains the implementations of finite state machines that will be used for parsing
(in-package :simple-secure-sockets)

(defparameter *valid-char-form* '(numberp :byte))

(defvar *lambda-table* (make-hash-table :test #'equal))
(defvar *state-table* (make-hash-table :test #'equal))




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
  (tlet ((states-and-lambdas list (list (generate-n-lambda-and-state n *valid-char-form*))))
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


;;;memoization functions

(defun check-in-table (var table)
  ;;just to make things more obvious
  "checks if var is in table."
  (declare (inline check-in-table))
  (gethash var table))

(defun check-if-forms-states-exists (form)
  (declare (inline check-if-forms-states-exists))
  (check-in-table form *state-table*))

(defun check-if-forms-lambda-exists (form)
  (declare (inline check-if-forms-lambda-exists))
  (check-in-table form *lambda-table*))

(defun generate-states-and-store (form)
  (declare (inline generate-states-and-store))
  (setf (gethash form *state-table*)
        (generate-state form)))

(defun generate-n-states-and-store (n form)
  (declare (inline generate-n-states-and-store))
  (setf (gethash (list n form) *state-table*)
        (generate-n-read-state n form)))

(defun compile-and-store (form)
  (declare (inline compile-and-store))
  (setf (gethash form *lambda-table*)
        (compile nil (generate-lambda-based-on-form form))))

(defun compile-and-store-n-forms (n form)
  (declare (inline compile-and-store))
  (setf (gethash (list n form) *lambda-table*)
        (compile nil (generate-lambda-do-form-n-times n form))))



(defun generate-lambda-based-on-form (form)
  "generates a lambda that when compiled can be used to read from a byte stream. form is a list like
'((eq :byte 111)) 111 is an ascii character code and :byte is replaced with the downloaded byte. 
The lambda will call the form with :byte replaced with the byte and if true, return the byte,
if not true it will signal a validation-failed-error"
  `(lambda (stream)
     (declare (optimize (speed 3)(safety 1)))
     (tlet ((byte (or boolean u-byte)
                  (handler-case (timed-non-block-read-byte stream)
                    (stream-error ()
                      nil))))
       (if ,(change-byte form 'byte)
           byte
           (signal-validation-failed-error "failed to validate form" ',form byte
                                           ',form)))))

(defun generate-lambda-do-form-n-times (n form)
  "The same as 'generate-lambda-based-on-form' except will loop n times reading a new byte each time
and will check each new byte with form the same as in 'generate-lambda-based-on-form"
  `(lambda (stream)
     (declare (optimize (speed 3)(safety 1)))
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
  "Takes in a list of forms like '((eq :byte 115)) and generates a new plist like
 ((:try 'trying (eq :byte 111)' :tried 'tried (eq :byte 111)' :lambda <lambda func>))"
  (declare (optimize (speed 3)(safety 1)))
  (mapcar (lambda (form)
            (tlet* ((state? (or list boolean)
                            (check-if-forms-states-exists form))
                    (lambda? (or function boolean)
                             (check-if-forms-lambda-exists form)))
              (append (if state?
                          state?
                          (generate-states-and-store form))
                      (list :lambda
                            (if lambda?
                                lambda?
                                (compile-and-store form))))))
          forms))

(defun generate-n-lambda-and-state (n form)
  "The same as 'generate-lambdas-and-states-from-forms' but is used for generating lambdas that
loop over a form"
  (declare (optimize (speed 3)(safety 1)))
  (tlet* ((state? (or list boolean)
                  (check-if-forms-states-exists (list n form)))
          (lambda? (or function boolean)
                   (check-if-forms-lambda-exists (list n form))))
    (append (if state?
                state?
                (generate-n-states-and-store n form))
            (list :lambda
                  (if lambda?
                      lambda?
                      (compile-and-store-n-forms n form))))))

(defun generate-lambdas-based-on-forms (forms)
  "Takes in a form like '((eq :byte 116)) and generates a lambda that takes 1 argument, an '(unsigned-byte 8) stream, this lambda will read a byte from that stream and check whether the byte read is valid by doing (if form byte <error>) where the keyword :byte is substituted with the
 byte read from the stream. if it is true then the byte is returned, if not a condition of type 
   'validation-failed-error is signalled"
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
  "Uses a micro-fsm to read from a stream"
  (declare (optimize (speed 3)(safety 1)))
  (with-accessors ((parser states-and-lambdas)
                   (result result)
                   (len len)
                   (final-condition final-condition))
      micro-fsm
    ;;    (tlet ((iter fixnum 0))
    (handler-case
        (progn
          (loop :for s-n-l list :in parser
                :for x fixnum := 0 :then (1+ x)
                :do (change-state micro-fsm (get-try s-n-l))
                    (tlet* ((func function (get-lambda s-n-l))
                            (byte (or boolean u-byte byte-array) (funcall func stream)))
                      (typecase byte
                        (integer (setf (aref result x) byte))
                        (simple-array (setf result byte))))
                    (change-state micro-fsm (get-tried s-n-l)))
          ;; (mapcar (lambda (state-n-lambda)
          ;;           (the list state-n-lambda)
          ;;           (change-state micro-fsm (get-try state-n-lambda))
          ;;           (tlet* ((func function (get-lambda state-n-lambda))
          ;;                   (byte (or boolean u-byte byte-array) (funcall func stream)))
          ;;             (typecase byte
          ;;               (integer (setf (aref result iter) byte))
          ;;               (simple-array (setf result byte))))
          ;;           (incf iter)
          ;;           (change-state micro-fsm (get-tried state-n-lambda)))
          ;;         parser)
          (change-state micro-fsm "done")
          (setf final-condition "success"))
      (validation-failed-error (e)
        (change-state micro-fsm "error")
        (setf final-condition e)
        micro-fsm)
      (failed-to-parse-complete-fsm (e)
        (change-state micro-fsm "error")
        (setf final-condition e)
        micro-fsm)))
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


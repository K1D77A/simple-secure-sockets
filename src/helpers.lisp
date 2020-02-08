(in-package :simple-secure-sockets)
;;;;this file contains functions which are simply simply helpers for the rest of the program
(defparameter *op-keywords* (list :ALL :IDENTIFY :DATA :KILL :ACK))

(defparameter *debug-level* :all)
(defparameter *debug-levels* '(:none :all :error :warn :info :debug))
 ;; '(:debug :info :warn :error :all :none))
(defparameter *valid-format-prefixes*
  (list :testing
        :packet-read :packet-write
        :packet-forward
        :server-start :server-stop
        :client-start :client-stop
        :client-receive
        :client-connect :client-disconnect
        :server-receive :server-disconnect
        :packet-process))
;;use :all to always print regardless of level
(defun valid-prefix-p (prefix)
  (member prefix *valid-format-prefixes* :test #'equal))
(defun set-log-level (level)
  (if (member level *debug-levels*)
      (setf *debug-level* level)
      (error 'type-error :datum level :expected-type *debug-levels*)))
(defun conc-level-prefix-and-control-string (level prefix control-string)
  (concatenate 'string (format nil "~&[~s][~s]" level prefix) control-string "~%"))
(defun f-format (level prefix  control-string &rest format-arguments)
  (unless (valid-prefix-p prefix)
    (error "Invalid prefix: ~S" prefix))
  (if (not (equal *debug-level* :none))
      (when (or (equal level :all)
                (<= (position level *debug-levels*)
                    (position *debug-level* *debug-levels*)))
        (apply (function forced-format) *trace-output*
               (conc-level-prefix-and-control-string level prefix control-string)
               format-arguments))))

(defun forced-format (destination control-string &rest format-arguments)
  "just a normal format function that forces output"
  (format destination control-string format-arguments)
  (force-output destination))



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

(defun n-or-lessp (n seq)
  "check if seq is greater than 0, n or less"
  (if (<= 1 (length seq) n)
      t
      nil))
(defmacro with-unsigned-byte-8-array (n &body body)
  "Creates an unsigned-byte-array of size n and is accessible through the variable name 'arr'"
  `(let ((arr (make-array ,n :element-type '(unsigned-byte 8))))
     ,@body))

(defgeneric convert-to-string (data)
  (:documentation "converts a variety of data types into a string")
  (:method ((data string))
    (format nil "~A" data))
  (:method ((data list))
    (format nil "~A" data))
  (:method ((data integer))
    (format nil "~d" data))
  (:method ((data float))
    (format nil "~d" data))
  (:method ((data simple-array))
    (byte-vector-to-string data))
  (:method ((data character))
    (string data))
  (:method (data)
    (format nil "~A" data)))


(defun vectorize-data (data &optional (set-length nil))
  "takes in a string and converts it to an array of type '(unsigned-byte 8)"
  (declare (optimize (speed 3)(safety 1)))
  (let* ((as-string (the simple-string (convert-to-string data)))
         (arr (make-array (or set-length (length as-string))
                          :element-type '(unsigned-byte 8))))
    (declare (string as-string))
    (the byte-array (map-into arr #'char-code as-string))))

(defun string-to-keyword (string)
  (intern string :keyword))

(defun set-threads-to-std-out ()
  (setf bt:*default-special-bindings*;;this sets the var of standard out for the threads
        (acons '*standard-output* *standard-output*
               bt:*default-special-bindings*)))
(defun unique-key-p (hash-table key)
  "checks if key is a unique entry in hashtable"
  (if (equal (type-of hash-table) 'hash-table)
      (not (gethash key hash-table))
      (error "hash-table is not of type hash-table. ~A" (type-of hash-table))))
(defun find-and-kill-thread (name)
  "finds and kills the thread 'name'"
  (let ((threads (bt:all-threads)))
    (mapcar (lambda (thread)
              (when (equal (bt:thread-name thread)
                           name)
                (bt:destroy-thread thread)))
            threads)))
(defun make-thread (func &key name)
  (set-threads-to-std-out)
  (bt:make-thread func :name name))
(defun stop-thread (thread)
  (if (bt:thread-alive-p thread)
      (bt:destroy-thread thread)
      t))

(defun remove-trailing-x (sequence x)
  "Removes trailing x from sequence"
  (let ((first-x (position-if-not (lambda (item)
                                    (equal item x))
                                  sequence
                                  :from-end t)))
    (subseq sequence 0 (1+ first-x))))

(defun remove-trailing-spaces (sequence)
  (remove-trailing-x sequence #\Space))
(defun remove-trailing-nulls (sequence)
  (remove-trailing-x sequence #\Nul))
(defmacro eval-body-n-times-by-interval-until-t (number-of-intervals interval-time &body body)
  "Evals body n times by an interval of seconds until either body evaluates to non nil, in which case the value is returned. If n is reached with no evaluation to non nil then nil is returned, indicating failure"
  (let ((val (gensym)))
    `(loop :for x :from 1 :upto ,number-of-intervals    
           :do (let ((,val ,@body))
                 (if ,val
                     (return ,val)
                     (sleep ,interval-time))))))
(defmacro if-timed (number-of-intervals interval-time test then &optional else)
  "this is a normal if, however it will check if test is true for the count of number-of-intervals with a spacing of intervals-time"
  `(if (eval-body-n-times-by-interval-until-t ,number-of-intervals ,interval-time
         (if ,test
             ,then
             nil))
       ,then
       ,else))
(defmacro when-timed (number-of-intervals interval-time test &body body)
  "Executed the body after checking if test is true number-of-intervals times with an interval of interval-time. body is an implicit progn like in a normal when form"
  `(if-timed ,number-of-intervals ,interval-time ,test (progn ,@body) nil))

(defmethod safe-socket-close ((socket usocket:usocket))
  (usocket:socket-close socket)
  t)
(defmethod safe-socket-close (socket)
  :SOCKET-WAS-NOT-A-SOCKET)
(defun class-direct-reader-slots (class)
  (mapcar #'closer-mop:slot-definition-readers
          (closer-mop:class-direct-slots class)))
(defun packet-class-readers (packet-class)
  (if (equal packet-class 'packet)
      (class-direct-reader-slots packet-class)
      (if (closer-mop:subclassp (find-class packet-class) (find-class 'packet))
          (append
           (class-direct-reader-slots (find-class packet-class))
           (class-direct-reader-slots (find-class 'packet))))))
(defun convert-to-string-and-clean (seq &optional (clean-func #'remove-trailing-nulls))
  "converts the seq to a string and then funcalls 'clean-func' on the returned string"
  (funcall clean-func (convert-to-string seq)))
(defun c2s-c (seq &optional (clean-func #'remove-trailing-nulls))
  (convert-to-string-and-clean seq clean-func))

(defun write-to-file (file string)
  (with-open-file (stream file :direction :output :if-exists :append :if-does-not-exist :create)
    (format stream "~s~%" string)
    (force-output stream)))
(defun write-error (error)
  (write-to-file "errors" error))

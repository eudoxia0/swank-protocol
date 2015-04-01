(in-package :cl-user)
(defpackage swank-protocol
  (:use :cl)
  (:export :make-connection
           :connection-hostname
           :connection-port
           :connect
           :read-message-string
           :send-message-string
           :emacs-rex
           :request-connection-info
           :read-event
           :read-response)
  (:documentation "Low-level implementation of a client for the Swank protocol."))
(in-package :swank-protocol)

;;; Encoding and decoding messages

(defun encode-integer (integer)
  "Encode an integer to a 0-padded 16-bit hexadecimal string."
  (format nil "~6,'0,X" integer))

(defun decode-integer (string)
  "Decode a string representing a 0-padded 16-bit hex string to an integer."
  (parse-integer string :radix 16))

;; Writing and reading messages to/from streams

(defun write-message-to-stream (stream message)
  "Write a string to a stream, prefixing it with length information for Swank."
  (let* ((length-string (encode-integer (1+ (length message))))
         (msg (concatenate 'string
                           length-string
                           message
                           (string #\Newline))))
    (write-sequence msg stream)))

(defun read-message-from-stream (stream)
  "Read a string from a string.

Parses length information to determine how many characters to read."
  (let ((length-string (make-array 6 :element-type 'character)))
    (read-sequence length-string stream)
    (let* ((length (decode-integer length-string))
             (buffer (make-array length :element-type 'character)))
      (read-sequence buffer stream)
      buffer)))

;;; Data

(defclass connection ()
  ((hostname :reader connection-hostname
             :initarg :hostname
             :type string
             :documentation "The host to connect to.")
   (port :reader connection-port
         :initarg :port
         :type integer
         :documentation "The port to connect to.")
   ;; Internal
   (socket :accessor connection-socket
           :documentation "The usocket socket.")
   (request-count :accessor connection-request-count
                  :initform 0
                  :type integer
                  :documentation "A number that is increased and sent along with every request.")
   (package :accessor connection-package
            :initform "COMMON-LISP-USER"
            :type string
            :documentation "The name of the connection's package.")
   (thread :accessor connection-thread
           :initform t
           :documentation "The current thread.")
   ;; Logging
   (logp :accessor connection-log-p
         :initarg :logp
         :initform nil
         :type boolean
         :documentation "Whether or not to log connection requests.")
   (logging-stream :accessor connection-logging-stream
                   :initarg :logging-stream
                   :initform *error-output*
                   :type stream
                   :documentation "The stream to log to."))
  (:documentation "A connection to a remote Lisp."))

(defun make-connection (hostname port &key (logp nil))
  "Create a connection to a remote Swank server."
  (make-instance 'connection
                 :hostname hostname
                 :port port
                 :logp logp))

(defun connect (connection)
  "Connect to the remote server. Returns t."
  (with-slots (hostname port) connection
    (let ((socket (usocket:socket-connect hostname
                                          port
                                          :element-type 'character)))
      (setf (connection-socket connection) socket)))
  t)

(defun log-message (connection format-string &rest arguments)
  "Log a message."
  (when (connection-log-p connection)
    (apply #'format (cons (connection-logging-stream connection)
                          (cons format-string
                                arguments)))))

(defun read-message-string (connection)
  "Read a message string from a Swank connection."
  (with-slots (socket) connection
    (let ((stream (usocket:socket-stream socket)))
      (when (usocket:wait-for-input socket :timeout 5)
        (let ((msg (read-message-from-stream stream)))
          (log-message connection "~%Read: ~A~%" msg)
          msg)))))

(defun send-message-string (connection message)
  "Send a message string to a Swank connection."
  (with-slots (socket) connection
    (let ((stream (usocket:socket-stream socket)))
      (write-message-to-stream stream message)
      (force-output stream)
      (log-message connection "~%Sent: ~A~%" message)
      message)))

;;; Functions

(defun emacs-rex (connection form-string)
  (with-slots (package thread) connection
    (send-message-string
     connection
     (concatenate 'string
                  "(:emacs-rex "
                  form-string
                  " "
                  (prin1-to-string package)
                  " "
                  (prin1-to-string thread)
                  " "
                  (write-to-string (incf (connection-request-count connection)))
                  ")"))))

(defun request-connection-info (connection)
  (emacs-rex connection "(swank:connection-info)"))

(defun read-event (response-string)
  (with-standard-io-syntax
    (read-from-string response-string)))

(defun read-response (response-string)
  (with-standard-io-syntax
    (let ((response (read-from-string response-string)))
      (list :status (first (second response))
            :value (second (second response))
            :request-id (first (last response))))))

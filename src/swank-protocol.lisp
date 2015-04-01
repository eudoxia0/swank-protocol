(in-package :cl-user)
(defpackage swank-protocol
  (:use :cl)
  (:export :make-connection
           :connection-hostname
           :connection-port
           :connect
           :read-message
           :send-message)
  (:documentation "Low-level implementation of a client for the Swank protocol."))
(in-package :swank-protocol)

;;; Encoding and decoding messages

(defun encode-integer (integer)
  (format nil "~6,'0,X" integer))

(defun decode-integer (string)
  (parse-integer string :radix 16))

;; Writing and reading messages to/from streams

(defun write-message-to-stream (stream message)
  (let* ((length-string (encode-integer (1+ (length message))))
         (msg (concatenate 'string
                           length-string
                           message
                           (string #\Newline))))
    (format t "Sending: ~A~%" msg)
    (write-sequence msg stream)))

(defun read-message-from-stream (stream)
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
         :documentation "The port to connect to."))
  (:documentation "A connection to a remote Lisp."))

(defun make-connection (hostname port)
  (make-instance 'connection
                 :hostname hostname
                 :port port))

(defun connect (connection)
  (declare (ignore connection))
  ;; Do nothing, since we create connections on each write/read :/
  t)

(defun read-message (connection)
  (with-slots (hostname port) connection
    (usocket:with-client-socket (socket stream hostname port)
      (when (usocket:wait-for-input socket :timeout 5)
        (let ((msg (read-message-from-stream stream)))
          msg)))))

(defun send-message (connection message)
  (with-slots (hostname port) connection
    (usocket:with-client-socket (socket stream hostname port)
      (write-message-to-stream stream message)
      (force-output stream)
      message)))

;;; Functions

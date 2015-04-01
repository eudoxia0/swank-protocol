(in-package :cl-user)
(defpackage swank-protocol-test
  (:use :cl :fiveam))
(in-package :swank-protocol-test)

;;; Utilities

(defparameter *start-port* 40000)

(defun start-swank (hostname port)
  (setf swank:*configure-emacs-indentation* nil
        swank::*enable-event-history* nil
        swank:*log-events* t)
  (let ((swank::*loopback-interface* hostname))
    (swank:create-server :port port :dont-close t)))

(defmacro with-swank-server ((hostname port) &body body)
  `(progn
     (finishes
      (format t "~%Starting server on port ~A...~%" ,port)
      (start-swank ,hostname ,port))
     (sleep 3)
     ,@body
     (finishes
      (format t "~%Stopping server...")
      (swank:stop-server ,port))))

(defmacro with-connection ((conn &key logp) &body body)
  (alexandria:with-gensyms (hostname port)
    `(let ((,hostname (uiop:hostname))
           (,port (incf *start-port*)))
       (with-swank-server (,hostname ,port)
         (let ((,conn (swank-protocol:make-connection ,hostname ,port
                                                      :logp ,logp)))
           (is-true
            (swank-protocol:connect ,conn))
           ,@body)))))

(defmacro with-response ((conn name request) &body body)
  (alexandria:with-gensyms (resp-str)
    `(progn
       (is
        (stringp ,request))
       (let* ((,resp-str (swank-protocol:read-message-string ,conn))
              (,name (swank-protocol:read-response ,resp-str)))
         ,@body))))

;;; Tests

(def-suite tests
  :description "swank-protocol tests.")
(in-suite tests)

(test encoding/decoding
  (is
    (equal (swank-protocol::encode-integer 1)
           "000001"))
  (is
    (equal (swank-protocol::decode-integer "000001")
           1))
  (loop for num in (list 1 10 100 1000 2000 1000000) do
    (is
     (equal (swank-protocol::decode-integer (swank-protocol::encode-integer num))
            num))))

(test (connect :depends-on encoding/decoding)
  (with-connection (conn)
    ;; Do nothing
    t))

(test (basic-requests :depends-on connect)
  (with-connection (conn :logp t)
    (is-true
     (stringp
      (swank-protocol:send-message-string
       conn
       "(:emacs-rex (swank:connection-info) \"COMMON-LISP-USER\" t 1)")))
    (let ((resp (swank-protocol:read-message-string conn)))
      (is
       (stringp resp)))))

(test (connection-info :depends-on basic-requests)
  (with-connection (conn)
    (with-response (conn resp (swank-protocol:request-connection-info conn))
      (is
       (equal (getf resp :request-id) 1))
      (is
       (equal (getf resp :status) :ok))
      (is
       (equal (getf (getf resp :value) :style)
              :spawn)))))

(test (repl :depends-on basic-requests)
  (with-connection (conn)
    ;; Create REPL
    ;; Evaluate
    ;; Read presentation messages
    t))

(run! 'tests)

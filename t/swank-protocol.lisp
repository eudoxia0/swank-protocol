(in-package :cl-user)
(defpackage swank-protocol-test
  (:use :cl :fiveam))
(in-package :swank-protocol-test)

;;; Utilities

(defmacro with-inferior-lisp ((port) &body body)
  (let ((process (gensym)))
    `(let* ((startup
              (list "(ql:quickload :swank)"
                    "(setf swank:*configure-emacs-indentation* nil)"
                    "(setf swank::*enable-event-history* nil)"
                    "(setf swank:*log-events* t)"
                    (format nil
                            "(let ((swank::*loopback-interface* (uiop:hostname)))
                               (swank:create-server :port ~D :dont-close t))"
                            ,port)))
            (,process (inferior-lisp:make-process "sbcl"
                                             :startup-code startup)))
       (inferior-lisp:start ,process)
       (sleep 2)
       (unwind-protect
            (progn
              ,@body)
         (inferior-lisp:kill ,process)))))

(defvar *port* 40000)

(defmacro with-connection ((conn &key logp) &body body)
  (let ((port (gensym)))
    `(let ((,port (incf *port*)))
       (with-inferior-lisp (,port)
         (let ((,conn (swank-protocol:make-connection (uiop:hostname)
                                                      ,port
                                                      :logp ,logp)))
           (is-true
            (swank-protocol:connect ,conn))
           ,@body)))))

(defmacro with-response ((conn name request) &body body)
  (let ((response-string (gensym)))
    `(progn
       (is
        (stringp ,request))
       (let* ((,response-string (swank-protocol:read-message ,conn))
              (,name (swank-protocol:parse-response ,response-string)))
         ,@body))))

(defmacro with-repl ((conn) &body body)
  `(progn
     ;; Create REPL
     (with-response (,conn resp (swank-protocol:request-init-presentations ,conn))
       (is
        (equal (getf resp :request-id) 1)))
     (with-response (,conn resp (swank-protocol:request-create-repl ,conn))
       (is
        (equal (getf resp :request-id) 2)))
     ,@body))

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
    (is-false
      (swank-protocol:message-waiting-p conn))
    (is-true
     (stringp
      (swank-protocol:send-message-string
       conn
       "(:emacs-rex (swank:connection-info) \"COMMON-LISP-USER\" t 1)")))
    (is-true
     (progn
       (sleep 0.2) ;; Ensure the reply has been sent
       (swank-protocol:message-waiting-p conn)))
    (let ((resp (swank-protocol:read-message-string conn)))
      (is
       (stringp resp))
      (is-false
       (swank-protocol:message-waiting-p conn)))))

(test (connection-info :depends-on basic-requests)
  (with-connection (conn)
    (with-response (conn resp (swank-protocol:request-connection-info conn))
      (is
       (equal (getf resp :request-id) 1))
      (is
       (equal (getf resp :status) :ok))
      (is
       (equal (getf (getf resp :value) :style)
              :spawn)))
    (with-response (conn resp (swank-protocol:request-swank-require conn
                                                                    '(swank-repl)))
      (is
       (equal (getf resp :request-id) 2))
      (is
       (equal (getf resp :status) :ok)))))

(test (repl :depends-on basic-requests)
  (with-connection (conn)
    (with-repl (conn)
      ;; Evaluate
      (finishes
       (swank-protocol:request-listener-eval conn "(+ 2 2)"))
      ;; Wait for all the presentations to show
      (sleep 0.1)
      (let ((messages (swank-protocol:read-all-messages conn)))
        (print messages)
        (is
         (equal (length messages) 5))
        (is
         (equal (list :presentation-start
                      :write-string
                      :presentation-end
                      :write-string
                      :return)
                (loop for i from 0 to 4 collecting
                  (first (nth i messages)))))))))

(test (debugging :depends-on repl)
  (with-connection (conn)
    (with-repl (conn)
      ;; Trigger an error
      (finishes
       (swank-protocol:request-listener-eval conn "(error \"message\")"))
      ;; Read debugging messages
      (let ((debug-msg (swank-protocol:read-message conn)))
        (is
         (equal (first debug-msg)
                :debug))
        (let ((info (swank-protocol:parse-debug debug-msg)))
          (is
           (integerp (getf info :thread)))
          (is
           (equal (getf info :level)
                  1))
          (is
           (every #'stringp (getf info :condition)))))
      (let ((debug-msg (swank-protocol:read-message conn)))
        (is
         (equal (first debug-msg)
                :debug-activate)))
      ;; Leave the debugger
      (swank-protocol:request-throw-to-toplevel conn)
      (let ((message (swank-protocol:read-message conn)))
        (is (equal (first message)
                   :return)))))
  (with-connection (conn)
    (with-repl (conn)
      ;; Trigger an error
      (finishes
       (swank-protocol:request-listener-eval conn "(error \"message\")"))
      ;; Read debug message and do nothing
      (swank-protocol:read-message conn)
      ;; Invoke a restart
      (finishes
       (swank-protocol:request-invoke-restart conn 1 1))
      ;; Read all messages
      (sleep 0.1)
      (let ((messages (swank-protocol:read-all-messages conn)))
        (print messages)))))

(test (standard-input :depends-on repl)
  (with-connection (conn)
    (with-repl (conn)
      ;; Call READ
      (finishes
       (swank-protocol:request-listener-eval conn "(read)"))
      ;; Read the request for input message
      (let ((message (swank-protocol:read-message conn)))
        (is
         (equal (first message)
                :read-string))
        (is
         (equal (second message)
                2))
        (is
         (equal (third message)
                1)))
      ;; Send some input
      (finishes
       (swank-protocol:request-input-string-newline conn "1"))
      ;; Wait for all the presentations to show
      (sleep 0.1)
      (let ((messages (swank-protocol:read-all-messages conn)))
        (is
         (equal (length messages)
                5))
        (is
         (equal (list :presentation-start
                      :write-string
                      :presentation-end
                      :write-string
                      :return)
                (loop for i from 0 to 4 collecting
                  (first (nth i messages)))))))))

(run! 'tests)

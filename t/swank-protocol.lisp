(in-package :cl-user)
(defpackage swank-protocol-test
  (:use :cl :fiveam))
(in-package :swank-protocol-test)

;;; Utilities

(defun start-swank (hostname port)
  (setf swank:*configure-emacs-indentation* nil
        swank::*enable-event-history* nil
        swank:*log-events* t)
  (let ((swank::*loopback-interface* hostname))
    (swank:create-server :port port :dont-close t)))

;;; Tests

(def-suite tests
  :description "swank-protocol tests.")
(in-suite tests)

(test basic
  (let ((hostname (uiop:hostname))
        (port (find-ports:find-port)))
    (finishes
      (start-swank hostname port))
    (sleep 3)
    (let ((connection (swank-protocol:make-connection hostname port)))
      (is-true
       (swank-protocol:connect connection))
      (is-true
       (stringp
        (swank-protocol:send-message connection
                                     "(:emacs-rex (swank:connection-info) \"COMMON-LISP-USER\" t 1)"))))
    (finishes
      (swank:stop-server port))))

(run! 'tests)

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
    (princ "Starting server")
    (finishes
     (start-swank hostname port))
    (sleep 3)
    (finishes
     (swank:stop-server port))))

(run! 'tests)

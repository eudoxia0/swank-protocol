(defsystem swank-protocol-test
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :depends-on (:swank-protocol
               :swank
               :fiveam
               :alexandria
               :find-port)
  :components ((:module "t"
                :serial t
                :components
                ((:file "swank-protocol")))))

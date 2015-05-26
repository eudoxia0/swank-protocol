(defsystem swank-protocol-test
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :depends-on (:swank-protocol
               :fiveam
               :external-program
               :alexandria)
  :components ((:module "t"
                :serial t
                :components
                ((:file "swank-protocol")))))

(defsystem swank-protocol
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :maintainer "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :version "0.1"
  :homepage "https://github.com/eudoxia0/swank-protocol"
  :bug-tracker "https://github.com/eudoxia0/swank-protocol/issues"
  :source-control (:git "git@github.com:eudoxia0/swank-protocol.git")
  :depends-on (:usocket
               :swank)
  :components ((:module "src"
                :serial t
                :components
                ((:file "swank-protocol"))))
  :description "A low-level Swank client."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op swank-protocol-test))))

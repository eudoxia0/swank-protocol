(defsystem swank-protocol
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :maintainer "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :version "0.1"
  :homepage ""
  :bug-tracker ""
  :source-control (:git "")
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

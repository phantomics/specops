;;;; specops.asd

(asdf:defsystem #:specops
  :description "A framework for creating assemblers."
  :author "Andrew Sengul"
  :license  "BSD"
  :version "0.1.2"
  :serial t
  :pathname "src/"
  :in-order-to ((asdf:test-op (asdf:test-op #:specops/test)))
  :components ((:file "package")
               (:file "checksum")
               (:file "serial")
               (:module "manifest"
                :serial t
                :components ((:file "scan")
                             (:file "manifest")))
               (:file "assembler")
               (:file "program")))

(asdf:defsystem #:specops/test
  :description "Test suite for SpecOps (FiveAM and cl-hamcrest)."
  :author "Andrew Sengul"
  :license  "BSD"
  :version "0.1.2"
  :serial t
  :depends-on (#:specops #:specops.map.ebcdic #:fiveam #:hamcrest/fiveam)
  :pathname "test/"
  :components ((:file "package")
               (:file "helpers")
               (:file "test-serial")
               (:file "test-manifest-basics")
               (:file "test-manifest-fields")
               (:file "test-manifest-records")
               (:file "test-manifest-variable")
               (:file "test-manifest-reads")
               (:file "test-manifest-destinations")
               (:file "test-manifest-formats")
               (:file "test-manifest-arrays")
               (:file "test-manifest-ref"))
  :perform (asdf:test-op (o s)
             (uiop:symbol-call '#:specops-tests '#:run-all-tests)))

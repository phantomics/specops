;;;; specops.asd

(asdf:defsystem #:specops
  :description "A framework for creating assemblers."
  :author "Andrew Sengul"
  :license  "BSD"
  :version "0.0.1"
  :serial t
  :pathname "src/"
  :components ((:file "package")
               (:file "checksum")
               (:file "scan")
               (:file "specops")
               (:file "program")))

(asdf:defsystem #:specops/demo.png
  :description "PNG manifest demo and 1x1 round-trip check."
  :author "Andrew Sengul"
  :license  "BSD"
  :version "0.0.1"
  :serial t
  :depends-on (#:specops #:specops.map.ebcdic)
  :pathname "test/"
  :components ((:file "manifest")))

;;;; specops.asd

(asdf:defsystem #:specops
  :description "A framework for creating assemblers."
  :author "Andrew Sengul"
  :license  "BSD"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "specops")))

(asdf:defsystem #:specops/format.ebcdic
  :description "A set of EBCDIC-Unicode conversion tables."
  :author "Andrew Sengul"
  :license  "BSD"
  :version "0.0.1"
  :serial t
  :depends-on (#:specops)
  :components ((:file "formats/ebcdic")))

(asdf:defsystem #:specops/format.cp437
  :description "A Code Page 437-Unicode conversion table."
  :author "Andrew Sengul"
  :license  "BSD"
  :version "0.0.1"
  :serial t
  :depends-on (#:specops)
  :components ((:file "formats/cp437")))

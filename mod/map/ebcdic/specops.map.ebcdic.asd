;;;; specops.map.ebcdic.asd

(asdf:defsystem #:specops.map.ebcdic
  :description "A set of EBCDIC-Unicode conversion tables."
  :author "Andrew Sengul"
  :license  "BSD"
  :version "0.0.1"
  :serial t
  :depends-on (#:specops)
  :components ((:file "ebcdic")))

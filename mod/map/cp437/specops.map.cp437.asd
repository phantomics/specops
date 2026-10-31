;;;; specops.map.cp437.asd

(asdf:defsystem #:specops.map.cp437
  :description "A Code Page 437-Unicode conversion table."
  :author "Andrew Sengul"
  :license  "BSD"
  :version "0.0.1"
  :serial t
  :depends-on (#:specops)
  :components ((:file "cp437")))

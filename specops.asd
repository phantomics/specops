;;;; specops.asd

(asdf:defsystem #:specops
  :description "A framework for creating assemblers."
  :author "Andrew Sengul"
  :license  "BSD"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "specops")))

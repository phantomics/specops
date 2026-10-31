;;;; specops.arch.6502.asd

(asdf:defsystem #:specops.arch.6502
  :description "Describe 6502 here"
  :author "Andrew Sengul"
  :license  "BSD"
  :version "0.0.1"
  :serial t
  :depends-on ("specops")
  :components ((:file "package")
               (:file "base")
               (:file "ops")))

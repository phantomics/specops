;;;; z80.asd

(asdf:defsystem #:specops.z80
  :description "Common Lisp assembler framework for z80 processor based on SpecOps."
  :author "Andrew Sengul"
  :license  "BSD"
  :version "0.0.1"
  :serial t
  :depends-on ("specops")
  :components ((:file "package")
               (:file "base")
               (:file "templates")
               (:file "ops")))

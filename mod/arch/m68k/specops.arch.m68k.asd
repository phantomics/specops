;;;; specops.arch.m68k.asd

(asdf:defsystem #:specops.arch.m68k
  :description "Assembler framework for Motorola 68000 architectures based on SpecOps."
  :author "Andrew Sengul"
  :license "BSD"
  :version "0.0.1"
  :serial t
  :depends-on ("specops")
  :components ((:file "package")
               (:file "base")
               (:file "templates")
               (:file "ops")))

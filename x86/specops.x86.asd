;;;; specops.x86.asd

(asdf:defsystem #:specops.x86
  :description "Assembler framework for x86 architectures based on SpecOps."
  :author "Andrew Sengul"
  :license "BSD-3"
  :version "0.0.1"
  :serial t
  :depends-on ("specops")
  :components ((:file "package")
               (:file "base")
               (:file "templates")
               (:file "ops.main")))

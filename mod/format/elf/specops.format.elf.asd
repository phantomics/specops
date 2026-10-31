;;;; specops.format.elf.asd

(asdf:defsystem #:specops.format.elf
  :description "ELF64 executable writer for SpecOps programs."
  :author "Andrew Sengul"
  :license  "BSD"
  :version "0.0.1"
  :serial t
  :depends-on (#:specops)
  :components ((:file "elf")))

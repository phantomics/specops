;;;; specops.format.wasm.asd

(asdf:defsystem #:specops.format.wasm
  :description "WebAssembly binary module writer for SpecOps programs."
  :author "Andrew Sengul"
  :license  "BSD"
  :version "0.0.1"
  :serial t
  :depends-on (#:specops #:specops.arch.wasm)
  :components ((:file "program")))

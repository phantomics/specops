;;;; specops.arch.wasm.asd

(asdf:defsystem #:specops.arch.wasm
  :description "Describe wasm here"
  :author "Your Name <your.name@example.com>"
  :license  "BSD"
  :version "0.0.1"
  :serial t
  :depends-on (#:specops)
  :components ((:file "package")
               (:file "base")))

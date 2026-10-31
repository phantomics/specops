;;;; specops.arch.superh.asd

(asdf:defsystem #:specops.arch.superh
  :description "Describe super-h here"
  :author "Andrew Sengul"
  :license  "BSD"
  :version "0.0.1"
  :serial t
  :depends-on ("specops")
  :components ((:file "package")
               (:file "base")
               (:file "ops")))

;;;; specops.arch.s390x.asd

(asdf:defsystem #:specops.arch.s390x
  :description "Describe system-z here"
  :author "Your Name <your.name@example.com>"
  :license  "BSD"
  :version "0.0.1"
  :serial t
  :depends-on ("specops")
  :components ((:file "package")
               (:file "base")
               (:file "ops")))

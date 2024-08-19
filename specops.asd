;;;; specops.asd

(asdf:defsystem #:specops
  :description "A framework for creating assemblers."
  :author "Andrew Sengul"
  :license  "BSD-3"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "specops")
               ;; (:file "x86")
               ;; (:file "x86-setup")
               ;; (:file "x86-code")
               ))

;;;; specops.format.goff.asd

(asdf:defsystem #:specops.format.goff
  :description "GOFF (Generalized Object File Format) object writer for SpecOps programs."
  :author "Andrew Sengul"
  :license  "BSD"
  :version "0.0.1"
  :serial t
  :depends-on (#:specops)
  :components ((:file "goff")))

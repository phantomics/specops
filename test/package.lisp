;;;; test/package.lisp — Test package for SpecOps.
;;;;
;;;; Square-conforming (see Square.md §5). Uses FiveAM for defining and running
;;;; tests and cl-hamcrest for structural assertions.

(defpackage #:specops-tests
  (:use #:cl #:specops)
  (:import-from #:fiveam
                #:def-suite #:in-suite #:def-test
                #:is #:is-true #:is-false #:signals #:finishes
                #:explain! #:results-status #:run)
  (:import-from #:hamcrest/fiveam
                #:assert-that)
  (:import-from #:hamcrest/matchers
                #:has-all #:has-slots #:has-type)
  ;; Internals the tests reach: the portable float functions, stream sources, the field plan
  ;; (RULE names a scan plan's slot) and the manifest registry, and symbols that expansion
  ;; checks look for in generated code.
  (:import-from #:specops
                #:*manifests*
                #:portable-decode-float #:portable-encode-float
                #:make-stream-source #:fill-stream-source #:stream-source-p #:source-units
                #:field-plan #:known-count #:known-extent #:rule
                #:check-failed #:grow-buffer #:manifest-buffer-array
                #:deserializer-for
                ;; the code builder for binary values, internal until proven
                #:unit-order #:unit-significances #:make-encoding
                #:entry-target #:vector-target #:general-source #:vector-source
                #:store-form #:store-code-form #:load-form #:extent-check-form)
  (:export #:run-all-tests
           #:run-suite))

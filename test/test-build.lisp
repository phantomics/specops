;;;; test-scripts/test-build.lisp — Tests for build-program
;;;
;;; Run with: sbcl --noinform --non-interactive --load test-scripts/test-build.lisp

(load "package.lisp")
(load "specops.lisp")
(load "program.lisp")
(load "system-z/package.lisp")
(load "system-z/base.lisp")
(load "system-z/ops.lisp")
(load "system-z/program.lisp")

(in-package :specops.system-z)

(format t "~%=== Testing build-program ===~%~%")

;; Test 1: Compare build-program output with existing assemble output
(format t "--- Test 1: Round-trip vs assemble ---~%")
(let* (;; Existing assembler approach
       (expected (assemble *assembler-prototype-z*
                   (:lhi 1 10) (:lhi 2 20) (:lhi 3 3)
                   (:lr  4 1)  (:ar  4 2)  (:mr  4 3)))
       ;; New program model approach
       (pgm (program *assembler-prototype-z*
               (params (:name "TEST1"))
               (code "C_CODE64"
                 (:lhi 1 10) (:lhi 2 20) (:lhi 3 3)
                 (:lr  4 1)  (:ar  4 2)  (:mr  4 3)))))
  (build-program pgm)
  (let* ((unit (first (pgm-units pgm)))
         (seg (lookup-segment unit "C_CODE64"))
         (got (seg-bytes seg)))
    (format t "  expected: ~a~%" expected)
    (format t "  got:      ~a~%" got)
    (format t "  match: ~a~%" (equalp expected got))
    (assert (equalp expected got) () "Test 1 FAILED: byte vectors don't match")))

;; Test 2: Verify assembled output matches assemble for a different sequence
(format t "~%--- Test 2: Second round-trip ---~%")
(let* ((expected (assemble *assembler-prototype-z*
                   (:lhi 1 10) (:lhi 2 20) (:ar 1 2)))
       (pgm (program *assembler-prototype-z*
               (params (:name "TEST2"))
               (code "C_CODE64"
                 (:lhi 1 10) (:lhi 2 20) (:ar 1 2)))))
  (build-program pgm)
  (let* ((unit (first (pgm-units pgm)))
         (seg (lookup-segment unit "C_CODE64"))
         (got (seg-bytes seg)))
    (format t "  expected: ~a~%" expected)
    (format t "  got:      ~a~%" got)
    (assert (equalp expected got) () "Test 2 FAILED: bytes don't match")))

;; Test 3: Function with symbol table
(format t "~%--- Test 3: Function item ---~%")
(let ((pgm (program *assembler-prototype-z*
             (params (:name "TEST3"))
             (code "C_CODE64"
               (function main (:binding :global)
                 (:lhi 1 10)
                 (:lhi 2 20)
                 (:ar  1 2))))))
  (build-program pgm)
  (let* ((unit (first (pgm-units pgm)))
         (seg (lookup-segment unit "C_CODE64"))
         (sym (lookup-symbol unit 'main)))
    (format t "  bytes: ~a~%" (seg-bytes seg))
    (format t "  seg-size: ~a~%" (seg-size seg))
    (format t "  symbol 'main': offset=~a, size=~a, type=~a, binding=~a~%"
            (sym-offset sym) (sym-size sym) (sym-type sym) (sym-binding sym))
    (assert (= 0 (sym-offset sym)) () "Test 3 FAILED: main should start at offset 0")
    (assert (eq :function (sym-type sym)) () "Test 3 FAILED: main should be :function type")
    (assert (eq :global (sym-binding sym)) () "Test 3 FAILED: main should be :global")))

;; Test 4: Data segment with big-endian encoding
(format t "~%--- Test 4: Data segment ---~%")
(let ((pgm (program *assembler-prototype-z*
             (params (:name "TEST4"))
             (data "C_WSA64"
               (my-word (:word #xDEADBEEF))
               (my-half (:half #xCAFE))
               (align 8)
               (my-byte (:byte #x42))))))
  (build-program pgm)
  (let* ((unit (first (pgm-units pgm)))
         (seg (lookup-segment unit "C_WSA64"))
         (bytes (seg-bytes seg)))
    (format t "  bytes: ~a~%" bytes)
    (format t "  size: ~a~%" (seg-size seg))
    ;; Check big-endian encoding of #xDEADBEEF
    (format t "  first 4 bytes: ~2,'0X ~2,'0X ~2,'0X ~2,'0X~%"
            (aref bytes 0) (aref bytes 1) (aref bytes 2) (aref bytes 3))
    (assert (= #xDE (aref bytes 0)) () "Test 4 FAILED: big-endian byte 0")
    (assert (= #xAD (aref bytes 1)) () "Test 4 FAILED: big-endian byte 1")
    (assert (= #xBE (aref bytes 2)) () "Test 4 FAILED: big-endian byte 2")
    (assert (= #xEF (aref bytes 3)) () "Test 4 FAILED: big-endian byte 3")
    ;; Check halfword #xCAFE
    (format t "  half at 4: ~2,'0X ~2,'0X~%"
            (aref bytes 4) (aref bytes 5))
    (assert (= #xCA (aref bytes 4)) () "Test 4 FAILED: halfword high byte")
    (assert (= #xFE (aref bytes 5)) () "Test 4 FAILED: halfword low byte")
    ;; Check alignment: my-byte should be at offset 8
    (let ((sym-byte (lookup-symbol unit 'my-byte)))
      (format t "  my-byte offset: ~a~%" (sym-offset sym-byte))
      (assert (= 8 (sym-offset sym-byte)) ()
              "Test 4 FAILED: my-byte should be at offset 8 after align"))))

;; Test 5: Multiple functions with sequential offsets
(format t "~%--- Test 5: Multiple functions ---~%")
(let ((pgm (program *assembler-prototype-z*
             (params (:name "TEST5"))
             (code "C_CODE64"
               (function first-fn (:binding :global)
                 (:lhi 1 1))
               (function second-fn (:binding :global)
                 (:lhi 2 2))))))
  (build-program pgm)
  (let* ((unit (first (pgm-units pgm)))
         (sym1 (lookup-symbol unit 'first-fn))
         (sym2 (lookup-symbol unit 'second-fn)))
    (format t "  first-fn: offset=~a, size=~a~%" (sym-offset sym1) (sym-size sym1))
    (format t "  second-fn: offset=~a, size=~a~%" (sym-offset sym2) (sym-size sym2))
    (assert (= 0 (sym-offset sym1)) () "Test 5 FAILED: first-fn at 0")
    (assert (< 0 (sym-offset sym2)) () "Test 5 FAILED: second-fn after first-fn")
    (assert (= (sym-offset sym2) (sym-size sym1)) ()
            "Test 5 FAILED: second-fn starts where first-fn ends")))

;; Test 6: Mixed code and data segments
(format t "~%--- Test 6: Mixed code + data ---~%")
(let ((pgm (program *assembler-prototype-z*
             (params (:name "TEST6"))
             (data "C_WSA64"
               (counter (:word 0))
               (max-val (:word #x7FFFFFFF)))
             (code "C_CODE64"
               (function main (:binding :global)
                 (:lhi 1 10)
                 (:ar  1 2))))))
  (build-program pgm)
  (let* ((unit (first (pgm-units pgm)))
         (code-seg (lookup-segment unit "C_CODE64"))
         (data-seg (lookup-segment unit "C_WSA64")))
    (format t "  code size: ~a~%" (seg-size code-seg))
    (format t "  data size: ~a~%" (seg-size data-seg))
    (format t "  code bytes: ~a~%" (seg-bytes code-seg))
    (format t "  data bytes: ~a~%" (seg-bytes data-seg))
    (assert (< 0 (seg-size code-seg)) () "Test 6 FAILED: code should have bytes")
    (assert (= 8 (seg-size data-seg)) () "Test 6 FAILED: data should be 8 bytes")))

(format t "~%=== All build-program tests passed ===~%")

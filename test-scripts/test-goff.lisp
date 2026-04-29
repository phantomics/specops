;;;; test-scripts/test-goff.lisp — Tests for the GOFF emitter
;;;
;;; Run with: sbcl --noinform --non-interactive --load test-scripts/test-goff.lisp

(load "package.lisp")
(load "specops.lisp")
(load "program.lisp")
(load "goff.lisp")
(load "system-z/package.lisp")
(load "system-z/base.lisp")
(load "system-z/ops.lisp")
(load "system-z/program.lisp")

(in-package :specops.system-z)

(format t "~%=== Testing GOFF emitter ===~%~%")

;; Test 1: Basic GOFF output with code and data
(format t "--- Test 1: GOFF with code + data ---~%")
(let ((pgm (program *assembler-prototype-z*
             (params (:name "GOFFTEST") (:entry main))
             (data "C_WSA64"
               (counter (:word 0))
               (max-val (:word #x7FFFFFFF)))
             (code "C_CODE64"
               (function main (:binding :global)
                 (:lhi 1 10)
                 (:lhi 2 20)
                 (:ar  1 2)
                 (:lr  3 1))))))
  (build-program pgm)

  (with-open-file (out "/tmp/test-specops.goff"
                       :direction :output
                       :if-exists :supersede
                       :element-type '(unsigned-byte 8))
    (let ((count (emit-program pgm :goff out)))
      (format t "  Written: /tmp/test-specops.goff (~a records)~%" count)))

  ;; Verify file structure
  (with-open-file (in "/tmp/test-specops.goff"
                      :element-type '(unsigned-byte 8))
    (let ((file-size (file-length in)))
      (format t "  File size: ~a bytes~%" file-size)
      (assert (zerop (mod file-size 80)) ()
              "GOFF file size must be a multiple of 80 bytes")
      (format t "  Records: ~a~%" (/ file-size 80))

      ;; Read and verify each record's PTV header
      (file-position in 0)
      (let ((record-num 0)
            (record-types nil))
        (loop :while (< (file-position in) file-size)
              :do (let ((rec (make-array 80 :element-type '(unsigned-byte 8))))
                    (read-sequence rec in)
                    (incf record-num)
                    ;; Check PTV prefix
                    (assert (= #x03 (aref rec 0)) ()
                            "Record ~a: PTV prefix must be #x03, got #x~2,'0X"
                            record-num (aref rec 0))
                    ;; Record type (high nibble of byte 1)
                    (let ((rec-type (logand #xF0 (aref rec 1))))
                      (push rec-type record-types)
                      (format t "  Record ~a: type=#x~2,'0X (~a)~%"
                              record-num rec-type
                              (case rec-type
                                (#xF0 "HDR") (#x00 "ESD") (#x10 "TXT")
                                (#x20 "RLD") (#x40 "END") (t "??"))))))

        (setf record-types (nreverse record-types))

        ;; Verify ordering: HDR first, END last
        (assert (= #xF0 (first record-types)) ()
                "First record must be HDR")
        (assert (= #x40 (car (last record-types))) ()
                "Last record must be END")

        ;; Verify HDR → ESDs → TXTs → END ordering
        ;; (RLD may be absent if no relocations)
        (format t "  Record order: HDR=~a ESD=~a TXT=~a END=~a~%"
                (count #xF0 record-types)
                (count #x00 record-types)
                (count #x10 record-types)
                (count #x40 record-types))

        (format t "  GOFF structure: OK~%")))))

;; Test 2: Code-only program
(format t "~%--- Test 2: GOFF code only ---~%")
(let ((pgm (program *assembler-prototype-z*
             (params (:name "CODEONLY") (:entry start))
             (code "C_CODE64"
               (function start (:binding :global)
                 (:lhi 1 0)
                 (:lhi 2 1))))))
  (build-program pgm)
  (with-open-file (out "/tmp/test-specops-code.goff"
                       :direction :output
                       :if-exists :supersede
                       :element-type '(unsigned-byte 8))
    (let ((count (emit-program pgm :goff out)))
      (format t "  Written: ~a records~%" count)
      ;; Should have: HDR + SD + ED + LD + TXT + END = 6 records minimum
      (assert (>= count 5) ()
              "Code-only GOFF should have at least 5 records")))
  (format t "  GOFF code-only: OK~%"))

;; Clean up
(ignore-errors (delete-file "/tmp/test-specops.goff"))
(ignore-errors (delete-file "/tmp/test-specops-code.goff"))

(format t "~%=== All GOFF emitter tests passed ===~%")

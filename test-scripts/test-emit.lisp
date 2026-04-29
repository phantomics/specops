;;;; test-scripts/test-emit.lisp — Tests for ELF and WASM emitters
;;;
;;; Run with: sbcl --noinform --non-interactive --load test-scripts/test-emit.lisp

(load "package.lisp")
(load "specops.lisp")
(load "program.lisp")
(load "elf.lisp")
(load "system-z/package.lisp")
(load "system-z/base.lisp")
(load "system-z/ops.lisp")
(load "system-z/program.lisp")
(load "wasm/package.lisp")
(load "wasm/base.lisp")
(load "wasm/program.lisp")

;; ═══════════════════════════════════════════════════════════════
;; Test 1: ELF emitter with System Z
;; ═══════════════════════════════════════════════════════════════
(in-package :specops.system-z)

(format t "~%=== Test 1: ELF emitter (System Z) ===~%")
(let ((pgm (program *assembler-prototype-z*
             (params (:name "ELFTEST") (:entry main))
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

  (with-open-file (out "/tmp/test-specops.elf"
                       :direction :output
                       :if-exists :supersede
                       :element-type '(unsigned-byte 8))
    (let ((size (emit-program pgm :elf out)))
      (format t "  Written: /tmp/test-specops.elf (~a bytes)~%" size)))

  ;; Verify with readelf
  (format t "~%  readelf -h:~%")
  (format t "~a"
          (with-output-to-string (s)
            (sb-ext:run-program "readelf" '("-h" "/tmp/test-specops.elf")
                                :output s :error s :search t)))

  (format t "  readelf -S:~%")
  (format t "~a"
          (with-output-to-string (s)
            (sb-ext:run-program "readelf" '("-S" "/tmp/test-specops.elf")
                                :output s :error s :search t)))

  (format t "  readelf -s:~%")
  (format t "~a"
          (with-output-to-string (s)
            (sb-ext:run-program "readelf" '("-s" "/tmp/test-specops.elf")
                                :output s :error s :search t)))

  ;; Verify ELF header bytes directly
  (with-open-file (in "/tmp/test-specops.elf"
                      :element-type '(unsigned-byte 8))
    (let ((hdr (make-array 16 :element-type '(unsigned-byte 8))))
      (read-sequence hdr in)
      ;; Check magic
      (assert (= #x7F (aref hdr 0)) () "ELF magic byte 0")
      (assert (= (char-code #\E) (aref hdr 1)) () "ELF magic byte 1")
      (assert (= (char-code #\L) (aref hdr 2)) () "ELF magic byte 2")
      (assert (= (char-code #\F) (aref hdr 3)) () "ELF magic byte 3")
      ;; Check class (ELF64 = 2)
      (assert (= 2 (aref hdr 4)) () "ELF class should be ELFCLASS64")
      ;; Check data encoding (big-endian = 2)
      (assert (= 2 (aref hdr 5)) () "ELF data should be ELFDATA2MSB")
      (format t "  ELF header verification: OK~%"))))

;; ═══════════════════════════════════════════════════════════════
;; Test 2: WASM emitter — add(i32, i32) -> i32
;; ═══════════════════════════════════════════════════════════════
(in-package :specops.wasm)

(format t "~%=== Test 2: WASM emitter (add function) ===~%")
(multiple-value-bind (pgm unit) (make-wasm-program)
  ;; Type section: (i32, i32) -> (i32)
  (let ((type-seg (lookup-segment unit "type")))
    (add-item type-seg (make-wasm-functype '(:i32 :i32) '(:i32)))
    (finalize-segment type-seg))

  ;; Export section
  (let ((export-seg (lookup-segment unit "export")))
    (add-item export-seg (make-wasm-export "add" :func 0))
    (finalize-segment export-seg))

  ;; Code section: function body
  (let ((code-seg (lookup-segment unit "code")))
    (let ((func (make-wasm-function 'add 0)))
      (setf (fi-body func)
            (list (make-instance 'instruction-item :expression '(:local.get 0))
                  (make-instance 'instruction-item :expression '(:local.get 1))
                  (make-instance 'instruction-item :expression '(:i32.add))))
      (add-item code-seg func)
      (finalize-segment code-seg)))

  ;; Write the module
  (with-open-file (out "/tmp/test-specops.wasm"
                       :direction :output
                       :if-exists :supersede
                       :element-type '(unsigned-byte 8))
    (emit-program pgm :wasm out))

  ;; Verify header bytes
  (with-open-file (in "/tmp/test-specops.wasm"
                      :element-type '(unsigned-byte 8))
    (let ((hdr (make-array 8 :element-type '(unsigned-byte 8))))
      (read-sequence hdr in)
      (format t "  Header: ~{#x~2,'0X ~}~%" (coerce hdr 'list))
      ;; Magic: \0asm
      (assert (= #x00 (aref hdr 0)) () "WASM magic byte 0")
      (assert (= #x61 (aref hdr 1)) () "WASM magic byte 1")
      (assert (= #x73 (aref hdr 2)) () "WASM magic byte 2")
      (assert (= #x6D (aref hdr 3)) () "WASM magic byte 3")
      ;; Version: 1
      (assert (= #x01 (aref hdr 4)) () "WASM version byte 0")
      (assert (= #x00 (aref hdr 5)) () "WASM version byte 1")
      (format t "  Magic + Version: OK~%")))

  ;; Verify against the expected byte-exact encoding
  ;; from the WASM spec for an add(i32,i32)->i32 module
  (let ((expected #(#x00 #x61 #x73 #x6D  ; magic
                    #x01 #x00 #x00 #x00  ; version
                    #x01                  ; section: Type
                    #x07                  ; size: 7
                    #x01                  ; count: 1
                    #x60                  ; func type
                    #x02 #x7F #x7F       ; params: 2 x i32
                    #x01 #x7F            ; results: 1 x i32
                    #x03                  ; section: Function
                    #x02                  ; size: 2
                    #x01                  ; count: 1
                    #x00                  ; type index 0
                    #x07                  ; section: Export
                    #x07                  ; size: 7
                    #x01                  ; count: 1
                    #x03 #x61 #x64 #x64  ; name: "add"
                    #x00                  ; kind: func
                    #x00                  ; index: 0
                    #x0A                  ; section: Code
                    #x09                  ; size: 9
                    #x01                  ; count: 1
                    #x07                  ; body size: 7
                    #x00                  ; locals: 0
                    #x20 #x00            ; local.get 0
                    #x20 #x01            ; local.get 1
                    #x6A                  ; i32.add
                    #x0B)))              ; end
    (let ((actual (with-open-file (in "/tmp/test-specops.wasm"
                                      :element-type '(unsigned-byte 8))
                    (let ((buf (make-array (file-length in)
                                           :element-type '(unsigned-byte 8))))
                      (read-sequence buf in)
                      buf))))
      (format t "  File size: ~a bytes (expected ~a)~%"
              (length actual) (length expected))
      (format t "  Byte-exact match: ~a~%" (equalp expected actual))
      (assert (equalp expected actual) ()
              "Test 2 FAILED: WASM output does not match expected encoding")))

  (format t "  WASM verification: OK~%"))

;; ═══════════════════════════════════════════════════════════════
;; Test 3: WASM with memory and data segment
;; ═══════════════════════════════════════════════════════════════
(format t "~%=== Test 3: WASM with memory + data ===~%")
(multiple-value-bind (pgm unit) (make-wasm-program)
  ;; Type section
  (let ((type-seg (lookup-segment unit "type")))
    (add-item type-seg (make-wasm-functype '() '(:i32)))
    (finalize-segment type-seg))

  ;; Memory section: 1 page minimum
  (let ((mem-seg (lookup-segment unit "memory")))
    (add-item mem-seg (make-wasm-memory 1))
    (finalize-segment mem-seg))

  ;; Export
  (let ((export-seg (lookup-segment unit "export")))
    (add-item export-seg (make-wasm-export "get" :func 0))
    (finalize-segment export-seg))

  ;; Code: function that loads from memory offset 0
  (let ((code-seg (lookup-segment unit "code")))
    (let ((func (make-wasm-function 'get-val 0)))
      (setf (fi-body func)
            (list (make-instance 'instruction-item :expression '(:i32.const 0))
                  (make-instance 'instruction-item :expression '(:i32.load 2 0))))
      (add-item code-seg func)
      (finalize-segment code-seg)))

  ;; Data: initialize memory at offset 0 with a value
  (let ((data-seg (lookup-segment unit "data")))
    (add-item data-seg
              (make-wasm-data #(#x2A #x00 #x00 #x00)  ; 42 in little-endian i32
                              :mode :active
                              :offset-expr '((:i32.const 0))))
    (finalize-segment data-seg))

  ;; Write
  (with-open-file (out "/tmp/test-specops-mem.wasm"
                       :direction :output
                       :if-exists :supersede
                       :element-type '(unsigned-byte 8))
    (emit-program pgm :wasm out))

  (let ((size (with-open-file (in "/tmp/test-specops-mem.wasm")
                (file-length in))))
    (format t "  Written: /tmp/test-specops-mem.wasm (~a bytes)~%" size))

  ;; Verify it starts with correct magic
  (with-open-file (in "/tmp/test-specops-mem.wasm"
                      :element-type '(unsigned-byte 8))
    (let ((magic (make-array 4 :element-type '(unsigned-byte 8))))
      (read-sequence magic in)
      (assert (equalp magic #(#x00 #x61 #x73 #x6D)) ()
              "Test 3 FAILED: wrong WASM magic")
      (format t "  WASM magic: OK~%"))))

;; Clean up temp files
(ignore-errors (delete-file "/tmp/test-specops.elf"))
(ignore-errors (delete-file "/tmp/test-specops.wasm"))
(ignore-errors (delete-file "/tmp/test-specops-mem.wasm"))

(format t "~%=== All emitter tests passed ===~%")

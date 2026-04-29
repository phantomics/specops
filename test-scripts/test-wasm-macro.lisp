;;;; test-scripts/test-wasm-macro.lisp — Tests for wasm-program macro
;;;
;;; Run with: sbcl --noinform --non-interactive --load test-scripts/test-wasm-macro.lisp

(load "package.lisp")
(load "specops.lisp")
(load "program.lisp")
(load "wasm/package.lisp")
(load "wasm/base.lisp")
(load "wasm/program.lisp")

(in-package :specops.wasm)

(format t "~%=== Testing wasm-program macro ===~%~%")

;; Test 1: Simple add function — must produce byte-exact match
(format t "--- Test 1: add(i32,i32)->i32 via macro ---~%")
(let ((pgm (wasm-program
             (types
               ((:i32 :i32) -> (:i32)))
             (export "add" :func 0)
             (func add ((:type 0))
               (:local.get 0)
               (:local.get 1)
               (:i32.add)))))

  ;; Write to file
  (with-open-file (out "/tmp/test-wasm-macro.wasm"
                       :direction :output :if-exists :supersede
                       :element-type '(unsigned-byte 8))
    (emit-program pgm :wasm out))

  ;; Compare against known-correct encoding
  (let ((expected #(#x00 #x61 #x73 #x6D  ; magic
                    #x01 #x00 #x00 #x00  ; version
                    #x01 #x07            ; Type section, 7 bytes
                    #x01 #x60           ; 1 functype
                    #x02 #x7F #x7F     ; 2 params: i32,i32
                    #x01 #x7F          ; 1 result: i32
                    #x03 #x02          ; Function section, 2 bytes
                    #x01 #x00          ; 1 func, type 0
                    #x07 #x07          ; Export section, 7 bytes
                    #x01               ; 1 export
                    #x03 #x61 #x64 #x64 ; "add"
                    #x00 #x00          ; func index 0
                    #x0A #x09          ; Code section, 9 bytes
                    #x01               ; 1 function
                    #x07               ; body: 7 bytes
                    #x00               ; 0 locals
                    #x20 #x00          ; local.get 0
                    #x20 #x01          ; local.get 1
                    #x6A               ; i32.add
                    #x0B)))            ; end
    (let ((actual (with-open-file (in "/tmp/test-wasm-macro.wasm"
                                      :element-type '(unsigned-byte 8))
                    (let ((buf (make-array (file-length in)
                                           :element-type '(unsigned-byte 8))))
                      (read-sequence buf in) buf))))
      (format t "  Size: ~a bytes (expected ~a)~%" (length actual) (length expected))
      (format t "  Byte-exact match: ~a~%" (equalp expected actual))
      (assert (equalp expected actual) ()
              "Test 1 FAILED: wasm-program output doesn't match expected bytes")))
  (format t "  OK~%"))

;; Test 2: Module with memory and data
(format t "~%--- Test 2: Memory + data + function ---~%")
(let ((pgm (wasm-program
             (types
               (() -> (:i32)))
             (memory 1)
             (export "get" :func 0)
             (data ((:i32.const 0)) #(#x2A #x00 #x00 #x00))
             (func get-val ((:type 0))
               (:i32.const 0)
               (:i32.load 2 0)))))

  (with-open-file (out "/tmp/test-wasm-macro2.wasm"
                       :direction :output :if-exists :supersede
                       :element-type '(unsigned-byte 8))
    (emit-program pgm :wasm out))

  ;; Verify magic and that it has the right sections
  (with-open-file (in "/tmp/test-wasm-macro2.wasm"
                      :element-type '(unsigned-byte 8))
    (let ((magic (make-array 4 :element-type '(unsigned-byte 8))))
      (read-sequence magic in)
      (assert (equalp magic #(#x00 #x61 #x73 #x6D)) () "Bad magic"))
    ;; Skip version
    (dotimes (i 4) (read-byte in))
    ;; Read section IDs
    (let ((sections nil))
      (loop :for byte = (read-byte in nil nil)
            :while byte
            :do (when (<= byte 13)
                  (pushnew byte sections)
                  ;; Read section size and skip body
                  (let ((size 0) (shift 0))
                    (loop :for b = (read-byte in)
                          :do (setf size (logior size (ash (logand b #x7F) shift)))
                              (incf shift 7)
                          :while (plusp (logand b #x80)))
                    (dotimes (i size) (read-byte in nil)))))
      (setf sections (sort sections #'<))
      (format t "  Sections present: ~a~%" sections)
      ;; Should have: Type(1), Function(3), Memory(5), Export(7), Code(10), Data(11)
      (assert (member 1 sections) () "Missing Type section")
      (assert (member 3 sections) () "Missing Function section")
      (assert (member 5 sections) () "Missing Memory section")
      (assert (member 7 sections) () "Missing Export section")
      (assert (member 10 sections) () "Missing Code section")
      (assert (member 11 sections) () "Missing Data section")))
  (format t "  OK~%"))

;; Test 3: Multiple functions
(format t "~%--- Test 3: Multiple functions ---~%")
(let ((pgm (wasm-program
             (types
               ((:i32 :i32) -> (:i32))
               ((:i32) -> (:i32)))
             (export "add" :func 0)
             (export "double" :func 1)
             (func add ((:type 0))
               (:local.get 0)
               (:local.get 1)
               (:i32.add))
             (func double-it ((:type 1))
               (:local.get 0)
               (:local.get 0)
               (:i32.add)))))

  (with-open-file (out "/tmp/test-wasm-macro3.wasm"
                       :direction :output :if-exists :supersede
                       :element-type '(unsigned-byte 8))
    (emit-program pgm :wasm out))

  (let ((size (with-open-file (in "/tmp/test-wasm-macro3.wasm") (file-length in))))
    (format t "  Size: ~a bytes~%" size)
    (assert (plusp size) () "File should not be empty"))
  (format t "  OK~%"))

;; Test 4: Function with locals and structured control flow
(format t "~%--- Test 4: Locals + block/loop ---~%")
(let ((pgm (wasm-program
             (types
               ((:i32) -> (:i32)))
             (export "fac" :func 0)
             (func factorial ((:type 0) (:locals (1 . :i32)))
               ;; result = 1
               (:i32.const 1)
               (:local.set 1)
               ;; loop while n > 0
               (block
                 (loop
                   ;; if n == 0, break
                   (:local.get 0)
                   (:i32.eqz)
                   (:br_if 1)
                   ;; result *= n
                   (:local.get 1)
                   (:local.get 0)
                   (:i32.mul)
                   (:local.set 1)
                   ;; n -= 1
                   (:local.get 0)
                   (:i32.const 1)
                   (:i32.sub)
                   (:local.set 0)
                   ;; continue loop
                   (:br 0)))
               ;; return result
               (:local.get 1)))))

  (with-open-file (out "/tmp/test-wasm-macro4.wasm"
                       :direction :output :if-exists :supersede
                       :element-type '(unsigned-byte 8))
    (emit-program pgm :wasm out))

  ;; Verify the file is valid (magic + non-trivial size)
  (with-open-file (in "/tmp/test-wasm-macro4.wasm"
                      :element-type '(unsigned-byte 8))
    (let ((magic (make-array 4 :element-type '(unsigned-byte 8))))
      (read-sequence magic in)
      (assert (equalp magic #(#x00 #x61 #x73 #x6D)) () "Bad magic")))
  (let ((size (with-open-file (in "/tmp/test-wasm-macro4.wasm") (file-length in))))
    (format t "  Size: ~a bytes~%" size)
    ;; Should be reasonably sized (type + func + export + code)
    (assert (> size 40) () "File too small for a factorial function"))
  (format t "  OK~%"))

;; Test 5: Globals
(format t "~%--- Test 5: Globals ---~%")
(let ((pgm (wasm-program
             (types
               (() -> (:i32)))
             (global :i32 42)
             (global :i32 :var 0)
             (export "getConst" :func 0)
             (func get-const ((:type 0))
               (:global.get 0)))))

  (with-open-file (out "/tmp/test-wasm-macro5.wasm"
                       :direction :output :if-exists :supersede
                       :element-type '(unsigned-byte 8))
    (emit-program pgm :wasm out))

  (let ((size (with-open-file (in "/tmp/test-wasm-macro5.wasm") (file-length in))))
    (format t "  Size: ~a bytes~%" size)
    (assert (plusp size) () "File should not be empty"))
  (format t "  OK~%"))

;; Cleanup
(ignore-errors (delete-file "/tmp/test-wasm-macro.wasm"))
(ignore-errors (delete-file "/tmp/test-wasm-macro2.wasm"))
(ignore-errors (delete-file "/tmp/test-wasm-macro3.wasm"))
(ignore-errors (delete-file "/tmp/test-wasm-macro4.wasm"))
(ignore-errors (delete-file "/tmp/test-wasm-macro5.wasm"))

(format t "~%=== All wasm-program macro tests passed ===~%")

;;;; test-scripts/test-program.lisp — Tests for the program macro
;;;
;;; Run with: sbcl --noinform --non-interactive --load test-scripts/test-program.lisp

(load "package.lisp")
(load "specops.lisp")
(load "program.lisp")
(load "system-z/package.lisp")
(load "system-z/base.lisp")
(load "system-z/program.lisp")

(in-package :specops.system-z)

(format t "~%=== Testing program macro ===~%~%")

;; Test 1: Minimal program with bare instructions
(format t "--- Test 1: Bare instructions ---~%")
(let ((pgm (program *assembler-prototype-z*
             (params (:name "MINIMAL") (:entry _start))
             (code ".text"
               (label _start :global)
               (:lhi 1 0)
               (:lhi 2 1)))))
  (let* ((unit (first (pgm-units pgm)))
         (seg (lookup-segment unit ".text")))
    (format t "  unit name: ~a~%" (pun-name unit))
    (format t "  entry: ~a~%" (pgm-entry-point pgm))
    (format t "  segment: ~a, kind: ~a~%" (seg-name seg) (seg-kind seg))
    (format t "  items: ~a~%" (length (seg-items seg)))
    (dolist (item (seg-items seg))
      (format t "    ~a: ~a~%"
              (type-of item)
              (typecase item
                (label-def-item (ldi-name item))
                (instruction-item (ii-expression item))
                (t "?"))))
    (assert (= 3 (length (seg-items seg))) ()
            "Test 1 FAILED: expected 3 items")
    (assert (eq '_start (pgm-entry-point pgm)) ()
            "Test 1 FAILED: wrong entry point")))

;; Test 2: Function (without :store — that requires domain setup)
(format t "~%--- Test 2: Function ---~%")
(let ((pgm (program *assembler-prototype-z*
             (params (:name "FUNCPROG")
                     (:entry main))
             (code "C_CODE64"
               (function main (:binding :global)
                 (:lhi 1 10)
                 (:lhi 2 20)
                 (:ar  1 2))))))
  (let* ((unit (first (pgm-units pgm)))
         (seg (lookup-segment unit "C_CODE64")))
    (format t "  unit: ~a~%" (pun-name unit))
    (format t "  items: ~a~%" (length (seg-items seg)))
    (let ((func (first (seg-items seg))))
      (format t "  function name: ~a~%" (fi-name func))
      (format t "  function options: ~a~%" (fi-properties func))
      (format t "  body items: ~a~%" (length (fi-body func)))
      (dolist (bi (fi-body func))
        (format t "    ~a: ~a~%"
                (type-of bi)
                (typecase bi
                  (instruction-item (ii-expression bi))
                  (t "?"))))
      (assert (= 3 (length (fi-body func))) ()
              "Test 2 FAILED: expected 3 body items")
      (assert (eq 'main (fi-name func)) ()
              "Test 2 FAILED: wrong function name"))))

;; Test 3: Data segment
(format t "~%--- Test 3: Data segment ---~%")
(let ((pgm (program *assembler-prototype-z*
             (params (:name "DATAPROG"))
             (data "C_WSA64"
               (my-const (:word #x00FF00FF))
               (my-bytes (raw #(#x01 #x02 #x03 #x04)))
               (align 8)
               (small-val #x42)))))
  (let* ((unit (first (pgm-units pgm)))
         (seg (lookup-segment unit "C_WSA64")))
    (format t "  segment: ~a, kind: ~a, flags: ~a~%"
            (seg-name seg) (seg-kind seg) (seg-flags seg))
    (format t "  items: ~a~%" (length (seg-items seg)))
    (dolist (item (seg-items seg))
      (format t "    ~a: ~a~%"
              (type-of item)
              (typecase item
                (label-def-item (format nil "label ~a" (ldi-name item)))
                (data-word-item (format nil "value=~a width=~a"
                                        (dwi-value item) (dwi-width item)))
                (raw-bytes-item (format nil "~a" (rbi-data item)))
                (align-item (format nil "boundary=~a" (ali-boundary item)))
                (t "?"))))
    (assert (= 7 (length (seg-items seg))) ()
            "Test 3 FAILED: expected 7 items (3 labels + 2 values + 1 raw + 1 align)")))

;; Test 4: Mixed code and data with multiple functions
(format t "~%--- Test 4: Mixed code and data ---~%")
(let ((pgm (program *assembler-prototype-z*
             (params (:name "MIXED") (:entry main))
             (data "C_WSA64"
               (counter (:word 0)))
             (code "C_CODE64"
               (function main (:binding :global)
                 (:lhi 1 10)
                 (:ar  1 2))
               (function helper ()
                 (:lr  3 4)
                 (:ar  3 5))))))
  (let* ((unit (first (pgm-units pgm)))
         (code-seg (lookup-segment unit "C_CODE64"))
         (data-seg (lookup-segment unit "C_WSA64")))
    (format t "  code items: ~a~%" (length (seg-items code-seg)))
    (format t "  data items: ~a~%" (length (seg-items data-seg)))
    (dolist (item (seg-items code-seg))
      (when (typep item 'function-item)
        (format t "    function ~a: ~a body items~%"
                (fi-name item) (length (fi-body item)))))
    (assert (= 2 (length (seg-items code-seg))) ()
            "Test 4 FAILED: expected 2 code items (2 functions)")
    (assert (= 2 (length (seg-items data-seg))) ()
            "Test 4 FAILED: expected 2 data items (label + value)")))

;; Test 5: Code with labels, align, and ref
(format t "~%--- Test 5: Labels, align, ref ---~%")
(let ((pgm (program *assembler-prototype-z*
             (params (:name "REFTEST"))
             (code ".text"
               (function compute ()
                 loop-top
                 (:ar 1 2)
                 (:lhi 3 0)
                 (align 4)
                 (:ar 3 4))
               (label exit-point :global))
             (data ".data"
               (jump-target (ref compute :width 8 :rel-type :r-390-64))))))
  (let* ((unit (first (pgm-units pgm)))
         (code-seg (lookup-segment unit ".text"))
         (data-seg (lookup-segment unit ".data")))
    ;; Check function body
    (let* ((func (first (seg-items code-seg)))
           (body (fi-body func)))
      (format t "  function ~a body:~%" (fi-name func))
      (dolist (bi body)
        (format t "    ~a: ~a~%"
                (type-of bi)
                (typecase bi
                  (label-def-item (format nil "label ~a" (ldi-name bi)))
                  (instruction-item (ii-expression bi))
                  (align-item (format nil "align ~a" (ali-boundary bi)))
                  (t "?"))))
      (assert (= 5 (length body)) ()
              "Test 5 FAILED: expected 5 body items"))
    ;; Check trailing label on code segment
    (let ((trailing (second (seg-items code-seg))))
      (format t "  trailing item: ~a ~a~%"
              (type-of trailing) (ldi-name trailing))
      (assert (typep trailing 'label-def-item) ()
              "Test 5 FAILED: trailing item should be a label"))
    ;; Check data segment ref
    (let ((data-items (seg-items data-seg)))
      (format t "  data items: ~a~%" (length data-items))
      (dolist (di data-items)
        (format t "    ~a: ~a~%"
                (type-of di)
                (typecase di
                  (label-def-item (format nil "label ~a" (ldi-name di)))
                  (label-ref-item (format nil "ref ~a width=~a type=~a"
                                          (lri-label di) (lri-width di)
                                          (lri-rel-type di)))
                  (t "?"))))
      (assert (= 2 (length data-items)) ()
              "Test 5 FAILED: expected 2 data items (label + ref)"))))

(format t "~%=== All program macro tests passed ===~%")

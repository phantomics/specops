;;;; test/test-serial.lisp — Serialization primitives used without a manifest: masque and
;;;; unmasque, IEEE float bit patterns, checksums and integer sources. Pure: no files.

(in-package #:specops-tests)

(in-suite serial)

;;; ============================================================
;;; Masque and unmasque
;;; ============================================================

;; Decimal masks in masque and unmasque.
(def-test masque-and-unmasque-decimal-constant-digits-whole-groups ()
  "Decimal masques: masque writes constant digits and whole groups, unmasque reads them back,
and unmasque returns NIL when a constant digit differs."
  (is (equalp 17245
              (masque "d:1a2bb" (a 7) (b 45))))
  (is (equalp 30009
              (masque "d:aa00bb" (a 3) (b 9))))
  (is (equalp '(7 45)
              (unmasque "d:1a2bb" 17245 (a b) (list a b))))
  (is (equalp '(12 34)
              (unmasque "d:aabb" 1234 (a b) (list a b))))
  (is-false (unmasque "d:1a2bb" 97245 (a b) (list a b)))
  (is (equalp '(3 4 5)
              (unmasque "d:a.bc" 345 (a b c) (list a b c)))))

;; The :overflow parameter of masque.
(def-test overflow-mask-by-default-nil-leaving-values-error-signalling ()
  ":overflow: :mask by default, NIL leaving values, :error signalling when the code runs and at
expansion."
  (is (equalp #x2304
              (masque "h:aabb" (a #x123) (b 4))))
  (is (equalp #xFF00
              (masque "h:aabb" (a -1) (b 0))))
  (is (equalp 2304
              (masque "d:aabb" (a 123) (b 4))))
  (is (equalp 12304
              (masque "d:aabb" ((:overflow)) (a 123) (b 4))))
  (let ((x 300))
    (signals error (masque "h:aabb" ((:overflow . :error)) (a x) (b 1))))
  (is (equalp #x8001
              (let ((x -128)) (masque "h:aabb" ((:overflow . :error)) (a x) (b 1)))))
  (let ((x 100))
    (signals error (masque "d:aabb" ((:overflow . :error)) (a x) (b 1))))
  (signals error
    (macroexpand '(masque "h:aabb" ((:overflow . :error)) (a 999) (b 1)))))

;;; ============================================================
;;; Integer sources
;;; ============================================================

(def-test source-units-numbers-an-integer-s-units-as-deserializer-for-does ()
  "source-units numbers an integer's units as deserializer-for does."
  (is (equalp '(1 2 3)
              (coerce (source-units #x010203 0) 'list)))
  (is (equalp '(0 0 1 2)
              (coerce (source-units (cons 4 #x0102) 0) 'list)))
  (is (equalp '(#x0102)
              (coerce (source-units #x0102 1) 'list))))

;;; ============================================================
;;; Checksums
;;; ============================================================

;; sfnt fonts, and WOFFs made from them.
(def-test sfnt-checksum-sums-big-endian-words-the-last-padded-with-zeros ()
  "sfnt-checksum sums big-endian words, the last padded with zeros."
  (is (equalp #x00010001
              (sfnt-checksum (->bytes #(0 1 0 0 0 0 0 1 0 0)) 0 10)))
  (is (equalp 0
              (sfnt-checksum (->bytes #(#xFF #xFF #xFF #xFF 0 0 0 1)) 0 8)))
  (is (equalp #x01000000
              (sfnt-checksum (->bytes #(9 0 0 0 0 1)) 1 6))))

;;; ============================================================
;;; IEEE float bit patterns
;;; ============================================================

;; Rounding, extremes and subnormals.
(def-test rounding-to-nearest-ties-to-even-integers-rationals-and-doubles ()
  "Rounding to nearest, ties to even: integers, rationals and doubles narrowed."
  (is (equalp #x4B800000
              (encode-float-bits 16777217 32)))
  (is (equalp #x4B800002
              (encode-float-bits 16777219 32)))
  (is (equalp #x3FB999999999999A
              (encode-float-bits 1/10 64)))
  (is (equalp #x3555
              (encode-float-bits 1/3 16)))
  (is (equalp #x3DCCCCCD
              (encode-float-bits 0.1d0 32)))
  (is (equalp #x7BFF
              (encode-float-bits 65504 16))))

(def-test subnormals-the-least-of-each-format-written-and-read-back ()
  "subnormals: the least of each format, written and read back exactly."
  (is (equalp 1
              (encode-float-bits (expt 2 -24) 16)))
  (is (equalp 1
              (encode-float-bits (expt 2 -149) 32)))
  (is (equalp 1
              (encode-float-bits (expt 2 -1074) 64)))
  (is (equalp 1
              (encode-float-bits (expt 2 -16494) 128)))
  (is (equalp (expt 2 -24)
              (rational (decode-float-bits 1 16))))
  (is (equalp (expt 2 -149)
              (rational (decode-float-bits 1 32))))
  (is (equalp (expt 2 -16494)
              (decode-float-bits 1 128))))

(def-test quad-precision-1-3-rounded-to-113-bits-and-read-back ()
  "Quad precision: 1/3 rounded to 113 bits and read back as that exact rational."
  (is (equalp (list "3FFD5555555555555555555555555555" (/ #x15555555555555555555555555555 (expt 2 114)) nil)
              (let ((bits (encode-float-bits 1/3 128)))
                (list (format nil "~32,'0x" bits) (decode-float-bits bits 128) (= (decode-float-bits bits 128) 1/3))))))

;; Infinities, NaN and negative zero.
(def-test keywords-for-infinities-and-nan-written-in-every-format-and-read ()
  "Keywords for infinities and NaN, written in every format, and read back from half and quad."
  (is (equalp (list #x7C00 #x7F800000 #x7FF0000000000000 (ash #x7FFF 112))
              (mapcar (lambda (bits) (encode-float-bits :infinity bits)) '(16 32 64 128))))
  (is (equalp #xFC00
              (encode-float-bits :-infinity 16)))
  (is (equalp #x7FC00000
              (encode-float-bits :nan 32)))
  (is (equalp :infinity
              (decode-float-bits #x7C00 16)))
  (is (equalp :-infinity
              (decode-float-bits #xFC00 16)))
  (is (equalp :nan
              (decode-float-bits #x7E00 16)))
  (is (equalp :infinity
              (decode-float-bits (ash #x7FFF 112) 128))))

(def-test negative-zero-written-and-read-back-in-every-format-but-quad ()
  "Negative zero, written and read back in every format but quad."
  (is (equalp #x80000000
              (encode-float-bits (- 0.0) 32)))
  (is (equalp #x8000000000000000
              (encode-float-bits (- 0d0) 64)))
  (is (equalp #x8000
              (encode-float-bits (- 0.0) 16)))
  (is (equalp -1.0
              (float-sign (decode-float-bits #x80000000 32))))
  (is (equalp -1.0
              (float-sign (decode-float-bits #x8000 16))))
  (is (equalp 0
              (decode-float-bits (ash 1 127) 128))))

;; The portable path against SBCL's floats.
#+sbcl
(def-test sbcl-the-portable-path-agrees-with-sbcl-s-floats-on-20000-random ()
  "SBCL: the portable path agrees with SBCL's floats on 20000 random patterns of 32 and 64
bits."
  (is (equalp 0
              (let ((*random-state* (sb-ext:seed-random-state 11)) (bad 0))
                (dotimes (i 10000)
                  (dolist (bits '(32 64))
                    (let* ((p (random (ash 1 bits))) (v (portable-decode-float p bits)))
                      (unless (keywordp v)
                        (unless (and (eql v (decode-float-bits p bits)) (eql (portable-encode-float v bits) p)
                                     (eql (encode-float-bits v bits) p))
                          (incf bad))))))
                bad))))

;;;; test/helpers.lisp — Suite hierarchy, runners and shared definitions for SpecOps.
;;;;
;;;; Square-conforming (see Square.md §6, §7, §11). This file defines the whole suite
;;;; hierarchy, the two exported runner functions, and the manifests, builders and fixtures
;;;; that more than one test file draws on. It MUST load after package.lisp and before any
;;;; test-*.lisp file. Marshal and unmarshal read a manifest's definition when they expand,
;;;; so shared manifests are defined here, before any test that uses them is compiled.

(in-package #:specops-tests)

;;; ============================================================
;;; Suite hierarchy  (Square.md §6)
;;; ============================================================

(def-suite specops
  :description "Root suite for all SpecOps tests")

(def-suite serial
  :description "Serialization primitives: masque and unmasque, IEEE float bits, checksums and integer sources"
  :in specops)

(def-suite manifest
  :description "The manifest layer: defmanifest, marshal and unmarshal"
  :in specops)

(def-suite manifest-basics
  :description "Scalars, byte orders, nesting, masques, constant strings and pads"
  :in manifest)

(def-suite manifest-fields
  :description "Variable-length and typed fields: terminators, LEB128, :case, vectors, fixed buffers, user stop rules and floats"
  :in manifest)

(def-suite manifest-records
  :description "Record classes, values from records, and round trips"
  :in manifest)

(def-suite manifest-variable
  :description "Sub-manifests chosen by :case and repeated"
  :in manifest)

(def-suite manifest-reads
  :description "Checked reads, stream sources and selective reads"
  :in manifest)

(def-suite manifest-destinations
  :description "Writing to functions, streams and vectors, buffers and overruns"
  :in manifest)

(def-suite manifest-formats
  :description "PNG, WOFF and sfnt, and placed fields"
  :in manifest)

(def-suite manifest-ref
  :description "The examples of the Manifest Reference, doc/Ref.Manifest.md"
  :in manifest)

;;; ============================================================
;;; Test runners  (Square.md §11)
;;; ============================================================
;;;
;;; Both follow the canonical shape: run -> explain! -> results-status. No suite is
;;; slow, so SKIP-SLOW runs the whole hierarchy.

(defun run-all-tests (&key skip-slow)
  "Run all SpecOps tests. Return T if all pass.
SKIP-SLOW omits the *-slow suites, of which there are none at present."
  (declare (ignore skip-slow))
  (let ((results (run 'specops)))
    (explain! results)
    (results-status results)))

(defun %resolve-suite-name (name)
  "Resolve a suite NAME (keyword, string, or symbol) to the interned symbol."
  (etypecase name
    (keyword (find-symbol (symbol-name name) '#:specops-tests))
    (string  (find-symbol (string-upcase name) '#:specops-tests))
    (symbol  name)))

(defun run-suite (suite-name)
  "Run a single named suite. Accept a keyword, string, or symbol. Return T if all pass."
  (let ((results (run (%resolve-suite-name suite-name))))
    (explain! results)
    (results-status results)))

;;; ============================================================
;;; PNG: manifests, a builder and an independent verifier
;;; ============================================================

(defun ->bytes (seq)
  "Coerce SEQ into a simple (unsigned-byte 8) vector."
  (coerce seq '(simple-array (unsigned-byte 8) (*))))

(defparameter +png-signature+ (->bytes #(137 80 78 71 13 10 26 10)))

(defenum png-color-type nil :grayscale 0 :rgb 2 :palette 3 :grayscale-alpha 4 :rgba 6)

(defenum png-interlace  nil :none 0 :adam7 1)

;; IHDR body — 13 bytes.
(defmanifest png-ihdr (:unit 8 :endian :big :record t)
  (:width       (:u 4) :default 0)
  (:height      (:u 4) :default 0)
  (:bit-depth   :u8    :default 8)
  (:color-type  :u8    :default 0 :enumerate-by png-color-type)
  (:compression :u8    :default 0)
  (:filter      :u8    :default 0)
  (:interlace   :u8    :default 0 :enumerate-by png-interlace))

;; IHDR chunk: [length][type "IHDR"][IHDR body via sub-manifest][crc over type+data].
;; length measures the sub-manifest body (13); crc covers type+data.
(defmanifest png-ihdr-chunk (:unit 8 :endian :big :record t)
  (:length (:u 4) :slot (:length-of :png-ihdr))
  (span :type+data
    (str "IHDR")
    (manifest :png-ihdr png-ihdr))
  (:crc (:u 4) :default 0 :slot (:checksum-of :type+data :by #'crc)))

;; IDAT chunk: [length][type "IDAT"][opaque data][crc over type+data].
(defmanifest png-idat-chunk (:unit 8 :endian :big :record t)
  (:length (:u 4) :slot (:length-of :data))
  (span :type+data
    (str "IDAT")
    (:data (:u 1 :vec)))
  (:crc (:u 4) :default 0 :slot (:checksum-of :type+data :by #'crc)))

;; IEND chunk: [length=0][type "IEND"][empty data][crc over type].
(defmanifest png-iend-chunk (:unit 8 :endian :big :record t)
  (:length (:u 4) :slot (:length-of :data))
  (span :type+data
    (str "IEND")
    (:data (:u 1 :vec)))
  (:crc (:u 4) :default 0 :slot (:checksum-of :type+data :by #'crc)))

;; Whole file: signature + chunks (each a sub-manifest instance).
(defmanifest png-file (:unit 8 :endian :big :record t)
  (:signature (:u 1 :vec) :count 8)
  (manifest :ihdr png-ihdr-chunk)
  (manifest :idat png-idat-chunk)
  (manifest :iend png-iend-chunk))

(defun zlib-store (raw)
  "Wrap RAW bytes in a zlib stream using a single uncompressed (stored) DEFLATE
block. The trailing Adler-32 is computed with the checksum library. Returns a
simple (unsigned-byte 8) vector."
  (let* ((raw   (->bytes raw))
         (len   (length raw))
         (nlen  (logxor len #xFFFF))
         (adler (adler32-checksum raw))
         (out   (make-array (+ 2 1 2 2 len 4) :element-type '(unsigned-byte 8))))
    ;; zlib header: CM=8/CINFO=7, FCHECK
    (setf (aref out 0) #x78
          (aref out 1) #x01
          ;; DEFLATE stored block: BFINAL=1, BTYPE=00
          (aref out 2) #x01
          (aref out 3) (logand len #xFF)
          (aref out 4) (logand (ash len -8) #xFF)
          (aref out 5) (logand nlen #xFF)
          (aref out 6) (logand (ash nlen -8) #xFF))
    (loop :for i :below len :do (setf (aref out (+ 7 i)) (aref raw i)))
    (let ((p (+ 7 len)))
      (setf (aref out p)        (logand (ash adler -24) #xFF)
            (aref out (+ p 1))  (logand (ash adler -16) #xFF)
            (aref out (+ p 2))  (logand (ash adler -8)  #xFF)
            (aref out (+ p 3))  (logand adler #xFF)))
    out))

(defun make-1x1-png (&key (r 255) (g 0) (b 0))
  "Marshal a 1x1 truecolor (RGB) PNG into a byte vector and return it."
  (let ((idat (zlib-store (->bytes (vector 0 r g b))))   ; filter byte 0 + one RGB pixel
        (buf  (make-array 512 :element-type '(unsigned-byte 8) :initial-element 0)))
    (let ((end (marshal png-file buf
                 :signature +png-signature+
                 :ihdr (:png-ihdr (:width 1 :height 1 :bit-depth 8 :color-type :rgb))
                 :idat (:data idat)
                 :iend (:data #()))))
      (subseq buf 0 end))))

(defun rd-u32-be (v i)
  (logior (ash (aref v i) 24) (ash (aref v (+ i 1)) 16)
          (ash (aref v (+ i 2)) 8) (aref v (+ i 3))))

(defun verify-png (v)
  "Walk PNG byte vector V: check signature, chunk framing, per-chunk lengths and
CRC-32s, chunk order (IHDR first, IEND last, consecutive IDATs, other chunks
ancillary), IHDR fields, and IEND emptiness. Returns T if valid, and prints a report."
  (let ((errors nil) (pos 0) (chunks nil))
    (flet ((err (fmt &rest args) (push (apply #'format nil fmt args) errors)))
      ;; signature
      (if (and (>= (length v) 8) (every #'= (subseq v 0 8) +png-signature+))
          (setf pos 8)
          (err "bad signature: ~a" (subseq v 0 (min 8 (length v)))))
      ;; chunks
      (loop :while (and (null errors) (< pos (length v))) :do
        (when (> (+ pos 8) (length v))
          (err "truncated chunk header at ~a" pos) (return))
        (let* ((len        (rd-u32-be v pos))
               (type-start (+ pos 4))
               (data-start (+ type-start 4))
               (crc-start  (+ data-start len)))
          (when (> (+ crc-start 4) (length v))
            (err "chunk at ~a overruns buffer (declared len ~a)" pos len) (return))
          (let* ((type       (subseq v type-start data-start))
                 (type-str   (map 'string #'code-char type))
                 (data       (subseq v data-start crc-start))
                 (stored-crc (rd-u32-be v crc-start))
                 (calc-crc   (crc v type-start crc-start)))  ; CRC over type + data
            (unless (= stored-crc calc-crc)
              (err "~a: CRC mismatch stored ~8,'0X calc ~8,'0X"
                   type-str stored-crc calc-crc))
            (unless (= len (length data))
              (err "~a: declared length ~a /= actual ~a" type-str len (length data)))
            (push (list type-str data) chunks)
            (setf pos (+ crc-start 4)))))
      (setf chunks (nreverse chunks))
      ;; semantic checks
      (when (null errors)
        (unless (= pos (length v))
          (err "trailing ~a bytes after last chunk" (- (length v) pos)))
        (let* ((order (mapcar #'first chunks))
               (idats (loop :for type :in order :for i :from 0
                            :when (string= type "IDAT") :collect i)))
          (unless (and (string= (first order) "IHDR") (string= (car (last order)) "IEND")
                       (= 1 (count "IHDR" order :test #'string=))
                       (= 1 (count "IEND" order :test #'string=))
                       idats (= (- (car (last idats)) (first idats)) (1- (length idats)))
                       ;; any other chunk is ancillary: its type begins with a lower-case letter
                       (every (lambda (type)
                                (or (member type '("IHDR" "IDAT" "IEND") :test #'string=)
                                    (lower-case-p (char type 0))))
                              order))
            (err "chunk order ~a is not IHDR, ancillary chunks and consecutive IDATs, IEND"
                 order)))
        (let ((ihdr (second (assoc "IHDR" chunks :test #'string=))))
          (when (and ihdr (= (length ihdr) 13))
            (let ((w  (rd-u32-be ihdr 0))
                  (h  (rd-u32-be ihdr 4))
                  (bd (aref ihdr 8))
                  (ct (aref ihdr 9)))
              (unless (and (= w 1) (= h 1) (= bd 8) (= ct 2))
                (err "IHDR fields w=~a h=~a depth=~a color=~a (want 1 1 8 2)" w h bd ct))))
          (unless (and ihdr (= (length ihdr) 13))
            (err "IHDR body length ~a /= 13" (and ihdr (length ihdr)))))
        (let ((iend (second (assoc "IEND" chunks :test #'string=))))
          (unless (and iend (zerop (length iend)))
            (err "IEND data length ~a /= 0" (and iend (length iend)))))))
    (setf errors (nreverse errors))
    (if errors
        (progn (format t "~&PNG verification FAILED:~%")
               (dolist (e errors) (format t "  - ~a~%" e))
               nil)
        (progn (format t "~&PNG verification PASSED (~a bytes, chunks: ~a).~%"
                       (length v) (mapcar #'first chunks))
               t))))

;;; ============================================================
;;; Probe manifests and stop rules shared across files
;;; ============================================================

;; Mixed signedness and per-field endianness, for exercising the deserializers.
(defmanifest unmarshal-probe (:unit 8 :endian :big)
  (:a (:s 2) :default 0)                  ; signed, big-endian
  (:b (:u 2) :default 0 :endian :little)  ; unsigned, little-endian
  (:c (:s 4) :default 0 :endian :little)  ; signed, little-endian
  (:d :u8    :default 0))

(defun ub8 (n)
  (make-array n :element-type '(unsigned-byte 8) :initial-element 0))

;; A sub-manifest between two scalars, to check the shared cursor moves through it.
(defmanifest nest-probe (:unit 8 :endian :big)
  (:marker  :u8 :default 0)
  (manifest :ihdr png-ihdr)
  (:trailer :u8 :default 0))

;; Children whose byte order differs from their parent's.
(defmanifest le-child-probe (:unit 8 :endian :little)
  (:x :u16 :default 0))

;; A masque between two scalars: a static group P and a keyword group T.
(defmanifest masque-probe (:unit 8 :endian :big)
  (:lead :u8 :default 0)
  (masque "h:pptt" (p #x03) (t :kind :u8 :default 0))
  (:tail :u8 :default 0))

;; An enum group and a signed group.
(defmanifest masque-probe-typed (:unit 8 :endian :big)
  (masque "h:aabb" (a :mode  :u8 :default 0 :enumerate-by png-color-type)
                   (b :delta (:s 1) :default 0))
  (:tail :u8 :default 0))

;; Constant string with a terminator byte.
(defmanifest terminated-probe (:unit 8 :endian :big)
  (str "AB" :end-by 0)
  (:n :u8 :default 0))

;; :align and :to after a variable-length vector, so pad sizes depend on the data.
(defmanifest pad-probe (:unit 8 :endian :big)
  (:len  :u8 :default 0)
  (:data (:u 1 :vec) :count :len)
  (pad #xAA :align 4)
  (:x    :u8 :default 0)
  (pad 0 :to 12)
  (:y    :u8 :default 0))

;; Trailing :length pads the record out after a variable-length field.
(defmanifest pad-length (:unit 8 :endian :big :length 16)
  (:len  :u8 :default 0)
  (:data (:u 1 :vec) :count :len))

;; The GOFF END record layout (goff.lisp isn't loaded by any system).
(defmanifest goff-end-probe (:unit 8 :endian :big :length 80 :pad 0)
  (masque "h:pptt00" (p #x03) (t :t-tag :u8 :default 0))
  (:entry-flag   :u8  :default 0)
  (:amode        :u8  :default 0)
  (pad 0 3)
  (:record-count :u32 :default 0)
  (:entry-esdid  :u32 :default 0)
  (pad 0 4)
  (:entry-offset :u32 :default 0))

;; A NUL-terminated string: the terminator is a raw code.
(defmanifest cstring-probe (:unit 8 :endian :big)
  (:s     (:u 1 :vec) :end-by 0)
  (:after :u8 :default 0))

(defmanifest leb-probe (:unit 8 :endian :big)
  (:n     (:u 1 :leb) :default 0)
  (:after :u8 :default 0))

(defmanifest case-probe (:unit 8 :endian :big)
  (:kind  :u8 :default 0)
  (:v     (:case :kind (0 :u8) ((1 3) :u 2) (2 :s 4) (4 :u 1 :leb)) :default 0)
  (:after :u8 :default 0))

(defenum case-size nil :byte 0 :word 1 :long 2 :varint 3)

;; Keyword branch keys are translated through the selector's enum.
(defmanifest case-enum-probe (:unit 8 :endian :big)
  (:kind  :u8 :default :word :enumerate-by case-size)
  (:v     (:case :kind (:byte :u8) (:word :u16) ((:long) :s32) (:varint :u 1 :leb)) :default 0)
  (:after :u8 :default 0))

(defmanifest case-unnamed-probe (:unit 8 :endian :big)
  (:kind  :u8 :default 0)
  (_      (:case :kind (0 :u8) (1 :u32)) :default 0)
  (:after :u8 :default 0))

;; A five-byte NUL-terminated buffer, tar-style: a full buffer needs no terminator.
(defmanifest bounded-probe (:unit 8 :endian :big)
  (:name  (:u 1 :vec) :count 5 :end-by 0)
  (:after :u8 :default 0))

;; A line ended by CR LF, given as a vector of codes.
(defmanifest crlf-probe (:unit 8 :endian :big)
  (:line  (:u 1 :vec) :end-by #(13 10))
  (:after :u8 :default 0))

(defmanifest leb-vec-probe (:unit 8 :endian :big)
  (:v     (:u 1 :vec :leb) :count 3)
  (:after :u8 :default 0))

;; The element count comes from an earlier field.
(defmanifest leb-vec-counted-probe (:unit 8 :endian :big)
  (:n     :u8 :default 0)
  (:v     (:u 1 :vec :leb) :count :n)
  (:after :u8 :default 0))

;; Ended by an element whose value is 0.
(defmanifest leb-vec-terminated-probe (:unit 8 :endian :big)
  (:v     (:u 1 :vec :leb) :end-by 0)
  (:after :u8 :default 0))

;; Stop rules are instantiated when a field's code is expanded, so their classes, and any
;; effector methods, must exist at compile time.
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; QUIC's variable-length integers: the top two bits of the first byte give the length,
  ;; 1, 2, 4 or 8 bytes. Complete once that many bytes are read, so it asks done-p.
  (defclass quic-length (stop-rule)
    ((expected :initform nil) (seen :initform 0)))

  ;; The high bit marks every element but the last, which is data: :keep and frame-element.
  (defclass high-bit-run (stop-rule) ())

  ;; Two zero elements end the field; both are dropped: :drop, trailer and frame-trailer.
  (defclass double-zero (stop-rule)
    ((previous :initform nil)))

  ;; A one-element terminator given by an initarg, with only the runtime methods...
  (defclass stop-at (stop-rule)
    ((code :initarg :code :reader stop-at-code)))

  ;; ...and the same rule upgraded to inline code by effector methods.
  (defclass stop-at-inline (stop-at) ())

  (defmethod stop-effector ((r stop-at-inline) ctx)
    (list :after `(eql ,(getf ctx :element) ,(stop-at-code r)) :keep nil))

  (defmethod frame-effector ((r stop-at-inline) ctx)
    (list :trailer (list (funcall (getf ctx :write) (stop-at-code r))))))

(defmethod feed ((r quic-length) element)
  (with-slots (expected seen) r
    (incf seen)
    (unless expected (setf expected (ash 1 (ash element -6))))
    nil))

(defmethod done-p ((r quic-length))
  (with-slots (expected seen) r
    (and expected (>= seen expected))))

(defmethod feed ((r high-bit-run) element)
  (if (logbitp 7 element) nil :keep))

(defmethod frame-element ((r high-bit-run) element last-p)
  (if last-p element (logior element #x80)))

(defmethod feed ((r double-zero) element)
  (with-slots (previous) r
    (prog1 (and (eql 0 element) (eql 0 previous) :drop)
      (setf previous element))))

(defmethod trailer ((r double-zero)) 1)

(defmethod frame-trailer ((r double-zero)) (list 0 0))

(defmethod feed ((r stop-at) element)
  (and (eql element (stop-at-code r)) :drop))

(defmethod frame-trailer ((r stop-at)) (list (stop-at-code r)))

;; Two fields with the same rule: each must start with a fresh instance.
(defmanifest quic-probe (:unit 8 :endian :big)
  (:a     (:u 1 :vec) :stop-by quic-length)
  (:b     (:u 1 :vec) :stop-by quic-length)
  (:after :u8 :default 0))

(defmanifest stop-at-probe (:unit 8 :endian :big)
  (:v     (:u 1 :vec) :stop-by (stop-at :code 255))
  (:after :u8 :default 0))

(defpackage #:specops.test.rules
  (:use #:cl #:specops)
  (:export #:until-code))

(in-package #:specops.test.rules)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass until-code (stop-rule)
    ((code :initarg :code :reader code-of))))

(defmethod feed ((r until-code) element)
  (and (eql element (code-of r)) :drop))

(defmethod frame-trailer ((r until-code))
  (list (code-of r)))

(in-package #:specops-tests)

;; Chunk types as codes: "IHDR", "IDAT" and "IEND" read as big-endian 32-bit integers.
(defenum png-chunk-type nil :ihdr #x49484452 :idat #x49444154 :iend #x49454E44)

;; Any PNG chunk: IHDR's body is a sub-manifest; every other chunk's data is raw bytes,
;; whose count comes from the length slot. A type the enum lacks reads as its code.
(defmanifest png-chunk (:unit 8 :endian :big :record t)
  (:length (:u 4) :slot (:length-of :data))
  (span :type+data
    (:type (:u 4) :enumerate-by png-chunk-type)
    (:data (:case :type (:ihdr (manifest png-ihdr)) (otherwise :u 1 :vec))))
  (:crc (:u 4) :default 0 :slot (:checksum-of :type+data :by #'crc)))

;; A whole PNG: the signature, then chunks until IEND.
(defmanifest png-any (:unit 8 :endian :big :record t)
  (:signature (:u 1 :vec) :count 8)
  (manifest :chunks png-chunk :until (:type :iend)))

;; A small instance, repeated by a count field, a fixed count, or to the end of the source.
(defmanifest item-probe (:unit 8 :endian :big :record t)
  (:tag   :u8  :default 0)
  (:value :u16 :default 0))

(defmanifest counted-items-probe (:unit 8 :endian :big)
  (:n :u8 :default 0)
  (manifest :items item-probe :count :n))

(defmanifest fixed-items-probe (:unit 8 :endian :big)
  (manifest :items item-probe :count 2)
  (:after :u8 :default 0))

(defmanifest rest-items-probe (:unit 8 :endian :big)
  (:head :u8 :default 0)
  (manifest :items item-probe :until :end))

;; Instances read until a length slot's bytes are used.
(defmanifest sized-items-probe (:unit 8 :endian :big)
  (:size  :u8 :slot (:length-of :items))
  (manifest :items item-probe :until :length)
  (:after :u8 :default 0))

;; Count fields computed on write: over a repeat, and over a vector of two-byte elements.
(defmanifest count-of-items-probe (:unit 8 :endian :big :record t)
  (:n     :u8 :slot (:count-of :items))
  (manifest :items item-probe :count :n)
  (:after :u8 :default 0))

(defun png-with-ancillary-chunks ()
  "A 1x1 PNG with a tEXt chunk and its image data split across two IDAT chunks, written
by png-any from a list of chunk plists. IEND's type is given as its code."
  (let ((zlib (zlib-store (->bytes (vector 0 255 0 0))))
        (buf (ub8 105)))
    (marshal png-any buf
      :signature +png-signature+
      :chunks (list (list :type :ihdr :data (list :width 1 :height 1 :color-type :rgb))
                    (list :type #x74455874        ; tEXt, "Title" NUL "png"
                          :data (map 'vector #'char-code
                                     (concatenate 'string "Title" (string (code-char 0)) "png")))
                    (list :type :idat :data (subseq zlib 0 7))
                    (list :type :idat :data (subseq zlib 7))
                    (list :type #x49454E44 :data #())))
    buf))

;; An offset slot over a field that follows another.
(defmanifest checked-offset-probe (:unit 8 :endian :big)
  (:at   :u8  :slot (:offset-of :body))
  (:x    :u8  :default 0)
  (:body :u16 :default 0))

;;; ============================================================
;;; Readers, collectors and fixtures  (Square.md §7)
;;; ============================================================

(defun byte-sum (buf start end)
  "The low byte of the sum of BUF's elements from START to END: a checksum function."
  (ldb (byte 8 0) (loop :for i :from start :below end :sum (aref buf i))))

(defun failed-checks (thunk &optional (restart 'continue))
  "Call THUNK, answering each manifest-check-failed with RESTART, CONTINUE or USE-VALUE
with the expected value; USE-VALUE falls back to CONTINUE where it is not offered. Return
a list of (kind field manifest expected actual position use-value-p) per failure, and the
value THUNK returned."
  (let ((seen nil))
    (let ((value (handler-bind ((manifest-check-failed
                                  (lambda (c)
                                    (let ((use (find-restart 'use-value c)))
                                      (push (list (check-kind c) (check-field c) (check-manifest c)
                                                  (check-expected c) (check-actual c) (check-position c)
                                                  (and use t))
                                            seen)
                                      (if (and use (eq restart 'use-value))
                                          (use-value (check-expected c) c)
                                          (continue c))))))
                   (funcall thunk))))
      (values (nreverse seen) value))))

(defun tree-mentions-p (symbol tree)
  "True when SYMBOL appears anywhere in TREE, such as an expansion."
  (cond ((eq symbol tree) t)
        ((consp tree) (or (tree-mentions-p symbol (car tree)) (tree-mentions-p symbol (cdr tree))))))

(defun collect-units (thunk)
  "Call THUNK with a function destination, and return the units it was given and THUNK's value."
  (let ((acc nil))
    (let ((value (funcall thunk (lambda (u) (push u acc)))))
      (values (->bytes (nreverse acc)) value))))

(defun with-source-file (units function)
  "Write UNITS to a fresh temporary file, then call FUNCTION with a stream reading it, and
return its value. The file is deleted afterwards."
  (uiop:with-temporary-file (:pathname path :type "bin")
    (with-open-file (out path :direction :output :if-exists :supersede
                              :element-type '(unsigned-byte 8))
      (write-sequence units out))
    (with-open-file (in path :element-type '(unsigned-byte 8))
      (funcall function in))))

(defun reads-outside-guard (form)
  "The number of deserializer calls, (funcall (aref ...) ...), in FORM, an expansion, outside
the code that passes over the rest of a stream's record."
  (cond ((atom form) 0)
        ((and (eq 'when (first form)) (consp (second form))
              (eq 'stream-source-p (first (second form))))
         0)
        ((and (eq 'funcall (first form)) (consp (second form)) (eq 'aref (first (second form))))
         (1+ (reads-outside-guard (rest form))))
        (t (+ (reads-outside-guard (car form)) (reads-outside-guard (cdr form))))))

(defun to-list (manifest-thunk)
             (let ((acc nil))
               (funcall manifest-thunk (lambda (b) (push b acc)))
               (reverse acc)))

;;;; scripts/bench-manifests.lisp — Measurements for doc/Eval.ManifestBenchmarks.md: the cost of
;;;; writing and reading one record with marshal and unmarshal, against packing and reading it
;;;; by hand and against binary-structures
;;;;
;;; Run with: sbcl --noinform --non-interactive --load scripts/bench-manifests.lisp
;;;
;;; Needs Quicklisp, for binary-structures. Each case writes one record a million times, in each
;;; of five runs, and the fastest run is reported, with the bytes allocated per record. Every
;;; write is checked to give the same 32 bytes. The cases are direct: the code marshal
;;; generates, by the fast path into a simple vector and by the general path into a vector
;;; that is not simple or a function; a patch; a read of one field; packing by hand; and
;;; binary-structures. The estimates T1 to T4 of the evaluation, made by transforming the
;;; expansion of release 0.1.3, are in this script as of commit 6fd7863; they match only that
;;; expansion, and no longer apply.

(ql:quickload "binary-structures" :silent t)
(asdf:load-system :specops/test)

(in-package #:specops-tests)

;;; ------------------------------------------------------------
;;; The records
;;; ------------------------------------------------------------

;; The plain cell of test/test-manifest-arrays.lisp, as written in its worked records: g
;; underlined in the text's colour, the glyph, ink slot and palette slot known only when the
;; code runs.
(defmacro write-plain-cell (buf glyph ink palette)
  `(marshal plain-cell ,buf :glyph-count 2 :type-1 1 :swatch #x1234 :palette ,palette
                            :g0 (:glyph ,glyph :ink ,ink) :g1 (:glyph 9 :operation :union)))

;; The same cell as whole bytes and words, with its bit fields packed by the caller, so that
;; binary-structures, which requires octet-aligned data, and a manifest write the same thing.
(defmanifest cell-bytes (:unit 8 :endian :little)
  (:format :u8 :default 0) (:structure :u8 :default 0) (:flags :u8 :default 0)
  (:parameters :u8 :default 0) (:swatch :u16 :default 0) (:palette :u8 :default 0)
  (:header :u8 :default 0)
  (:g0 :u32 :default 0) (:g1 :u32 :default 0) (:g2 :u32 :default 0)
  (:g3 :u32 :default 0) (:g4 :u32 :default 0) (:g5 :u32 :default 0))

(org.shirakumo.binary-structures:define-io-structure bs-cell
  (format org.shirakumo.binary-structures.types:uint8)
  (structure org.shirakumo.binary-structures.types:uint8)
  (flags org.shirakumo.binary-structures.types:uint8)
  (parameters org.shirakumo.binary-structures.types:uint8)
  (swatch org.shirakumo.binary-structures.types:uint16)
  (palette org.shirakumo.binary-structures.types:uint8)
  (header org.shirakumo.binary-structures.types:uint8)
  (g0 org.shirakumo.binary-structures.types:uint32)
  (g1 org.shirakumo.binary-structures.types:uint32)
  (g2 org.shirakumo.binary-structures.types:uint32)
  (g3 org.shirakumo.binary-structures.types:uint32)
  (g4 org.shirakumo.binary-structures.types:uint32)
  (g5 org.shirakumo.binary-structures.types:uint32))

;;; ------------------------------------------------------------
;;; The cases
;;; ------------------------------------------------------------

(defun write-by-marshal (buf glyph ink palette n)
  (dotimes (i n) (write-plain-cell buf glyph ink palette)))

(defun write-constants (buf n)
  (dotimes (i n)
    (marshal plain-cell buf :glyph-count 2 :type-1 1 :swatch #x1234 :palette 7
                            :g0 (:glyph 103 :ink 1) :g1 (:glyph 9 :operation :union))))

(defun write-to-function (glyph ink palette n)
  (let ((sink (lambda (u) u)))
    (dotimes (i n)
      (marshal plain-cell sink :glyph-count 2 :type-1 1 :swatch #x1234 :palette palette
                               :g0 (:glyph glyph :ink ink) :g1 (:glyph 9 :operation :union)))))

(defun patch-cell (buf palette n)
  (dotimes (i n)
    (marshal plain-cell buf (:patch t) :blink 0 :palette palette (:g1 :reach) 0)))

(defun read-blink (buf n)
  (let ((s 0))
    (dotimes (i n) (incf s (unmarshal plain-cell buf nil :blink)))
    s))

(defun read-blink-by-hand (buf n)
  (declare (type (simple-array (unsigned-byte 8) (*)) buf) (fixnum n) (optimize speed))
  (let ((s 0))
    (declare (fixnum s))
    (dotimes (i n) (incf s (ldb (byte 1 2) (aref buf 2))))
    s))

(defun read-path (buf n)
  (let ((s 0))
    (dotimes (i n) (incf s (unmarshal plain-cell buf nil (:g1 :reach))))
    s))

(defvar *read* nil
  "The last record read, kept so that the reads are not optimized away.")

(defun read-full (buf n)
  (dotimes (i n) (setf *read* (unmarshal plain-cell buf))))

(defun read-record (buf n)
  (dotimes (i n) (setf *read* (unmarshal plain-cell buf (:as :record)))))

(defun read-into (buf n)
  (let ((cell (unmarshal plain-cell buf (:as :record))))
    (dotimes (i n) (unmarshal plain-cell buf (:into cell)))))

(defun write-by-hand (buf glyph ink palette n)
  (declare (type (simple-array (unsigned-byte 8) (*)) buf) (fixnum n glyph ink palette)
           (optimize speed))
  (dotimes (i n)
    (when (> 32 (length buf)) (error "Too small."))
    (fill buf 0 :end 32)
    (setf (aref buf 1) #x0A (aref buf 4) #x34 (aref buf 5) #x12 (aref buf 6) palette
          (aref buf 8) (ldb (byte 8 0) glyph) (aref buf 9) (ldb (byte 8 8) glyph)
          (aref buf 10) (ldb (byte 8 16) glyph) (aref buf 11) (ash ink 1)
          (aref buf 12) 9)))

(defun write-bytes-by-marshal (buf glyph ink palette n)
  (dotimes (i n)
    (marshal cell-bytes buf :structure #x0A :swatch #x1234 :palette palette
                            :g0 (logior glyph (ash (ash ink 1) 24)) :g1 9)))

(defun write-bytes-by-binary-structures (buf glyph ink palette n)
  (let ((cell (make-bs-cell :format 0 :structure #x0A :flags 0 :parameters 0 :swatch #x1234
                            :palette 0 :header 0 :g0 0 :g1 9 :g2 0 :g3 0 :g4 0 :g5 0)))
    (dotimes (i n)
      (setf (bs-cell-palette cell) palette
            (bs-cell-g0 cell) (logior glyph (ash (ash ink 1) 24)))
      (write-bs-cell cell buf))))

;;; ------------------------------------------------------------
;;; The runs
;;; ------------------------------------------------------------

(defun measure (thunk n)
  "The fastest of five runs of THUNK over N records, in ns and bytes allocated per record."
  (let ((best nil) (bytes nil))
    (dotimes (run 5)
      (let ((t0 (get-internal-real-time)) (b0 (sb-ext:get-bytes-consed)))
        (funcall thunk)
        (let ((ns (/ (* 1e9 (/ (- (get-internal-real-time) t0) internal-time-units-per-second)) n))
              (b (/ (- (sb-ext:get-bytes-consed) b0) n)))
          (when (or (null best) (< ns best)) (setf best ns bytes b)))))
    (values best bytes)))

(let* ((n 1000000)
       (expected (let ((v (ub8 32))) (write-by-marshal v 103 1 7 1) v))
       (not-simple (lambda () (make-array 32 :element-type '(unsigned-byte 8) :adjustable t)))
       (cases
         (list (list "marshal, fast path: a simple vector" (ub8 32)
                     (lambda (b) (write-by-marshal b 103 1 7 n)))
               (list "marshal, fast path, every value a constant" (ub8 32)
                     (lambda (b) (write-constants b n)))
               (list "marshal, general path: a vector not simple" (funcall not-simple)
                     (lambda (b) (write-by-marshal b 103 1 7 n)))
               (list "marshal, general path: a function" nil
                     (lambda (b) (declare (ignore b)) (write-to-function 103 1 7 n)))
               (list "marshal, a patch of three fields" (copy-seq expected)
                     (lambda (b) (patch-cell b 7 n)))
               (list "unmarshal, :blink alone" (copy-seq expected)
                     (lambda (b) (read-blink b n)))
               (list "by hand, the blink bit" (copy-seq expected)
                     (lambda (b) (read-blink-by-hand b n)))
               (list "unmarshal, the path (:g1 :reach)" (copy-seq expected)
                     (lambda (b) (read-path b n)))
               (list "unmarshal, the full cell as a plist" (copy-seq expected)
                     (lambda (b) (read-full b n)))
               (list "unmarshal, the full cell as a record" (copy-seq expected)
                     (lambda (b) (read-record b n)))
               (list "unmarshal, the full cell into a record" (copy-seq expected)
                     (lambda (b) (read-into b n)))
               (list "by hand" (ub8 32) (lambda (b) (write-by-hand b 103 1 7 n)))
               (list "marshal, the cell as bytes and words" (ub8 32)
                     (lambda (b) (write-bytes-by-marshal b 103 1 7 n)))
               (list "binary-structures, the cell as bytes and words" (ub8 32)
                     (lambda (b) (write-bytes-by-binary-structures b 103 1 7 n))))))
  (format t "~&~a, ~a~%" (lisp-implementation-type) (lisp-implementation-version))
  (dolist (c cases)
    (destructuring-bind (label buf fn) c
      (multiple-value-bind (ns bytes) (measure (lambda () (funcall fn buf)) n)
        (format t "~&~48a ~8,1f ns ~8,1f bytes ~:[(different bytes)~;~]~%"
                label ns bytes (or (null buf) (equalp (coerce buf '(vector (unsigned-byte 8))) expected)))))))

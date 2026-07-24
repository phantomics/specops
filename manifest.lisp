;;;; manifest.lisp — PNG manifest demo and 1x1 round-trip check (Parts A & B)
;;;;
;;;; Part A: PNG format expressed with defmanifest (signature + IHDR/IDAT/IEND
;;;;         chunks, with length-prefixing and CRC-32 checksums, IHDR body as a
;;;;         sub-manifest).
;;;; Part B: marshal a 1x1 RGB PNG into a byte vector and structurally verify it
;;;;         (signature, per-chunk framing, lengths, CRCs, field values).

(in-package #:specops)

;;; ===========================================================================
;;; Constants
;;; ===========================================================================

(defun ->bytes (seq)
  "Coerce SEQ into a simple (unsigned-byte 8) vector."
  (coerce seq '(simple-array (unsigned-byte 8) (*))))

(defparameter +png-signature+ (->bytes #(137 80 78 71 13 10 26 10)))
(defparameter +png-type-ihdr+ (->bytes #(73 72 68 82)))  ; "IHDR"
(defparameter +png-type-idat+ (->bytes #(73 68 65 84)))  ; "IDAT"
(defparameter +png-type-iend+ (->bytes #(73 69 78 68)))  ; "IEND"

;;; ===========================================================================
;;; Part A — PNG manifests
;;; ===========================================================================

(defenum png-color-type nil :grayscale 0 :rgb 2 :palette 3 :grayscale-alpha 4 :rgba 6)
(defenum png-interlace  nil :none 0 :adam7 1)

;; IHDR body — 13 bytes.
(defmanifest png-ihdr (:unit 8 :endian :big)
  (:width       (:u 4) :default 0)
  (:height      (:u 4) :default 0)
  (:bit-depth   :u8    :default 8)
  (:color-type  :u8    :default 0 :enumerate-by png-color-type)
  (:compression :u8    :default 0)
  (:filter      :u8    :default 0)
  (:interlace   :u8    :default 0 :enumerate-by png-interlace))

;; IHDR chunk: [length][type "IHDR"][IHDR body via sub-manifest][crc over type+data].
;; length measures the sub-manifest body (13); crc covers type+data.
(defmanifest png-ihdr-chunk (:unit 8 :endian :big)
  (:length (:u 4) :slot (:length-of :png-ihdr))
  (span :type+data
    (:type (:u 1 :vec))
    (manifest :png-ihdr png-ihdr))
  (:crc (:u 4) :default 0 :slot (:checksum-of :type+data :by #'crc)))

;; IDAT chunk: [length][type "IDAT"][opaque data][crc over type+data].
(defmanifest png-idat-chunk (:unit 8 :endian :big)
  (:length (:u 4) :slot (:length-of :data))
  (span :type+data
    (:type (:u 1 :vec))
    (:data (:u 1 :vec)))
  (:crc (:u 4) :default 0 :slot (:checksum-of :type+data :by #'crc)))

;; IEND chunk: [length=0][type "IEND"][empty data][crc over type].
(defmanifest png-iend-chunk (:unit 8 :endian :big)
  (:length (:u 4) :slot (:length-of :data))
  (span :type+data
    (:type (:u 1 :vec))
    (:data (:u 1 :vec)))
  (:crc (:u 4) :default 0 :slot (:checksum-of :type+data :by #'crc)))

;; Whole file: signature + chunks (each a sub-manifest instance).
(defmanifest png-file (:unit 8 :endian :big)
  (:signature (:u 1 :vec))
  (manifest :ihdr png-ihdr-chunk)
  (manifest :idat png-idat-chunk)
  (manifest :iend png-iend-chunk))

;;; ===========================================================================
;;; Part B — build a 1x1 PNG and verify it
;;; ===========================================================================

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
                 :ihdr (:type +png-type-ihdr+
                        :png-ihdr (:width 1 :height 1 :bit-depth 8 :color-type :rgb))
                 :idat (:type +png-type-idat+ :data idat)
                 :iend (:type +png-type-iend+ :data #()))))
      (subseq buf 0 end))))

;;; --- structural verifier (a hand reader; prototypes unmarshal) --------------

(defun rd-u32-be (v i)
  (logior (ash (aref v i) 24) (ash (aref v (+ i 1)) 16)
          (ash (aref v (+ i 2)) 8) (aref v (+ i 3))))

(defun verify-png (v)
  "Walk PNG byte vector V: check signature, chunk framing, per-chunk lengths and
CRC-32s, chunk order, IHDR fields, and IEND emptiness. Returns T if valid, and
prints a report."
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
        (let ((order (mapcar #'first chunks)))
          (unless (equal order '("IHDR" "IDAT" "IEND"))
            (err "chunk order ~a /= (IHDR IDAT IEND)" order)))
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

(defun run-png-demo ()
  "Build a 1x1 PNG and verify it. Returns the byte vector."
  (let ((png (make-1x1-png)))
    (format t "~&Marshalled 1x1 PNG, ~a bytes:~% ~a~%" (length png) png)
    (verify-png png)
    png))

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

;;; ===========================================================================
;;; Part A — PNG manifests
;;; ===========================================================================

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
                 :ihdr (:png-ihdr (:width 1 :height 1 :bit-depth 8 :color-type :rgb))
                 :idat (:data idat)
                 :iend (:data #()))))
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

;;; ===========================================================================
;;; Part D — unmarshal checks (flat, fixed-width manifests)
;;; ===========================================================================

;; Mixed signedness and per-field endianness, for exercising the deserializers.
(defmanifest unmarshal-probe (:unit 8 :endian :big)
  (:a (:s 2) :default 0)                  ; signed, big-endian
  (:b (:u 2) :default 0 :endian :little)  ; unsigned, little-endian
  (:c (:s 4) :default 0 :endian :little)  ; signed, little-endian
  (:d :u8    :default 0))                 ; trailing byte, checks the cursor advanced

;; The manifest's byte order applies to every field that gives none of its own.
(defmanifest little-endian-probe (:unit 8 :endian :little)
  (:a :u16 :default 0)                    ; little-endian, from the manifest
  (:b (:s 4) :default 0)                  ; little-endian, from the manifest
  (:c :u16 :default 0 :endian :big))      ; the field's own byte order wins

;; An enumerated field whose default is a keyword. png-color-type has no code 1.
(defmanifest enum-default-probe (:unit 8 :endian :big)
  (:color :u8 :default :rgba :enumerate-by png-color-type))

(defun ub8 (n)
  (make-array n :element-type '(unsigned-byte 8) :initial-element 0))

(defun run-unmarshal-demo ()
  "Exercise unmarshal on flat manifests. Each check compares the values returned
by unmarshal with the values that were marshalled. Returns T if all pass."
  (let ((results nil))
    (flet ((check (label got want)
             (let ((ok (equalp got want)))
               (push ok results)
               (format t "~&  ~:[FAIL~;ok  ~]  ~a~%" ok label)
               (unless ok
                 (format t "~&          got  ~s~%          want ~s~%" got want)))))
      (format t "~&unmarshal checks:~%")

      ;; 1. Round trip a png-ihdr body with distinct field values.
      (let ((buf (ub8 13)))
        (marshal png-ihdr buf :width 640 :height 480 :bit-depth 8
                              :color-type :rgba :interlace :adam7)
        (check "png-ihdr, all fields in manifest order (enums decoded)"
               (unmarshal png-ihdr buf)
               '(:width 640 :height 480 :bit-depth 8 :color-type :rgba
                 :compression 0 :filter 0 :interlace :adam7))
        (check "png-ihdr, :height alone (skips :width)"
               (multiple-value-list (unmarshal png-ihdr buf nil :height))
               '(480))
        (check "png-ihdr, :interlace :width (out of order, returned in call order)"
               (multiple-value-list (unmarshal png-ihdr buf nil :interlace :width))
               '(:adam7 640))
        (check "png-ihdr, :color-type alone (enum decode mid-record)"
               (multiple-value-list (unmarshal png-ihdr buf nil :color-type))
               '(:rgba)))

      ;; 2. Read the IHDR body out of the demo PNG (bytes 16..28).
      (let ((ihdr (subseq (make-1x1-png) 16 29)))
        (check "IHDR body from the marshalled 1x1 PNG"
               (multiple-value-list (unmarshal png-ihdr ihdr nil :width :height :color-type))
               '(1 1 :rgb)))

      ;; 3. Signed values and per-field endianness.
      (let ((buf (ub8 9)))
        (marshal unmarshal-probe buf :a -2 :b #x0102 :c -300 :d 7)
        (check "marshalled bytes for unmarshal-probe"
               (coerce buf 'list)
               '(#xFF #xFE  #x02 #x01  #xD4 #xFE #xFF #xFF  7))
        (check "unmarshal-probe, all fields (signed BE, unsigned LE, signed LE)"
               (unmarshal unmarshal-probe buf)
               '(:a -2 :b #x0102 :c -300 :d 7))
        (check "unmarshal-probe, :d alone (cursor passes three fields)"
               (multiple-value-list (unmarshal unmarshal-probe buf nil :d))
               '(7)))

      ;; 4. A byte order given once, by the manifest.
      (let ((buf (ub8 8)))
        (marshal little-endian-probe buf :a #x0102 :b -300 :c #x0304)
        (check "little-endian-probe, fields follow the manifest's :endian unless they give one"
               (coerce buf 'list)
               '(#x02 #x01  #xD4 #xFE #xFF #xFF  #x03 #x04))
        (check "little-endian-probe, read back"
               (unmarshal little-endian-probe buf)
               '(:a #x0102 :b -300 :c #x0304)))

      ;; 5. Enums: a keyword default, and a code the enum lacks.
      (let ((buf (ub8 1)))
        (marshal enum-default-probe buf)
        (check "enum-default-probe, a keyword default written as its code"
               (list (coerce buf 'list) (unmarshal enum-default-probe buf))
               '((6) (:color :rgba))))
      (let ((buf (ub8 1)))
        (marshal enum-default-probe buf :color 1)
        (check "enum-default-probe, a code the enum lacks reads as itself and writes back"
               (list (unmarshal enum-default-probe (->bytes #(1))) (coerce buf 'list))
               '((:color 1) (1)))))
    (let ((pass (every #'identity results)))
      (format t "~&unmarshal checks ~:[FAILED~;PASSED~] (~a of ~a).~%"
              pass (count t results) (length results))
      pass)))

;;; ===========================================================================
;;; Part E — unmarshal checks for chunks: spans, vectors, sub-manifests
;;; ===========================================================================

;; A sub-manifest between two scalars, to check the shared cursor moves through it.
(defmanifest nest-probe (:unit 8 :endian :big)
  (:marker  :u8 :default 0)
  (manifest :ihdr png-ihdr)
  (:trailer :u8 :default 0))

;; One more level of nesting, to check the sub-lexicon is passed down.
(defmanifest nest-probe2 (:unit 8 :endian :big)
  (:head :u8 :default 0)
  (manifest :inner nest-probe))

;; Children whose byte order differs from their parent's.
(defmanifest le-child-probe (:unit 8 :endian :little)
  (:x :u16 :default 0))

(defmanifest be-child-probe (:unit 8 :endian :big)
  (:x :u16 :default 0))

(defmanifest mixed-order-probe (:unit 8 :endian :big)
  (:a :u16 :default 0)
  (manifest :le le-child-probe)
  (:b :u16 :default 0))

(defmanifest mixed-order-probe2 (:unit 8 :endian :little)
  (:a :u16 :default 0)
  (manifest :be be-child-probe))

(defun run-chunk-unmarshal-demo ()
  "Unmarshal PNG chunks out of the marshalled 1x1 PNG, plus nested probes.
Returns T if all checks pass."
  (let* ((results nil)
         (png  (make-1x1-png))
         (blob (zlib-store (->bytes (vector 0 255 0 0))))
         (ihdr-chunk (subseq png 8 33))    ; 4 len + 4 type + 13 body + 4 crc
         (idat-chunk (subseq png 33 60))   ; 4 len + 4 type + 15 data + 4 crc
         (iend-chunk (subseq png 60 72)))  ; 4 len + 4 type + 0 data + 4 crc
    (flet ((check (label got want)
             (let ((ok (equalp got want)))
               (push ok results)
               (format t "~&  ~:[FAIL~;ok  ~]  ~a~%" ok label)
               (unless ok
                 (format t "~&          got  ~s~%          want ~s~%" got want)))))
      (format t "~&chunk unmarshal checks:~%")

      ;; 1. IDAT: span, constant "IDAT" tag, :data counted by the :length field.
      (check "IDAT, all fields"
             (unmarshal png-idat-chunk idat-chunk)
             (list :length 15 :data blob
                   :crc (rd-u32-be idat-chunk 23)))
      (check "IDAT, :data alone (count taken from unrequested :length)"
             (unmarshal png-idat-chunk idat-chunk nil :data)
             blob)
      (check "IDAT, :data :length (out of order)"
             (multiple-value-list (unmarshal png-idat-chunk idat-chunk nil :data :length))
             (list blob 15))
      (check "IDAT, stored :crc matches recomputed CRC over type+data"
             (unmarshal png-idat-chunk idat-chunk nil :crc)
             (crc idat-chunk 4 23))

      ;; 2. IEND: zero-length :data.
      (check "IEND, all fields (empty :data)"
             (unmarshal png-iend-chunk iend-chunk)
             (list :length 0 :data #()
                   :crc (rd-u32-be iend-chunk 8)))

      ;; 3. Sub-manifest between scalars.
      (let ((buf (ub8 15)))
        (marshal nest-probe buf :marker 238
                                :ihdr (:width 640 :height 480 :bit-depth 8
                                       :color-type :rgba :interlace :adam7)
                                :trailer 170)
        (check "nest-probe, all fields (nested plist)"
               (unmarshal nest-probe buf)
               '(:marker 238
                 :ihdr (:width 640 :height 480 :bit-depth 8 :color-type :rgba
                        :compression 0 :filter 0 :interlace :adam7)
                 :trailer 170))
        (check "nest-probe, :trailer alone (cursor passes the sub-manifest)"
               (unmarshal nest-probe buf nil :trailer)
               170))

      ;; 4. Two levels of nesting.
      (let ((buf (ub8 16)))
        (marshal nest-probe2 buf :head 9
                                 :inner (:marker 238
                                         :ihdr (:width 1 :height 2 :color-type :rgb)
                                         :trailer 170))
        (check "nest-probe2, two levels"
               (unmarshal nest-probe2 buf)
               '(:head 9
                 :inner (:marker 238
                         :ihdr (:width 1 :height 2 :bit-depth 8 :color-type :rgb
                                :compression 0 :filter 0 :interlace :none)
                         :trailer 170))))

      ;; 5. Children with a byte order of their own.
      (let ((buf (ub8 6)))
        (marshal mixed-order-probe buf :a #x0102 :le (:x #x0304) :b #x0506)
        (check "mixed-order-probe, a little-endian child inside a big-endian parent"
               (coerce buf 'list) '(#x01 #x02 #x04 #x03 #x05 #x06))
        (check "mixed-order-probe, read back"
               (unmarshal mixed-order-probe buf)
               '(:a #x0102 :le (:x #x0304) :b #x0506)))
      (let ((buf (ub8 4)))
        (marshal mixed-order-probe2 buf :a #x0102 :be (:x #x0304))
        (check "mixed-order-probe2, a big-endian child inside a little-endian parent"
               (coerce buf 'list) '(#x02 #x01 #x03 #x04))
        (check "mixed-order-probe2, read back"
               (unmarshal mixed-order-probe2 buf)
               '(:a #x0102 :be (:x #x0304))))

      ;; 6. IHDR chunk: span containing a sub-manifest.
      (check "IHDR chunk, all fields (span + sub-manifest)"
             (unmarshal png-ihdr-chunk ihdr-chunk)
             (list :length 13
                   :png-ihdr '(:width 1 :height 1 :bit-depth 8 :color-type :rgb
                               :compression 0 :filter 0 :interlace :none)
                   :crc (rd-u32-be ihdr-chunk 21))))
    (let ((pass (every #'identity results)))
      (format t "~&chunk unmarshal checks ~:[FAILED~;PASSED~] (~a of ~a).~%"
              pass (count t results) (length results))
      pass)))

;;; ===========================================================================
;;; Part F — masque fields: marshal and unmarshal
;;; ===========================================================================

;; A masque between two scalars: a static group P and a keyword group T.
(defmanifest masque-probe (:unit 8 :endian :big)
  (:lead :u8 :default 0)
  (masque "h:pptt" (p #x03) (t :kind :u8 :default 0))
  (:tail :u8 :default 0))

;; Static group of zero, so the masque's leading byte is 0.
(defmanifest masque-probe-zero (:unit 8 :endian :big)
  (:lead :u8 :default 0)
  (masque "h:pptt" (p #x00) (t :kind :u8 :default 0))
  (:tail :u8 :default 0))

;; An enum group and a signed group.
(defmanifest masque-probe-typed (:unit 8 :endian :big)
  (masque "h:aabb" (a :mode  :u8 :default 0 :enumerate-by png-color-type)
                   (b :delta (:s 1) :default 0))
  (:tail :u8 :default 0))

;; An enumerated masque group whose default is a keyword.
(defmanifest masque-default-probe (:unit 8 :endian :big)
  (masque "h:aabb" (a :mode  :u8 :default :rgb :enumerate-by png-color-type)
                   (b :delta (:s 1) :default 0))
  (:tail :u8 :default 0))

(defun run-masque-demo ()
  "Marshal and unmarshal masque fields. Returns T if all checks pass."
  (let ((results nil))
    (flet ((check (label got want)
             (let ((ok (equalp got want)))
               (push ok results)
               (format t "~&  ~:[FAIL~;ok  ~]  ~a~%" ok label)
               (unless ok
                 (format t "~&          got  ~s~%          want ~s~%" got want)))))
      (format t "~&masque checks:~%")

      ;; 1. Static and keyword groups.
      (let ((buf (ub8 4)))
        (marshal masque-probe buf :lead 1 :kind 5 :tail 9)
        (check "masque-probe, marshalled bytes"
               (coerce buf 'list) '(1 3 5 9))
        (check "masque-probe, all fields"
               (unmarshal masque-probe buf)
               '(:lead 1 :kind 5 :tail 9))
        (check "masque-probe, :tail alone (cursor passes the masque)"
               (unmarshal masque-probe buf nil :tail)
               9)
        (check "masque-probe, :kind alone"
               (unmarshal masque-probe buf nil :kind)
               5)
        (let ((bad (copy-seq buf)))
          (setf (aref bad 1) #x04)
          (check "masque-probe, wrong static group signals an error"
                 (handler-case (progn (unmarshal masque-probe bad) :no-error)
                   (error () :error))
                 :error)))

      ;; 2. Keyword group omitted: its default is written.
      (let ((buf (ub8 4)))
        (marshal masque-probe buf :lead 1 :tail 9)
        (check "masque-probe, omitted :kind uses its default"
               (coerce buf 'list) '(1 3 0 9)))

      ;; 3. Leading zero byte inside the masque.
      (let ((buf (make-array 4 :element-type '(unsigned-byte 8) :initial-element #xEE)))
        (marshal masque-probe-zero buf :lead 1 :kind 0 :tail 9)
        (check "masque-probe-zero, marshalled bytes (leading zero kept)"
               (coerce buf 'list) '(1 0 0 9))
        (check "masque-probe-zero, all fields"
               (unmarshal masque-probe-zero buf)
               '(:lead 1 :kind 0 :tail 9)))

      ;; 4. Enum and signed groups.
      (let ((buf (ub8 3)))
        (marshal masque-probe-typed buf :mode :rgba :delta -3 :tail 7)
        (check "masque-probe-typed, marshalled bytes (enum 6, -3 as #xFD)"
               (coerce buf 'list) '(6 #xFD 7))
        (check "masque-probe-typed, all fields (enum decoded, sign extended)"
               (unmarshal masque-probe-typed buf)
               '(:mode :rgba :delta -3 :tail 7)))

      ;; 5. A keyword default in a masque group.
      (let ((buf (ub8 3)))
        (marshal masque-default-probe buf :tail 7)
        (check "masque-default-probe, the default :rgb written as 2, and read back"
               (list (coerce buf 'list) (unmarshal masque-default-probe buf))
               '((2 0 7) (:mode :rgb :delta 0 :tail 7)))))
    (let ((pass (every #'identity results)))
      (format t "~&masque checks ~:[FAILED~;PASSED~] (~a of ~a).~%"
              pass (count t results) (length results))
      pass)))

;;; ===========================================================================
;;; Part G — constant strings: chunk tags, codecs, terminators
;;; ===========================================================================

;; Constant string encoded in EBCDIC (code page 037).
(defmanifest ebcdic-probe (:unit 8 :endian :big)
  (str "HELLO" :codec #'specops.map.ebcdic:ebcdic-code-cp037)
  (:n :u8 :default 0))

;; Constant string with a terminator byte.
(defmanifest terminated-probe (:unit 8 :endian :big)
  (str "AB" :end-by 0)
  (:n :u8 :default 0))

;; A constant string in a manifest whose fields are all little-endian.
(defmanifest le-string-probe (:unit 8 :endian :little)
  (str "AB")
  (:x :u16 :default 0))

(defun signals-error-p (thunk)
  (handler-case (progn (funcall thunk) nil)
    (error () t)))

(defun run-string-demo ()
  "Marshal and unmarshal constant strings. Returns T if all checks pass."
  (let ((results nil))
    (flet ((check (label got want)
             (let ((ok (equalp got want)))
               (push ok results)
               (format t "~&  ~:[FAIL~;ok  ~]  ~a~%" ok label)
               (unless ok
                 (format t "~&          got  ~s~%          want ~s~%" got want)))))
      (format t "~&string checks:~%")

      ;; 1. Chunk tags in the marshalled PNG.
      (let* ((png  (make-1x1-png))
             (idat (subseq png 33 60)))
        (check "IHDR/IDAT/IEND tags written by constant str fields"
               (list (map 'string #'code-char (subseq png 12 16))
                     (map 'string #'code-char (subseq png 37 41))
                     (map 'string #'code-char (subseq png 64 68)))
               '("IHDR" "IDAT" "IEND"))
        (let ((bad (copy-seq idat)))
          (setf (aref bad 7) (char-code #\X))           ; "IDAT" -> "IDAX"
          (check "IDAT with a corrupted tag signals an error"
                 (signals-error-p (lambda () (unmarshal png-idat-chunk bad)))
                 t))
        (check "png-file, whole file read back"
               (unmarshal png-file png)
               (list :signature +png-signature+
                     :ihdr (list :length 13
                                 :png-ihdr '(:width 1 :height 1 :bit-depth 8 :color-type :rgb
                                             :compression 0 :filter 0 :interlace :none)
                                 :crc (rd-u32-be png 29))
                     :idat (list :length 15 :data (subseq png 41 56) :crc (rd-u32-be png 56))
                     :iend (list :length 0 :data #() :crc (rd-u32-be png 68)))))

      ;; 2. EBCDIC codec.
      (let ((buf (ub8 6)))
        (marshal ebcdic-probe buf :n 7)
        (check "ebcdic-probe, \"HELLO\" written in code page 037"
               (coerce buf 'list) '(#xC8 #xC5 #xD3 #xD3 #xD6 7))
        (check "ebcdic-probe, read back (string validated)"
               (unmarshal ebcdic-probe buf)
               '(:n 7))
        (let ((bad (copy-seq buf)))
          (setf (aref bad 0) (char-code #\H))           ; ASCII H, not EBCDIC
          (check "ebcdic-probe, ASCII byte in place of EBCDIC signals an error"
                 (signals-error-p (lambda () (unmarshal ebcdic-probe bad)))
                 t)))

      ;; 3. Terminator.
      (let ((buf (ub8 4)))
        (marshal terminated-probe buf :n 9)
        (check "terminated-probe, bytes include the terminator"
               (coerce buf 'list) '(#x41 #x42 0 9))
        (check "terminated-probe, read back (cursor passes the terminator)"
               (unmarshal terminated-probe buf)
               '(:n 9))
        (let ((bad (copy-seq buf)))
          (setf (aref bad 2) 1)
          (check "terminated-probe, missing terminator signals an error"
                 (signals-error-p (lambda () (unmarshal terminated-probe bad)))
                 t)))

      ;; 4. A string where no field is big-endian.
      (let ((buf (ub8 4)))
        (marshal le-string-probe buf :x #x0102)
        (check "le-string-probe, the string in stream order, the field little-endian"
               (coerce buf 'list) '(#x41 #x42 #x02 #x01))
        (check "le-string-probe, read back"
               (unmarshal le-string-probe buf)
               '(:x #x0102))))
    (let ((pass (every #'identity results)))
      (format t "~&string checks ~:[FAILED~;PASSED~] (~a of ~a).~%"
              pass (count t results) (length results))
      pass)))

;;; ===========================================================================
;;; Part H — padding: count, :to (record-relative), :align (absolute), :length
;;; ===========================================================================

;; :align and :to after a variable-length vector, so pad sizes depend on the data.
(defmanifest pad-probe (:unit 8 :endian :big)
  (:len  :u8 :default 0)
  (:data (:u 1 :vec) :count :len)
  (pad #xAA :align 4)
  (:x    :u8 :default 0)
  (pad 0 :to 12)
  (:y    :u8 :default 0))

;; :to inside a sub-manifest counts from the child's start.
(defmanifest pad-inner (:unit 8 :endian :big)
  (:a :u8 :default 0)
  (pad 0 :to 6)
  (:b :u8 :default 0))

(defmanifest pad-outer (:unit 8 :endian :big)
  (:h1 :u8 :default 0)
  (:h2 :u8 :default 0)
  (manifest :inner pad-inner)
  (:tail :u8 :default 0))

;; Trailing :length pads the record out after a variable-length field.
(defmanifest pad-length (:unit 8 :endian :big :length 16)
  (:len  :u8 :default 0)
  (:data (:u 1 :vec) :count :len))

(defmanifest pad-length-outer (:unit 8 :endian :big)
  (manifest :rec pad-length)
  (:after :u8 :default 0))

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

(defun run-pad-demo ()
  "Marshal and unmarshal pads. Returns T if all checks pass."
  (let ((results nil))
    (flet ((check (label got want)
             (let ((ok (equalp got want)))
               (push ok results)
               (format t "~&  ~:[FAIL~;ok  ~]  ~a~%" ok label)
               (unless ok
                 (format t "~&          got  ~s~%          want ~s~%" got want)))))
      (format t "~&pad checks:~%")

      ;; 1. :align and :to depend on the length of the preceding vector.
      (let ((buf (ub8 13)))
        (marshal pad-probe buf :len 1 :data #(7) :x #x11 :y #x22)
        (check "pad-probe, 1-byte data: 2 align bytes, :to 12 fills 7"
               (coerce buf 'list) '(1 7 #xAA #xAA #x11 0 0 0 0 0 0 0 #x22))
        (check "pad-probe, 1-byte data, read back"
               (unmarshal pad-probe buf)
               '(:len 1 :data #(7) :x #x11 :y #x22)))
      (let ((buf (ub8 13)))
        (marshal pad-probe buf :len 3 :data #(7 8 9) :x #x11 :y #x22)
        (check "pad-probe, 3-byte data: already aligned, no align bytes"
               (coerce buf 'list) '(3 7 8 9 #x11 0 0 0 0 0 0 0 #x22))
        (check "pad-probe, 3-byte data, read back"
               (unmarshal pad-probe buf)
               '(:len 3 :data #(7 8 9) :x #x11 :y #x22))
        (check "pad-probe, :y alone (cursor passes both pads)"
               (unmarshal pad-probe buf nil :y)
               #x22))

      ;; 2. :to past its target signals an error in both directions.
      (check "pad-probe, :to overrun on marshal signals an error"
             (signals-error-p
              (lambda () (marshal pad-probe (ub8 32) :len 9 :data #(1 2 3 4 5 6 7 8 9)
                                                     :x 1 :y 2)))
             t)
      (let ((bad (ub8 16)))
        (replace bad '(9 1 2 3 4 5 6 7 8 9 #xAA #xAA #x11))
        (check "pad-probe, :to overrun on unmarshal signals an error"
               (signals-error-p (lambda () (unmarshal pad-probe bad)))
               t))

      ;; 3. :to inside a sub-manifest is relative to the child's start.
      (let ((buf (ub8 10)))
        (marshal pad-outer buf :h1 1 :h2 2 :inner (:a 3 :b 4) :tail 5)
        (check "pad-outer, child :to 6 counts from the child (byte 2)"
               (coerce buf 'list) '(1 2 3 0 0 0 0 0 4 5))
        (check "pad-outer, read back"
               (unmarshal pad-outer buf)
               '(:h1 1 :h2 2 :inner (:a 3 :b 4) :tail 5)))

      ;; 4. Trailing :length.
      (let* ((buf (make-array 20 :element-type '(unsigned-byte 8) :initial-element #xEE))
             (end (marshal pad-length buf :len 3 :data #(1 2 3))))
        (check "pad-length, marshal stops at :length 16"
               end 16)
        (check "pad-length, bytes padded to 16, rest of buffer untouched"
               (coerce buf 'list)
               '(3 1 2 3 0 0 0 0 0 0 0 0 0 0 0 0 #xEE #xEE #xEE #xEE)))
      (let ((buf (ub8 17)))
        (marshal pad-length-outer buf :rec (:len 3 :data #(1 2 3)) :after 9)
        (check "pad-length-outer, field after a padded sub-manifest lands at byte 16"
               (aref buf 16) 9)
        (check "pad-length-outer, read back"
               (unmarshal pad-length-outer buf)
               '(:rec (:len 3 :data #(1 2 3)) :after 9)))

      ;; 5. GOFF END record layout.
      (let* ((buf (make-array 96 :element-type '(unsigned-byte 8) :initial-element #xEE))
             (end (marshal goff-end-probe buf :t-tag #x40 :entry-flag 1 :amode 2
                                              :record-count 7 :entry-esdid 5
                                              :entry-offset #x100)))
        (check "goff-end-probe, record is 80 bytes"
               end 80)
        (check "goff-end-probe, first 24 bytes"
               (coerce (subseq buf 0 24) 'list)
               '(#x03 #x40 0  1 2  0 0 0  0 0 0 7  0 0 0 5  0 0 0 0  0 0 1 0))
        (check "goff-end-probe, bytes 24..79 are zero"
               (every #'zerop (subseq buf 24 80)) t)
        (check "goff-end-probe, read back"
               (unmarshal goff-end-probe buf)
               '(:t-tag #x40 :entry-flag 1 :amode 2 :record-count 7
                 :entry-esdid 5 :entry-offset #x100)))

      ;; 6. A function destination produces the same bytes as a vector.
      (let ((acc nil))
        (marshal pad-probe (lambda (b) (push b acc)) :len 1 :data #(7) :x #x11 :y #x22)
        (check "pad-probe to a function destination matches the vector bytes"
               (reverse acc) '(1 7 #xAA #xAA #x11 0 0 0 0 0 0 0 #x22))))
    (let ((pass (every #'identity results)))
      (format t "~&pad checks ~:[FAILED~;PASSED~] (~a of ~a).~%"
              pass (count t results) (length results))
      pass)))

;;; ===========================================================================
;;; Part I — codecs on :vec fields, and terminated fields (:end-by)
;;; ===========================================================================

;; A fixed-length text field encoded in EBCDIC, with a string default.
(defmanifest codec-probe (:unit 8 :endian :big)
  (:num  :u16 :default 0)
  (:text (:u 1 :vec) :count 5 :default "     "
         :codec #'specops.map.ebcdic:ebcdic-code-cp037))

;; A numeric default is already a code, so it is not passed through the codec.
(defmanifest codec-default-probe (:unit 8 :endian :big)
  (:fill (:u 1 :vec) :count 3 :default 64
         :codec #'specops.map.ebcdic:ebcdic-code-cp037))

;; A constant string with a character the codec does not map (a Tab).
(defmanifest codec-str-probe (:unit 8 :endian :big)
  (str #.(coerce (list #\A #\Tab) 'string)
       :codec #'specops.map.ebcdic:ebcdic-code-cp037))

;; A NUL-terminated string: the terminator is a raw code.
(defmanifest cstring-probe (:unit 8 :endian :big)
  (:s     (:u 1 :vec) :end-by 0)
  (:after :u8 :default 0))

;; A character terminator is encoded with the field's codec ($ is #x5B in CP037).
(defmanifest ebcdic-terminated-probe (:unit 8 :endian :big)
  (:s     (:u 1 :vec) :end-by #\$ :codec #'specops.map.ebcdic:ebcdic-code-cp037)
  (:after :u8 :default 0))

;; Two-unit little-endian elements: the terminator is one element, two bytes.
(defmanifest wide-terminated-probe (:unit 8 :endian :big)
  (:s     (:u 2 :vec) :endian :little :end-by 0)
  (:after :u8 :default 0))

;; A length slot over a span that holds a terminated field counts the terminator.
(defmanifest terminated-span-probe (:unit 8 :endian :big)
  (:len :u8 :slot (:length-of :body))
  (span :body
    (:s (:u 1 :vec) :end-by 0)
    (:k :u8 :default 0)))

(defun run-terminated-demo ()
  "Marshal and unmarshal codec-encoded vectors and terminated fields.
Returns T if all checks pass."
  (let ((results nil))
    (flet ((check (label got want)
             (let ((ok (equalp got want)))
               (push ok results)
               (format t "~&  ~:[FAIL~;ok  ~]  ~a~%" ok label)
               (unless ok
                 (format t "~&          got  ~s~%          want ~s~%" got want))))
           (to-list (manifest-thunk)
             (let ((acc nil))
               (funcall manifest-thunk (lambda (b) (push b acc)))
               (reverse acc))))
      (format t "~&codec and terminator checks:~%")

      ;; 1. Codecs on :vec fields.
      (check "codec-probe, \"Hello\" encoded in CP037"
             (to-list (lambda (d) (marshal codec-probe d :num 50 :text "Hello")))
             '(0 50 #xC8 #x85 #x93 #x93 #x96))
      (check "codec-probe, string default encoded (EBCDIC spaces)"
             (to-list (lambda (d) (marshal codec-probe d :num 50)))
             '(0 50 #x40 #x40 #x40 #x40 #x40))
      (check "codec-probe, read back and decoded to a string"
             (unmarshal codec-probe (->bytes #(0 50 #xC8 #x85 #x93 #x93 #x96)))
             '(:num 50 :text "Hello"))
      (check "codec-default-probe, numeric default written as is, :count times"
             (to-list (lambda (d) (marshal codec-default-probe d)))
             '(64 64 64))
      (check "codec-probe, a character the codec doesn't map signals an error on write"
             (signals-error-p
              (lambda () (to-list (lambda (d)
                                    (marshal codec-probe d :num 1
                                             :text (coerce (list #\A #\Tab #\B #\C #\D)
                                                           'string))))))
             t)
      (check "codec-probe, a code the codec doesn't assign signals an error on read"
             (signals-error-p
              (lambda () (unmarshal codec-probe (->bytes #(0 1 #x30 #xC1 #xC1 #xC1 #xC1)))))
             t)
      (check "codec-str-probe, unmapped character in a constant string signals an error"
             (signals-error-p (lambda () (to-list (lambda (d) (marshal codec-str-probe d)))))
             t)

      ;; 2. NUL-terminated strings.
      (check "cstring-probe, \"abc\" then the terminator"
             (to-list (lambda (d) (marshal cstring-probe d :s "abc" :after 9)))
             '(97 98 99 0 9))
      (check "cstring-probe, read back (field after the terminator lines up)"
             (unmarshal cstring-probe (->bytes #(97 98 99 0 9)))
             '(:s #(97 98 99) :after 9))
      (check "cstring-probe, no value writes the terminator alone"
             (to-list (lambda (d) (marshal cstring-probe d :after 9)))
             '(0 9))
      (check "cstring-probe, empty field read back"
             (unmarshal cstring-probe (->bytes #(0 9)))
             '(:s #() :after 9))
      (check "cstring-probe, data containing the terminator signals an error"
             (signals-error-p
              (lambda () (to-list (lambda (d)
                                    (marshal cstring-probe d
                                             :s (coerce (list #\a (code-char 0) #\b) 'string)
                                             :after 9)))))
             t)
      (check "cstring-probe, a source with no terminator signals an error"
             (signals-error-p (lambda () (unmarshal cstring-probe (->bytes #(97 98 99)))))
             t)

      ;; 3. Terminator encoded through the codec.
      (check "ebcdic-terminated-probe, #\\$ written as #x5B"
             (to-list (lambda (d) (marshal ebcdic-terminated-probe d :s "HI" :after 9)))
             '(#xC8 #xC9 #x5B 9))
      (check "ebcdic-terminated-probe, read back as \"HI\""
             (unmarshal ebcdic-terminated-probe (->bytes #(#xC8 #xC9 #x5B 9)))
             '(:s "HI" :after 9))

      ;; 4. Two-byte elements and terminator.
      (check "wide-terminated-probe, little-endian elements and a 2-byte terminator"
             (to-list (lambda (d) (marshal wide-terminated-probe d :s #(#x4142 #x43) :after 9)))
             '(#x42 #x41 #x43 0 0 0 9))
      (check "wide-terminated-probe, read back"
             (unmarshal wide-terminated-probe (->bytes #(#x42 #x41 #x43 0 0 0 9)))
             '(:s #(#x4142 #x43) :after 9))

      ;; 5. A length slot over a span holding a terminated field.
      (let ((buf (ub8 5)))
        (marshal terminated-span-probe buf :s "ab" :k 7)
        (check "terminated-span-probe, length counts the terminator"
               (coerce buf 'list) '(4 97 98 0 7))
        (check "terminated-span-probe, read back"
               (unmarshal terminated-span-probe buf)
               '(:len 4 :s #(97 98) :k 7))))
    (let ((pass (every #'identity results)))
      (format t "~&codec and terminator checks ~:[FAILED~;PASSED~] (~a of ~a).~%"
              pass (count t results) (length results))
      pass)))

;;; ===========================================================================
;;; Part J — LEB128 fields
;;; ===========================================================================

(defmanifest leb-probe (:unit 8 :endian :big)
  (:n     (:u 1 :leb) :default 0)
  (:after :u8 :default 0))

(defmanifest sleb-probe (:unit 8 :endian :big)
  (:n     (:s 1 :leb) :default 0)
  (:after :u8 :default 0))

;; A length slot over a span counts however many bytes the LEB takes.
(defmanifest leb-span-probe (:unit 8 :endian :big)
  (:len :u8 :slot (:length-of :body))
  (span :body
    (:n (:u 1 :leb) :default 0)
    (:k :u8 :default 0)))

(defenum leb-opcode nil :nop 0 :call 200 :jump 300)

(defmanifest leb-enum-probe (:unit 8 :endian :big)
  (:op (:u 1 :leb) :default 0 :enumerate-by leb-opcode))

(defmanifest leb-enum-default-probe (:unit 8 :endian :big)
  (:op (:u 1 :leb) :default :jump :enumerate-by leb-opcode))

;; An unnamed LEB is still read through, so the field after it lines up.
(defmanifest leb-unnamed-probe (:unit 8 :endian :big)
  (_ (:u 1 :leb) :default 300)
  (:k :u8 :default 0))

(defun run-leb-demo ()
  "Marshal and unmarshal unsigned and signed LEB128 fields.
Returns T if all checks pass."
  (let ((results nil))
    (flet ((check (label got want)
             (let ((ok (equalp got want)))
               (push ok results)
               (format t "~&  ~:[FAIL~;ok  ~]  ~a~%" ok label)
               (unless ok
                 (format t "~&          got  ~s~%          want ~s~%" got want))))
           (to-list (manifest-thunk)
             (let ((acc nil))
               (funcall manifest-thunk (lambda (b) (push b acc)))
               (reverse acc))))
      (format t "~&LEB128 checks:~%")

      ;; 1. Unsigned.
      (check "leb-probe, 624485 then the next field"
             (to-list (lambda (d) (marshal leb-probe d :n 624485 :after 9)))
             '(#xE5 #x8E #x26 9))
      (check "leb-probe, read back (field after the LEB lines up)"
             (unmarshal leb-probe (->bytes #(#xE5 #x8E #x26 9)))
             '(:n 624485 :after 9))
      (check "leb-probe, 0, 127 and 128 at the one-byte boundary"
             (mapcar (lambda (x) (to-list (lambda (d) (marshal leb-probe d :n x :after 9))))
                     '(0 127 128))
             '((0 9) (#x7F 9) (#x80 #x01 9)))
      (check "leb-probe, no value writes the default 0"
             (to-list (lambda (d) (marshal leb-probe d)))
             '(0 0))
      (check "leb-probe, a negative value signals an error"
             (signals-error-p (lambda () (to-list (lambda (d) (marshal leb-probe d :n -1)))))
             t)
      (check "leb-probe, an unfinished LEB at the end of the source signals an error"
             (signals-error-p (lambda () (unmarshal leb-probe (->bytes #(#x80 #x80)))))
             t)

      ;; 2. Signed.
      (check "sleb-probe, -123456"
             (to-list (lambda (d) (marshal sleb-probe d :n -123456 :after 9)))
             '(#xC0 #xBB #x78 9))
      (check "sleb-probe, read back"
             (unmarshal sleb-probe (->bytes #(#xC0 #xBB #x78 9)))
             '(:n -123456 :after 9))
      (check "sleb-probe, 63, 64, -64 and -65 at the sign-bit boundary"
             (mapcar (lambda (x) (to-list (lambda (d) (marshal sleb-probe d :n x :after 9))))
                     '(63 64 -64 -65))
             '((#x3F 9) (#xC0 #x00 9) (#x40 9) (#xBF #x7F 9)))

      ;; 3. Round trips, bignums included.
      (check "leb-probe, unsigned values round-trip"
             (loop :for x :in (list 0 1 127 128 300 16383 16384 624485 (expt 2 70))
                   :collect (getf (unmarshal leb-probe
                                             (->bytes (to-list (lambda (d)
                                                                 (marshal leb-probe d :n x)))))
                                  :n))
             (list 0 1 127 128 300 16383 16384 624485 (expt 2 70)))
      (check "sleb-probe, signed values round-trip"
             (loop :for x :in (list 0 1 -1 63 64 -64 -65 -128 -123456 (expt 2 70) (- (expt 2 70)))
                   :collect (getf (unmarshal sleb-probe
                                             (->bytes (to-list (lambda (d)
                                                                 (marshal sleb-probe d :n x)))))
                                  :n))
             (list 0 1 -1 63 64 -64 -65 -128 -123456 (expt 2 70) (- (expt 2 70))))

      ;; 4. A length slot over a span holding a LEB.
      (let ((buf (ub8 4)))
        (marshal leb-span-probe buf :n 300 :k 7)
        (check "leb-span-probe, length counts the LEB's two bytes"
               (coerce buf 'list) '(3 #xAC #x02 7))
        (check "leb-span-probe, read back"
               (unmarshal leb-span-probe buf)
               '(:len 3 :n 300 :k 7)))

      ;; 5. Enumerated and unnamed LEBs.
      (check "leb-enum-probe, :call written as LEB 200"
             (to-list (lambda (d) (marshal leb-enum-probe d :op :call)))
             '(#xC8 #x01))
      (check "leb-enum-probe, read back as :call"
             (unmarshal leb-enum-probe (->bytes #(#xC8 #x01)))
             '(:op :call))
      (check "leb-enum-default-probe, the default :jump written as LEB 300, and read back"
             (let ((bytes (to-list (lambda (d) (marshal leb-enum-default-probe d)))))
               (list bytes (unmarshal leb-enum-default-probe (->bytes bytes))))
             '((#xAC #x02) (:op :jump)))
      (check "leb-unnamed-probe, default 300 written"
             (to-list (lambda (d) (marshal leb-unnamed-probe d :k 7)))
             '(#xAC #x02 7))
      (check "leb-unnamed-probe, read skips the LEB"
             (unmarshal leb-unnamed-probe (->bytes #(#xAC #x02 7)))
             '(:k 7)))
    (let ((pass (every #'identity results)))
      (format t "~&LEB128 checks ~:[FAILED~;PASSED~] (~a of ~a).~%"
              pass (count t results) (length results))
      pass)))

;;; ===========================================================================
;;; Part K — :case widths selected by an earlier field
;;; ===========================================================================

(defmanifest case-probe (:unit 8 :endian :big)
  (:kind  :u8 :default 0)
  (:v     (:case :kind (0 :u8) ((1 3) :u 2) (2 :s 4) (4 :u 1 :leb)) :default 0)
  (:after :u8 :default 0))

(defmanifest case-otherwise-probe (:unit 8 :endian :big)
  (:kind  :u8 :default 0)
  (:v     (:case :kind (0 :u8) (otherwise :u16)) :default 0)
  (:after :u8 :default 0))

(defenum case-size nil :byte 0 :word 1 :long 2 :varint 3)

;; Keyword branch keys are translated through the selector's enum.
(defmanifest case-enum-probe (:unit 8 :endian :big)
  (:kind  :u8 :default :word :enumerate-by case-size)
  (:v     (:case :kind (:byte :u8) (:word :u16) ((:long) :s32) (:varint :u 1 :leb)) :default 0)
  (:after :u8 :default 0))

;; The selector sits outside the span; the length slot counts the chosen width.
(defmanifest case-span-probe (:unit 8 :endian :big)
  (:kind :u8 :default 0)
  (:len  :u8 :slot (:length-of :body))
  (span :body
    (:v (:case :kind (0 :u8) (1 :u32)) :default 0)
    (:k :u8 :default 0)))

(defmanifest case-unnamed-probe (:unit 8 :endian :big)
  (:kind  :u8 :default 0)
  (_      (:case :kind (0 :u8) (1 :u32)) :default 0)
  (:after :u8 :default 0))

;; The :case field's own value is enumerated.
(defmanifest case-value-enum-probe (:unit 8 :endian :big)
  (:kind :u8 :default 0)
  (:op   (:case :kind (0 :u8) (1 :u16)) :default 0 :enumerate-by case-size))

(defmanifest case-value-default-probe (:unit 8 :endian :big)
  (:kind :u8 :default 0)
  (:op   (:case :kind (0 :u8) (1 :u16)) :default :long :enumerate-by case-size))

;; The selector comes after the field it selects for.
(defmanifest case-late-probe (:unit 8 :endian :big)
  (:v    (:case :kind (0 :u8)) :default 0)
  (:kind :u8 :default 0))

;; A :case key that isn't in the selector's enum.
(defmanifest case-badkey-probe (:unit 8 :endian :big)
  (:kind :u8 :default 0 :enumerate-by case-size)
  (:v    (:case :kind (:quad :u64)) :default 0))

;; A :case field inside a sub-manifest, whose marshal expansion is nested.
(defmanifest case-nest-probe (:unit 8 :endian :big)
  (:head :u8 :default 0)
  (manifest :inner case-enum-probe))

;; Two plain fields, for checking each value form is evaluated once.
(defmanifest eval-once-probe (:unit 8 :endian :big)
  (:a :u8 :default 0)
  (:b :u8 :default 0))

(defun run-case-demo ()
  "Marshal and unmarshal :case fields whose width is chosen by an earlier field.
Returns T if all checks pass."
  (let ((results nil))
    (labels ((check (label got want)
               (let ((ok (equalp got want)))
                 (push ok results)
                 (format t "~&  ~:[FAIL~;ok  ~]  ~a~%" ok label)
                 (unless ok
                   (format t "~&          got  ~s~%          want ~s~%" got want))))
             (to-list (manifest-thunk)
               (let ((acc nil))
                 (funcall manifest-thunk (lambda (b) (push b acc)))
                 (reverse acc)))
             (has-case-p (tree)
               (or (eq tree 'case)
                   (and (consp tree) (or (has-case-p (car tree)) (has-case-p (cdr tree)))))))
      (format t "~&:case checks:~%")

      ;; 1. Each branch, with a constant selector.
      (check "case-probe, kind 0 writes one byte"
             (to-list (lambda (d) (marshal case-probe d :kind 0 :v 5 :after 9)))
             '(0 5 9))
      (check "case-probe, kind 1 writes two bytes"
             (to-list (lambda (d) (marshal case-probe d :kind 1 :v #x102 :after 9)))
             '(1 1 2 9))
      (check "case-probe, kind 3 shares the (1 3) branch"
             (to-list (lambda (d) (marshal case-probe d :kind 3 :v #x102 :after 9)))
             '(3 1 2 9))
      (check "case-probe, kind 2 writes a signed four-byte value"
             (to-list (lambda (d) (marshal case-probe d :kind 2 :v -2 :after 9)))
             '(2 #xFF #xFF #xFF #xFE 9))
      (check "case-probe, kind 4 writes a LEB"
             (to-list (lambda (d) (marshal case-probe d :kind 4 :v 300 :after 9)))
             '(4 #xAC #x02 9))

      ;; 2. A selector known only at run time.
      (check "case-probe, a run-time selector picks each branch"
             (loop :for (k v) :in '((0 5) (1 #x102) (2 -2) (3 #x102) (4 300))
                   :collect (to-list (lambda (d) (marshal case-probe d :kind k :v v :after 9))))
             '((0 5 9) (1 1 2 9) (2 #xFF #xFF #xFF #xFE 9) (3 1 2 9) (4 #xAC #x02 9)))
      (check "case-probe, a constant selector is resolved at expansion, a run-time one is not"
             (list (has-case-p (macroexpand-1 '(marshal case-probe d :kind 1 :v 5)))
                   (has-case-p (macroexpand-1 '(marshal case-probe d :kind k :v 5))))
             '(nil t))
      (check "case-probe, an unmatched run-time selector signals an error"
             (signals-error-p
              (lambda () (let ((k 7)) (to-list (lambda (d) (marshal case-probe d :kind k :v 1))))))
             t)
      (check "case-probe, an unmatched constant selector is an expansion error"
             (signals-error-p (lambda () (macroexpand-1 '(marshal case-probe d :kind 7 :v 1))))
             t)

      ;; 3. Enumerated selectors.
      (check "case-enum-probe, defaulted selector (:word) picks the two-byte branch"
             (to-list (lambda (d) (marshal case-enum-probe d :v #x102 :after 9)))
             '(1 1 2 9))
      (check "case-enum-probe, a keyword selector picks the signed branch"
             (to-list (lambda (d) (marshal case-enum-probe d :kind :long :v -2 :after 9)))
             '(2 #xFF #xFF #xFF #xFE 9))
      (check "case-enum-probe, a run-time keyword selector picks the LEB branch"
             (let ((k :varint))
               (to-list (lambda (d) (marshal case-enum-probe d :kind k :v 300 :after 9))))
             '(3 #xAC #x02 9))
      (check "case-enum-probe, read back with the selector decoded"
             (list (unmarshal case-enum-probe (->bytes #(1 1 2 9)))
                   (unmarshal case-enum-probe (->bytes #(2 #xFF #xFF #xFF #xFE 9)))
                   (unmarshal case-enum-probe (->bytes #(3 #xAC #x02 9))))
             '((:kind :word :v #x102 :after 9)
               (:kind :long :v -2 :after 9)
               (:kind :varint :v 300 :after 9)))
      (check "case-badkey-probe, a key outside the enum is an expansion error on both sides"
             (list (signals-error-p (lambda () (macroexpand-1 '(marshal case-badkey-probe d :v 1))))
                   (signals-error-p (lambda () (macroexpand-1 '(unmarshal case-badkey-probe s)))))
             '(t t))

      ;; 4. Default branch, order, and reading.
      (check "case-otherwise-probe, an unlisted selector takes the otherwise branch"
             (to-list (lambda (d) (marshal case-otherwise-probe d :kind 9 :v #x102 :after 9)))
             '(9 1 2 9))
      (check "case-otherwise-probe, read back"
             (unmarshal case-otherwise-probe (->bytes #(9 1 2 9)))
             '(:kind 9 :v #x102 :after 9))
      (check "case-late-probe, a selector after its field is an expansion error on both sides, naming both"
             (flet ((names-both-p (form)
                      ;; the message names the field (:V) and the selector it needs (:KIND)
                      (handler-case (progn (macroexpand-1 form) nil)
                        (error (e) (let ((message (princ-to-string e)))
                                     (and (search ":V " message) (search ":KIND" message) t))))))
               (list (names-both-p '(marshal case-late-probe d :kind 0))
                     (names-both-p '(unmarshal case-late-probe s))))
             '(t t))
      (check "case-probe, every branch round-trips"
             (loop :for (k v) :in '((0 5) (1 #x102) (2 -2) (3 #x102) (4 300))
                   :collect (unmarshal case-probe
                                       (->bytes (to-list (lambda (d)
                                                           (marshal case-probe d
                                                                    :kind k :v v :after 9))))))
             '((:kind 0 :v 5 :after 9) (:kind 1 :v #x102 :after 9) (:kind 2 :v -2 :after 9)
               (:kind 3 :v #x102 :after 9) (:kind 4 :v 300 :after 9)))
      (check "case-probe, an unmatched selector on read signals an error"
             (signals-error-p (lambda () (unmarshal case-probe (->bytes #(7 0 0)))))
             t)

      ;; 5. Spans, unnamed and enumerated :case fields.
      (let ((buf (ub8 7)))
        (marshal case-span-probe buf :kind 1 :v 7 :k 9)
        (check "case-span-probe, length counts the chosen four-byte width"
               (coerce buf 'list) '(1 5 0 0 0 7 9))
        (check "case-span-probe, read back"
               (unmarshal case-span-probe buf)
               '(:kind 1 :len 5 :v 7 :k 9)))
      (check "case-unnamed-probe, the default is written at the chosen width"
             (to-list (lambda (d) (marshal case-unnamed-probe d :kind 1 :after 9)))
             '(1 0 0 0 0 9))
      (check "case-unnamed-probe, read skips the chosen width"
             (unmarshal case-unnamed-probe (->bytes #(1 0 0 0 0 9)))
             '(:kind 1 :after 9))
      (check "case-value-enum-probe, an enumerated :case value written and read back"
             (let ((bytes (to-list (lambda (d) (marshal case-value-enum-probe d :kind 1 :op :long)))))
               (list bytes (unmarshal case-value-enum-probe (->bytes bytes))))
             '((1 0 2) (:kind 1 :op :long)))
      (check "case-value-default-probe, a keyword default for the :case value"
             (let ((bytes (to-list (lambda (d) (marshal case-value-default-probe d :kind 1)))))
               (list bytes (unmarshal case-value-default-probe (->bytes bytes))))
             '((1 0 2) (:kind 1 :op :long)))
      (check "case-nest-probe, a run-time selector inside a sub-manifest"
             (let ((k :long))
               (to-list (lambda (d) (marshal case-nest-probe d :head 7
                                             :inner (:kind k :v -2 :after 9)))))
             '(7 2 #xFF #xFF #xFF #xFE 9))
      (check "case-nest-probe, read back"
             (unmarshal case-nest-probe (->bytes #(7 2 #xFF #xFF #xFF #xFE 9)))
             '(:head 7 :inner (:kind :long :v -2 :after 9)))

      ;; 6. Value forms are evaluated once.
      (check "eval-once-probe, each value form is evaluated once"
             (let ((n 0))
               (list (to-list (lambda (d) (marshal eval-once-probe d :a (incf n) :b (incf n))))
                     n))
             '((1 2) 2)))
    (let ((pass (every #'identity results)))
      (format t "~&:case checks ~:[FAILED~;PASSED~] (~a of ~a).~%"
              pass (count t results) (length results))
      pass)))

;;; ===========================================================================
;;; Part L — :vec fields on the scan model
;;; ===========================================================================

;; A terminated field followed by a scalar, for counting element reads.
(defmanifest vec-scan-probe (:unit 8 :endian :big)
  (:s     (:u 1 :vec) :end-by 0)
  (:after :u8 :default 0))

;; Characters go through the codec; integers are raw codes.
(defmanifest vec-codec-mixed-probe (:unit 8 :endian :big)
  (:s (:u 1 :vec) :count 3 :codec #'specops.map.ebcdic:ebcdic-code-cp037))

;; Signed elements with a terminator that is negative when sign-extended.
(defmanifest vec-signed-probe (:unit 8 :endian :big)
  (:s     (:s 1 :vec) :end-by #xFF)
  (:after :u8 :default 0))

(defenum vec-colour nil :red 1 :green 2 :blue 3)

(defmanifest vec-enum-probe (:unit 8 :endian :big)
  (:c (:u 1 :vec) :count 3 :enumerate-by vec-colour))

(defmanifest vec-plain-probe (:unit 8 :endian :big)
  (:c (:u 1 :vec) :count 1))

(defmanifest vec-unnamed-probe (:unit 8 :endian :big)
  (_  (:u 1 :vec) :end-by 0)
  (:k :u8 :default 0))

;; A :vec branch inherits the field's :end-by.
(defmanifest vec-case-probe (:unit 8 :endian :big)
  (:kind  :u8 :default 0)
  (:v     (:case :kind (0 :u8) (1 :u 1 :vec)) :end-by 0 :default 0)
  (:after :u8 :default 0))

;; A :case field's enum reaches both a scalar and a vector branch.
(defmanifest vec-case-enum-probe (:unit 8 :endian :big)
  (:kind :u8 :default 0)
  (:v    (:case :kind (0 :u8) (1 :u 1 :vec)) :count 2 :default 0 :enumerate-by vec-colour))

(defmanifest vec-wide-probe (:unit 8 :endian :big)
  (:s (:u 2 :vec) :count 2 :endian :little))

(defun run-vec-scan-demo ()
  "Marshal and unmarshal :vec fields built from stop rules and assemblers.
Returns T if all checks pass."
  (let ((results nil))
    (labels ((check (label got want)
               (let ((ok (equalp got want)))
                 (push ok results)
                 (format t "~&  ~:[FAIL~;ok  ~]  ~a~%" ok label)
                 (unless ok
                   (format t "~&          got  ~s~%          want ~s~%" got want))))
             (to-list (manifest-thunk)
               (let ((acc nil))
                 (funcall manifest-thunk (lambda (b) (push b acc)))
                 (reverse acc)))
             (count-reads (tree)
               ;; element reads are calls of a deserializer: (funcall (aref ds …) …)
               (if (consp tree)
                   (+ (if (and (eq (car tree) 'funcall) (consp (second tree))
                               (eq (car (second tree)) 'aref))
                          1 0)
                      (count-reads (car tree)) (count-reads (cdr tree)))
                   0)))
      (format t "~&vector scan checks:~%")

      ;; 1. One read per element.
      (check "vec-scan-probe, a terminated field reads each element once"
             (count-reads (macroexpand-1 '(unmarshal vec-scan-probe s)))
             2)

      ;; 2. Element encoding: characters through the codec, integers raw.
      (check "vec-codec-mixed-probe, integers in a codec field are raw codes"
             (to-list (lambda (d) (marshal vec-codec-mixed-probe d :s #(64 64 64))))
             '(#x40 #x40 #x40))
      (check "vec-codec-mixed-probe, characters and integers mixed"
             (to-list (lambda (d) (marshal vec-codec-mixed-probe d :s (vector #\A 64 #\B))))
             '(#xC1 #x40 #xC2))

      ;; 3. Signed elements compare and write raw codes.
      (check "vec-signed-probe, #(1 -128) written with the #xFF terminator"
             (to-list (lambda (d) (marshal vec-signed-probe d :s #(1 -128) :after 9)))
             '(1 #x80 #xFF 9))
      (check "vec-signed-probe, read back"
             (unmarshal vec-signed-probe (->bytes #(1 #x80 #xFF 9)))
             '(:s #(1 -128) :after 9))
      (check "vec-signed-probe, -1 is the terminator's code and signals an error"
             (signals-error-p (lambda () (to-list (lambda (d) (marshal vec-signed-probe d :s #(1 -1))))))
             t)
      (check "vec-signed-probe, -129 does not fit and signals an error"
             (signals-error-p (lambda () (to-list (lambda (d) (marshal vec-signed-probe d :s #(-129))))))
             t)

      ;; 4. Enumerated elements.
      (check "vec-enum-probe, keyword and integer elements written as codes"
             (to-list (lambda (d) (marshal vec-enum-probe d :c #(:green :red 3))))
             '(2 1 3))
      (check "vec-enum-probe, read back as keywords"
             (unmarshal vec-enum-probe (->bytes #(2 1 3)))
             '(:c #(:green :red :blue)))
      (check "vec-enum-probe, a code the enum lacks reads as itself and writes back"
             (let ((value (getf (unmarshal vec-enum-probe (->bytes #(2 9 3))) :c)))
               (list value (to-list (lambda (d) (marshal vec-enum-probe d :c value)))))
             '(#(:green 9 :blue) (2 9 3)))
      (check "vec-enum-probe, a keyword outside the enum signals an error"
             (signals-error-p
              (lambda () (to-list (lambda (d) (marshal vec-enum-probe d :c #(:green :mauve 3))))))
             t)
      (check "vec-plain-probe, a keyword element without an enum signals an error"
             (signals-error-p (lambda () (to-list (lambda (d) (marshal vec-plain-probe d :c #(:red))))))
             t)

      ;; 5. Unnamed fields, :case branches and wide elements.
      (check "vec-unnamed-probe, an unnamed terminated field writes its terminator"
             (to-list (lambda (d) (marshal vec-unnamed-probe d :k 7)))
             '(0 7))
      (check "vec-unnamed-probe, read skips it"
             (unmarshal vec-unnamed-probe (->bytes #(97 98 0 7)))
             '(:k 7))
      (check "vec-case-probe, a scalar branch and a terminated vector branch written"
             (list (to-list (lambda (d) (marshal vec-case-probe d :kind 0 :v 5 :after 9)))
                   (to-list (lambda (d) (marshal vec-case-probe d :kind 1 :v "ab" :after 9))))
             '((0 5 9) (1 97 98 0 9)))
      (check "vec-case-probe, both branches read back"
             (list (unmarshal vec-case-probe (->bytes #(0 5 9)))
                   (unmarshal vec-case-probe (->bytes #(1 97 98 0 9))))
             '((:kind 0 :v 5 :after 9) (:kind 1 :v #(97 98) :after 9)))
      (check "vec-case-enum-probe, the enum decodes a scalar branch and a vector branch"
             (list (unmarshal vec-case-enum-probe (->bytes #(0 3)))
                   (unmarshal vec-case-enum-probe (->bytes #(1 2 1))))
             '((:kind 0 :v :blue) (:kind 1 :v #(:green :red))))
      (check "vec-wide-probe, two-byte little-endian elements written"
             (to-list (lambda (d) (marshal vec-wide-probe d :s #(#x4142 #x43))))
             '(#x42 #x41 #x43 0))
      (check "vec-wide-probe, read back"
             (unmarshal vec-wide-probe (->bytes #(#x42 #x41 #x43 0)))
             '(:s #(#x4142 #x43)))
      (check "vec-wide-probe, a list is accepted as a vector's value"
             (let ((value (list #x4142 #x43)))
               (to-list (lambda (d) (marshal vec-wide-probe d :s value))))
             '(#x42 #x41 #x43 0)))
    (let ((pass (every #'identity results)))
      (format t "~&vector scan checks ~:[FAILED~;PASSED~] (~a of ~a).~%"
              pass (count t results) (length results))
      pass)))

;;; ===========================================================================
;;; Part M — fixed buffers holding a terminator (:count with :end-by)
;;; ===========================================================================

;; A five-byte NUL-terminated buffer, tar-style: a full buffer needs no terminator.
(defmanifest bounded-probe (:unit 8 :endian :big)
  (:name  (:u 1 :vec) :count 5 :end-by 0)
  (:after :u8 :default 0))

;; An EBCDIC buffer whose character terminator goes through the codec.
(defmanifest bounded-ebcdic-probe (:unit 8 :endian :big)
  (:name (:u 1 :vec) :count 4 :end-by #\$
         :codec #'specops.map.ebcdic:ebcdic-code-cp037))

;; Two-byte little-endian elements: the buffer is three elements, six bytes.
(defmanifest bounded-wide-probe (:unit 8 :endian :big)
  (:s (:u 2 :vec) :count 3 :end-by 0 :endian :little))

;; A length slot over a span counts the whole buffer.
(defmanifest bounded-span-probe (:unit 8 :endian :big)
  (:len :u8 :slot (:length-of :body))
  (span :body
    (:name (:u 1 :vec) :count 4 :end-by 0)
    (:k    :u8 :default 0)))

;; The buffer's size comes from an earlier field.
(defmanifest bounded-counted-probe (:unit 8 :endian :big)
  (:n     :u8 :default 0)
  (:name  (:u 1 :vec) :count :n :end-by 0)
  (:after :u8 :default 0))

(defun run-bounded-demo ()
  "Marshal and unmarshal fixed buffers that hold a terminator.
Returns T if all checks pass."
  (let ((results nil))
    (flet ((check (label got want)
             (let ((ok (equalp got want)))
               (push ok results)
               (format t "~&  ~:[FAIL~;ok  ~]  ~a~%" ok label)
               (unless ok
                 (format t "~&          got  ~s~%          want ~s~%" got want))))
           (to-list (manifest-thunk)
             (let ((acc nil))
               (funcall manifest-thunk (lambda (b) (push b acc)))
               (reverse acc))))
      (format t "~&bounded buffer checks:~%")

      ;; 1. Writing: short, full and empty buffers.
      (check "bounded-probe, short data is followed by terminators to the buffer's end"
             (to-list (lambda (d) (marshal bounded-probe d :name "ab" :after 9)))
             '(97 98 0 0 0 9))
      (check "bounded-probe, data filling the buffer has no terminator"
             (to-list (lambda (d) (marshal bounded-probe d :name "abcde" :after 9)))
             '(97 98 99 100 101 9))
      (check "bounded-probe, no value fills the buffer with terminators"
             (to-list (lambda (d) (marshal bounded-probe d :after 9)))
             '(0 0 0 0 0 9))
      (check "bounded-probe, data longer than the buffer signals an error"
             (signals-error-p (lambda () (to-list (lambda (d) (marshal bounded-probe d :name "abcdef")))))
             t)
      (check "bounded-probe, data containing the terminator signals an error"
             (signals-error-p
              (lambda () (to-list (lambda (d) (marshal bounded-probe d :name (vector 97 0 98))))))
             t)

      ;; 2. Reading: stop at the terminator or the buffer's end, move past the buffer.
      (check "bounded-probe, a short buffer read back"
             (unmarshal bounded-probe (->bytes #(97 98 0 0 0 9)))
             '(:name #(97 98) :after 9))
      (check "bounded-probe, a full buffer read back without a terminator"
             (unmarshal bounded-probe (->bytes #(97 98 99 100 101 9)))
             '(:name #(97 98 99 100 101) :after 9))
      (check "bounded-probe, bytes after the terminator are skipped, whatever they hold"
             (unmarshal bounded-probe (->bytes #(97 0 122 122 122 9)))
             '(:name #(97) :after 9))
      (check "bounded-probe, an empty buffer read back"
             (unmarshal bounded-probe (->bytes #(0 0 0 0 0 9)))
             '(:name #() :after 9))
      (check "bounded-probe, a source shorter than the buffer signals an error"
             (signals-error-p (lambda () (unmarshal bounded-probe (->bytes #(97 98 0 0)))))
             t)

      ;; 3. Codecs, wide elements, spans and counts from other fields.
      (check "bounded-ebcdic-probe, \"HI\" then $ (#x5B) to the buffer's end"
             (to-list (lambda (d) (marshal bounded-ebcdic-probe d :name "HI")))
             '(#xC8 #xC9 #x5B #x5B))
      (check "bounded-ebcdic-probe, read back as a string"
             (unmarshal bounded-ebcdic-probe (->bytes #(#xC8 #xC9 #x5B #x5B)))
             '(:name "HI"))
      (check "bounded-wide-probe, one two-byte element then two terminator elements"
             (to-list (lambda (d) (marshal bounded-wide-probe d :s #(#x4142))))
             '(#x42 #x41 0 0 0 0))
      (check "bounded-wide-probe, read back"
             (unmarshal bounded-wide-probe (->bytes #(#x42 #x41 0 0 0 0)))
             '(:s #(#x4142)))
      (let ((buf (ub8 6)))
        (marshal bounded-span-probe buf :name "ab" :k 7)
        (check "bounded-span-probe, the length slot counts the whole buffer"
               (coerce buf 'list) '(5 97 98 0 0 7))
        (check "bounded-span-probe, read back"
               (unmarshal bounded-span-probe buf)
               '(:len 5 :name #(97 98) :k 7)))
      (check "bounded-counted-probe, a buffer sized by an earlier field read back"
             (unmarshal bounded-counted-probe (->bytes #(4 97 0 122 122 9)))
             '(:n 4 :name #(97) :after 9))
      (check "bounded-counted-probe, writing it needs an integer :count for now"
             (signals-error-p
              (lambda () (macroexpand-1 '(marshal bounded-counted-probe d :n 4 :name "a"))))
             t)

      ;; 4. Facts: the data count is unknown, the extent is the buffer.
      (let ((plan (field-plan (second (gethash 'bounded-probe *manifests*))
                              :count-form (lambda (i) (getf i :count)))))
        (check "bounded-probe, known-count is NIL and known-extent is 5 units"
               (list (known-count (slot-value plan 'rule)) (known-extent plan))
               '(nil 5))))
    (let ((pass (every #'identity results)))
      (format t "~&bounded buffer checks ~:[FAILED~;PASSED~] (~a of ~a).~%"
              pass (count t results) (length results))
      pass)))

;;; ===========================================================================
;;; Part N — terminators of several elements
;;; ===========================================================================

;; A line ended by CR LF, given as a vector of codes.
(defmanifest crlf-probe (:unit 8 :endian :big)
  (:line  (:u 1 :vec) :end-by #(13 10))
  (:after :u8 :default 0))

;; The same terminator given as a string.
(defmanifest crlf-string-probe (:unit 8 :endian :big)
  (:line  (:u 1 :vec) :end-by #.(coerce (list #\Return #\Newline) 'string))
  (:after :u8 :default 0))

;; A pattern that overlaps itself, for matches that straddle data and terminator.
(defmanifest aa-probe (:unit 8 :endian :big)
  (:s     (:u 1 :vec) :end-by "AA")
  (:after :u8 :default 0))

;; Characters of the terminator go through the codec ($ is #x5B in CP037).
(defmanifest ebcdic-multi-probe (:unit 8 :endian :big)
  (:name (:u 1 :vec) :end-by "$$" :codec #'specops.map.ebcdic:ebcdic-code-cp037))

;; A six-byte buffer ended by CR LF.
(defmanifest bounded-crlf-probe (:unit 8 :endian :big)
  (:line  (:u 1 :vec) :count 6 :end-by #(13 10))
  (:after :u8 :default 0))

(defmanifest empty-terminator-probe (:unit 8 :endian :big)
  (:s (:u 1 :vec) :end-by #()))

(defun run-multi-terminator-demo ()
  "Marshal and unmarshal fields ended by terminators of several elements.
Returns T if all checks pass."
  (let ((results nil))
    (flet ((check (label got want)
             (let ((ok (equalp got want)))
               (push ok results)
               (format t "~&  ~:[FAIL~;ok  ~]  ~a~%" ok label)
               (unless ok
                 (format t "~&          got  ~s~%          want ~s~%" got want))))
           (to-list (manifest-thunk)
             (let ((acc nil))
               (funcall manifest-thunk (lambda (b) (push b acc)))
               (reverse acc))))
      (format t "~&multi-element terminator checks:~%")

      ;; 1. CR LF, as a vector and as a string.
      (check "crlf-probe, \"ab\" then CR LF"
             (to-list (lambda (d) (marshal crlf-probe d :line "ab" :after 9)))
             '(97 98 13 10 9))
      (check "crlf-probe, read back (both elements of the terminator dropped)"
             (unmarshal crlf-probe (->bytes #(97 98 13 10 9)))
             '(:line #(97 98) :after 9))
      (check "crlf-string-probe, a string terminator writes the same bytes"
             (to-list (lambda (d) (marshal crlf-string-probe d :line "ab" :after 9)))
             '(97 98 13 10 9))
      (check "crlf-probe, no value writes the terminator alone, and reads back empty"
             (list (to-list (lambda (d) (marshal crlf-probe d :after 9)))
                   (unmarshal crlf-probe (->bytes #(13 10 9))))
             '((13 10 9) (:line #() :after 9)))

      ;; 2. Partial matches are data.
      (check "crlf-probe, a CR not followed by LF is data, on both sides"
             (list (to-list (lambda (d) (marshal crlf-probe d :line #(97 13 98) :after 9)))
                   (unmarshal crlf-probe (->bytes #(97 13 98 13 10 9))))
             '((97 13 98 13 10 9) (:line #(97 13 98) :after 9)))
      (check "crlf-probe, data ending in CR reads back whole"
             (list (to-list (lambda (d) (marshal crlf-probe d :line #(97 13) :after 9)))
                   (unmarshal crlf-probe (->bytes #(97 13 13 10 9))))
             '((97 13 13 10 9) (:line #(97 13) :after 9)))

      ;; 3. Data that would end the field early.
      (check "crlf-probe, data containing CR LF signals an error"
             (signals-error-p
              (lambda () (to-list (lambda (d) (marshal crlf-probe d :line #(97 13 10 98))))))
             t)
      (check "aa-probe, data ending in A would form AA with the terminator: an error"
             (signals-error-p (lambda () (to-list (lambda (d) (marshal aa-probe d :s "xA")))))
             t)
      (check "aa-probe, data starting with A is fine, and reads back"
             (let ((bytes (to-list (lambda (d) (marshal aa-probe d :s "Ax" :after 9)))))
               (list bytes (unmarshal aa-probe (->bytes bytes))))
             '((65 120 65 65 9) (:s #(65 120) :after 9)))
      (check "crlf-probe, a source with no terminator signals an error"
             (signals-error-p (lambda () (unmarshal crlf-probe (->bytes #(97 98 13)))))
             t)

      ;; 4. A codec, and an empty terminator.
      (check "ebcdic-multi-probe, \"HI\" then $$ encoded as #x5B #x5B"
             (to-list (lambda (d) (marshal ebcdic-multi-probe d :name "HI")))
             '(#xC8 #xC9 #x5B #x5B))
      (check "ebcdic-multi-probe, read back as a string"
             (unmarshal ebcdic-multi-probe (->bytes #(#xC8 #xC9 #x5B #x5B)))
             '(:name "HI"))
      (check "empty-terminator-probe, an empty :end-by is an expansion error"
             (signals-error-p (lambda () (macroexpand-1 '(unmarshal empty-terminator-probe s))))
             t)

      ;; 5. Fixed buffers ended by CR LF.
      (check "bounded-crlf-probe, \"ab\" then CR LF repeated to the buffer's end"
             (to-list (lambda (d) (marshal bounded-crlf-probe d :line "ab" :after 9)))
             '(97 98 13 10 13 10 9))
      (check "bounded-crlf-probe, \"abcd\" leaves room for exactly one CR LF"
             (to-list (lambda (d) (marshal bounded-crlf-probe d :line "abcd" :after 9)))
             '(97 98 99 100 13 10 9))
      (check "bounded-crlf-probe, \"abcdef\" fills the buffer with no terminator"
             (to-list (lambda (d) (marshal bounded-crlf-probe d :line "abcdef" :after 9)))
             '(97 98 99 100 101 102 9))
      (check "bounded-crlf-probe, \"abcde\" leaves room for half a terminator: an error"
             (signals-error-p
              (lambda () (to-list (lambda (d) (marshal bounded-crlf-probe d :line "abcde")))))
             t)
      (check "bounded-crlf-probe, read back, skipping what follows the terminator"
             (list (unmarshal bounded-crlf-probe (->bytes #(97 98 13 10 13 10 9)))
                   (unmarshal bounded-crlf-probe (->bytes #(97 13 10 122 122 122 9))))
             '((:line #(97 98) :after 9) (:line #(97) :after 9)))
      (check "bounded-crlf-probe, a full buffer ending in CR keeps the CR as data"
             (unmarshal bounded-crlf-probe (->bytes #(97 98 99 100 101 13 9)))
             '(:line #(97 98 99 100 101 13) :after 9)))
    (let ((pass (every #'identity results)))
      (format t "~&multi-element terminator checks ~:[FAILED~;PASSED~] (~a of ~a).~%"
              pass (count t results) (length results))
      pass)))

;;; ===========================================================================
;;; Part O — vectors of LEB128 elements
;;; ===========================================================================

(defmanifest leb-vec-probe (:unit 8 :endian :big)
  (:v     (:u 1 :vec :leb) :count 3)
  (:after :u8 :default 0))

(defmanifest sleb-vec-probe (:unit 8 :endian :big)
  (:v (:s 1 :vec :leb) :count 3))

;; The element count comes from an earlier field.
(defmanifest leb-vec-counted-probe (:unit 8 :endian :big)
  (:n     :u8 :default 0)
  (:v     (:u 1 :vec :leb) :count :n)
  (:after :u8 :default 0))

;; Ended by an element whose value is 0.
(defmanifest leb-vec-terminated-probe (:unit 8 :endian :big)
  (:v     (:u 1 :vec :leb) :end-by 0)
  (:after :u8 :default 0))

;; Enumerated elements: leb-opcode is :nop 0, :call 200, :jump 300.
(defmanifest leb-vec-enum-probe (:unit 8 :endian :big)
  (:ops (:u 1 :vec :leb) :end-by 0 :enumerate-by leb-opcode))

;; A LEB vector as one branch of a :case field.
(defmanifest leb-vec-case-probe (:unit 8 :endian :big)
  (:kind  :u8 :default 0)
  (:v     (:case :kind (0 :u8) (1 :u 1 :vec :leb)) :end-by 0 :default 0)
  (:after :u8 :default 0))

;; A length slot counts bytes, which cannot give an element count on read.
(defmanifest leb-vec-length-probe (:unit 8 :endian :big)
  (:len :u8 :slot (:length-of :v))
  (:v   (:u 1 :vec :leb)))

(defmanifest leb-vec-bounded-probe (:unit 8 :endian :big)
  (:v (:u 1 :vec :leb) :count 3 :end-by 0))

(defmanifest leb-vec-wide-probe (:unit 8 :endian :big)
  (:v (:u 2 :vec :leb) :count 3))

(defun run-leb-vector-demo ()
  "Marshal and unmarshal vectors whose elements are LEB128 integers.
Returns T if all checks pass."
  (let ((results nil))
    (flet ((check (label got want)
             (let ((ok (equalp got want)))
               (push ok results)
               (format t "~&  ~:[FAIL~;ok  ~]  ~a~%" ok label)
               (unless ok
                 (format t "~&          got  ~s~%          want ~s~%" got want))))
           (to-list (manifest-thunk)
             (let ((acc nil))
               (funcall manifest-thunk (lambda (b) (push b acc)))
               (reverse acc))))
      (format t "~&LEB vector checks:~%")

      ;; 1. Counted vectors.
      (check "leb-vec-probe, #(1 300 624485) as LEBs of one, two and three bytes"
             (to-list (lambda (d) (marshal leb-vec-probe d :v #(1 300 624485) :after 9)))
             '(1 #xAC #x02 #xE5 #x8E #x26 9))
      (check "leb-vec-probe, read back, with the field after lined up"
             (unmarshal leb-vec-probe (->bytes #(1 #xAC #x02 #xE5 #x8E #x26 9)))
             '(:v #(1 300 624485) :after 9))
      (check "leb-vec-probe, a bignum element round-trips"
             (let ((v (vector 0 (expt 2 70) 5)))
               (getf (unmarshal leb-vec-probe
                                (->bytes (to-list (lambda (d) (marshal leb-vec-probe d :v v)))))
                     :v))
             (vector 0 (expt 2 70) 5))
      (check "sleb-vec-probe, signed elements #(-1 64 -123456)"
             (to-list (lambda (d) (marshal sleb-vec-probe d :v #(-1 64 -123456))))
             '(#x7F #xC0 #x00 #xC0 #xBB #x78))
      (check "sleb-vec-probe, read back"
             (unmarshal sleb-vec-probe (->bytes #(#x7F #xC0 #x00 #xC0 #xBB #x78)))
             '(:v #(-1 64 -123456)))
      (check "leb-vec-counted-probe, a count from an earlier field, written and read back"
             (let ((bytes (to-list (lambda (d) (marshal leb-vec-counted-probe d
                                                        :n 2 :v #(5 300) :after 9)))))
               (list bytes (unmarshal leb-vec-counted-probe (->bytes bytes))))
             '((2 5 #xAC #x02 9) (:n 2 :v #(5 300) :after 9)))
      (check "leb-vec-probe, a LEB unfinished at the end of the source signals an error"
             (signals-error-p (lambda () (unmarshal leb-vec-probe (->bytes #(1 #xAC)))))
             t)

      ;; 2. Terminated vectors: the terminator is compared with element values.
      (check "leb-vec-terminated-probe, #(127 128) then the element 0"
             (to-list (lambda (d) (marshal leb-vec-terminated-probe d :v #(127 128) :after 9)))
             '(#x7F #x80 #x01 0 9))
      (check "leb-vec-terminated-probe, read back"
             (unmarshal leb-vec-terminated-probe (->bytes #(#x7F #x80 #x01 0 9)))
             '(:v #(127 128) :after 9))
      (check "leb-vec-terminated-probe, an element with the value 0 signals an error"
             (signals-error-p
              (lambda () (to-list (lambda (d) (marshal leb-vec-terminated-probe d :v #(5 0 6))))))
             t)
      (check "leb-vec-enum-probe, keywords written as LEB codes, read back as keywords"
             (let ((bytes (to-list (lambda (d) (marshal leb-vec-enum-probe d :ops #(:call :jump))))))
               (list bytes (unmarshal leb-vec-enum-probe (->bytes bytes))))
             '((#xC8 #x01 #xAC #x02 0) (:ops #(:call :jump))))
      (check "leb-vec-case-probe, a LEB vector branch inheriting :end-by, both ways"
             (let ((bytes (to-list (lambda (d) (marshal leb-vec-case-probe d :kind 1 :v #(300) :after 9)))))
               (list bytes (unmarshal leb-vec-case-probe (->bytes bytes))))
             '((1 #xAC #x02 0 9) (:kind 1 :v #(300) :after 9)))

      ;; 3. What a LEB vector cannot be, yet.
      (let ((buf (ub8 4)))
        (marshal leb-vec-length-probe buf :v #(1 300))
        (check "leb-vec-length-probe, the length slot holds the vector's bytes on write"
               (coerce buf 'list) '(3 1 #xAC #x02)))
      (check "leb-vec-length-probe, reading needs :count or :end-by: an expansion error"
             (signals-error-p (lambda () (macroexpand-1 '(unmarshal leb-vec-length-probe s))))
             t)
      (check "leb-vec-bounded-probe, a fixed buffer of LEBs is an expansion error on both sides"
             (list (signals-error-p (lambda () (macroexpand-1 '(unmarshal leb-vec-bounded-probe s))))
                   (signals-error-p (lambda () (macroexpand-1 '(marshal leb-vec-bounded-probe d :v #(1))))))
             '(t t))
      (check "leb-vec-wide-probe, LEB elements wider than one byte: an expansion error"
             (signals-error-p (lambda () (macroexpand-1 '(unmarshal leb-vec-wide-probe s))))
             t))
    (let ((pass (every #'identity results)))
      (format t "~&LEB vector checks ~:[FAILED~;PASSED~] (~a of ~a).~%"
              pass (count t results) (length results))
      pass)))

;;; ===========================================================================
;;; Part P — user-defined stop rules (:stop-by) and the runtime protocol
;;; ===========================================================================

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

(defmanifest high-bit-probe (:unit 8 :endian :big)
  (:v     (:u 1 :vec) :stop-by high-bit-run)
  (:after :u8 :default 0))

(defmanifest double-zero-probe (:unit 8 :endian :big)
  (:v     (:u 1 :vec) :stop-by double-zero)
  (:after :u8 :default 0))

(defmanifest stop-at-probe (:unit 8 :endian :big)
  (:v     (:u 1 :vec) :stop-by (stop-at :code 255))
  (:after :u8 :default 0))

(defmanifest stop-at-inline-probe (:unit 8 :endian :big)
  (:v     (:u 1 :vec) :stop-by (stop-at-inline :code 255))
  (:after :u8 :default 0))

;; A rule defined in another package, through the names specops exports.
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

(in-package #:specops)

(defmanifest foreign-rule-probe (:unit 8 :endian :big)
  (:v     (:u 1 :vec) :stop-by (specops.test.rules:until-code :code 254))
  (:after :u8 :default 0))

(defmanifest stop-by-unknown-probe (:unit 8 :endian :big)
  (:v (:u 1 :vec) :stop-by no-such-rule))

(defmanifest stop-by-with-end-by-probe (:unit 8 :endian :big)
  (:v (:u 1 :vec) :stop-by double-zero :end-by 0))

(defun run-stop-by-demo ()
  "Marshal and unmarshal fields ended by user-defined stop rules.
Returns T if all checks pass."
  (let ((results nil))
    (labels ((check (label got want)
               (let ((ok (equalp got want)))
                 (push ok results)
                 (format t "~&  ~:[FAIL~;ok  ~]  ~a~%" ok label)
                 (unless ok
                   (format t "~&          got  ~s~%          want ~s~%" got want))))
             (to-list (manifest-thunk)
               (let ((acc nil))
                 (funcall manifest-thunk (lambda (b) (push b acc)))
                 (reverse acc)))
             (mentions-p (tree symbols)
               (if (consp tree)
                   (or (mentions-p (car tree) symbols) (mentions-p (cdr tree) symbols))
                   (and (member tree symbols) t))))
      (format t "~&stop-by checks:~%")

      ;; 1. done-p, and fresh state per field.
      (check "quic-probe, a 1-byte and a 2-byte varint, each field with its own rule"
             (unmarshal quic-probe (->bytes #(#x25 #x7B #xBD 9)))
             '(:a #(#x25) :b #(#x7B #xBD) :after 9))
      (check "quic-probe, written unchanged"
             (to-list (lambda (d) (marshal quic-probe d :a #(#x25) :b #(#x7B #xBD) :after 9)))
             '(#x25 #x7B #xBD 9))
      (check "quic-probe, a source that ends before the varint does signals an error"
             (signals-error-p (lambda () (unmarshal quic-probe (->bytes #(#x25 #x7B)))))
             t)

      ;; 2. :keep and frame-element.
      (check "high-bit-probe, every element but the last is marked on write"
             (to-list (lambda (d) (marshal high-bit-probe d :v #(1 2 3) :after 9)))
             '(#x81 #x82 3 9))
      (check "high-bit-probe, read back: the unmarked element ends the field and is kept"
             (unmarshal high-bit-probe (->bytes #(#x81 #x82 3 9)))
             '(:v #(#x81 #x82 3) :after 9))

      ;; 3. :drop, trailer and frame-trailer.
      (check "double-zero-probe, data then two zeros"
             (to-list (lambda (d) (marshal double-zero-probe d :v #(5 0 6) :after 9)))
             '(5 0 6 0 0 9))
      (check "double-zero-probe, read back: a single zero is data, both final zeros dropped"
             (unmarshal double-zero-probe (->bytes #(5 0 6 0 0 9)))
             '(:v #(5 0 6) :after 9))

      ;; 4. Initargs, and the upgrade to inline code.
      (check "stop-at-probe, a terminator given by an initarg, written and read back"
             (let ((bytes (to-list (lambda (d) (marshal stop-at-probe d :v #(1 2) :after 9)))))
               (list bytes (unmarshal stop-at-probe (->bytes bytes))))
             '((1 2 255 9) (:v #(1 2) :after 9)))
      (check "stop-at-inline-probe, the upgraded rule gives the same bytes and values"
             (let ((bytes (to-list (lambda (d) (marshal stop-at-inline-probe d :v #(1 2) :after 9)))))
               (list bytes (unmarshal stop-at-inline-probe (->bytes bytes))))
             '((1 2 255 9) (:v #(1 2) :after 9)))
      (check "stop-at-probe calls the runtime protocol; stop-at-inline-probe does not"
             (list (mentions-p (macroexpand-1 '(unmarshal stop-at-probe s)) '(feed done-p))
                   (mentions-p (macroexpand-1 '(marshal stop-at-probe d :v #(1))) '(frame-trailer))
                   (mentions-p (macroexpand-1 '(unmarshal stop-at-inline-probe s))
                               '(feed done-p make-instance))
                   (mentions-p (macroexpand-1 '(marshal stop-at-inline-probe d :v #(1)))
                               '(frame-element frame-trailer make-instance)))
             '(t t nil nil))

      (check "foreign-rule-probe, a rule from another package, written and read back"
             (let ((bytes (to-list (lambda (d) (marshal foreign-rule-probe d :v #(1 2) :after 9)))))
               (list bytes (unmarshal foreign-rule-probe (->bytes bytes))))
             '((1 2 254 9) (:v #(1 2) :after 9)))

      ;; 5. Fields that cannot use :stop-by.
      (check "stop-by-unknown-probe, a class that does not exist is an expansion error"
             (signals-error-p (lambda () (macroexpand-1 '(unmarshal stop-by-unknown-probe s))))
             t)
      (check "stop-by-with-end-by-probe, :stop-by with :end-by is an expansion error"
             (signals-error-p (lambda () (macroexpand-1 '(unmarshal stop-by-with-end-by-probe s))))
             t)
      (check ":stop-by on a scalar field is an error when the manifest is defined"
             (signals-error-p (lambda () (macroexpand-1 '(defmanifest stop-by-scalar-probe
                                                          (:unit 8 :endian :big)
                                                          (:v :u8 :stop-by double-zero)))))
             t))
    (let ((pass (every #'identity results)))
      (format t "~&stop-by checks ~:[FAILED~;PASSED~] (~a of ~a).~%"
              pass (count t results) (length results))
      pass)))

;;; ===========================================================================
;;; Part Q — records: values from a record at run time, and round trips
;;; ===========================================================================

;; A field with no default, which a record must supply.
(defmanifest record-required-probe (:unit 8 :endian :big)
  (:a :u8)
  (:b :u8 :default 5))

;; Records: a class named after the manifest, and one named explicitly.
(defmanifest rec-ihdr (:unit 8 :endian :big :record t)
  (:width      (:u 4) :default 0)
  (:height     (:u 4) :default 0)
  (:color-type :u8    :default 0 :enumerate-by png-color-type))

;; A chunk whose record holds the body's record; the length and CRC are slot fields.
(defmanifest rec-chunk (:unit 8 :endian :big :record rec-chunk-record)
  (:length (:u 4) :slot (:length-of :body))
  (span :type+data
    (str "RCHK")
    (manifest :body rec-ihdr))
  (:crc (:u 4) :default 0 :slot (:checksum-of :type+data :by #'crc)))

;; A record whose sub-manifest has no record type, so that field holds a plist.
(defmanifest rec-parent (:unit 8 :endian :big :record t)
  (:a     :u8 :default 0)
  (manifest :child unmarshal-probe))

;; Masque keyword groups are fields of the record.
(defmanifest rec-masque (:unit 8 :endian :big :record t)
  (masque "h:aabb" (a :mode  :u8 :default 0 :enumerate-by png-color-type)
                   (b :delta (:s 1) :default 0))
  (:tail :u8 :default 0))

;; A record class named in another package: its accessors and constructor go there too.
(defmanifest rec-foreign (:unit 8 :endian :big :record specops.test.rules::rule-record)
  (:a :u8 :default 0))

(defun write-ihdr-from (record)
  "Marshal a png-ihdr body from RECORD, a plist known only when this runs."
  (let ((buf (ub8 13)))
    (marshal png-ihdr buf (:from record))
    buf))

(defun run-record-demo ()
  "Marshal from records given at run time, and round-trip records.
Returns T if all checks pass."
  (let ((results nil))
    (flet ((check (label got want)
             (let ((ok (equalp got want)))
               (push ok results)
               (format t "~&  ~:[FAIL~;ok  ~]  ~a~%" ok label)
               (unless ok
                 (format t "~&          got  ~s~%          want ~s~%" got want))))
           (to-list (manifest-thunk)
             (let ((acc nil))
               (funcall manifest-thunk (lambda (b) (push b acc)))
               (reverse acc))))
      (format t "~&record checks:~%")

      ;; 1. Flat records known only at run time.
      (let ((literal (ub8 13)))
        (marshal png-ihdr literal :width 640 :height 480 :bit-depth 8
                                  :color-type :rgba :interlace :adam7)
        (check "png-ihdr, a plist record writes the same bytes as literal values"
               (write-ihdr-from (list :width 640 :height 480 :bit-depth 8
                                      :color-type :rgba :interlace :adam7))
               literal))
      (check "png-ihdr, one compiled marshal writes two different records"
             (list (coerce (write-ihdr-from (list :width 1 :height 1)) 'list)
                   (coerce (write-ihdr-from (list :width 2 :height 3 :color-type :rgb)) 'list))
             '((0 0 0 1  0 0 0 1  8 0 0 0 0) (0 0 0 2  0 0 0 3  8 2 0 0 0)))
      (check "png-ihdr, fields the record lacks take their defaults"
             (unmarshal png-ihdr (write-ihdr-from (list :width 1 :height 1)))
             '(:width 1 :height 1 :bit-depth 8 :color-type :grayscale
               :compression 0 :filter 0 :interlace :none))
      (check "png-ihdr, enum values in a record as a keyword, a known code, and an unknown code"
             (mapcar (lambda (c) (aref (write-ihdr-from (list :color-type c)) 9))
                     '(:rgba 2 1))
             '(6 2 1))
      (check "png-ihdr, a literal pair overrides the record"
             (let ((buf (ub8 13)) (rec (list :width 1 :height 1)))
               (marshal png-ihdr buf (:from rec) :height 600)
               (getf (unmarshal png-ihdr buf) :height))
             600)
      (check "record-required-probe, a field with no default and no value in the record"
             (let ((rec (list :b 1)))
               (list (to-list (lambda (d) (marshal record-required-probe d (:from (list :a 7 :b 1)))))
                     (signals-error-p
                      (lambda () (to-list (lambda (d) (marshal record-required-probe d (:from rec))))))))
             '((7 1) t))
      (check "an unknown marshal option is an expansion error"
             (signals-error-p (lambda () (macroexpand-1 '(marshal png-ihdr d (:form rec)))))
             t)

      ;; 2. Nested records; slot fields are recomputed.
      (let ((literal (ub8 25)) (from (ub8 25))
            (rec (list :length 999 :crc 12345
                       :png-ihdr (list :width 1 :height 1 :color-type :rgb))))
        (marshal png-ihdr-chunk literal :png-ihdr (:width 1 :height 1 :color-type :rgb))
        (marshal png-ihdr-chunk from (:from rec))
        (check "png-ihdr-chunk, a nested record; its length and CRC recomputed, not copied"
               from literal))
      (let ((literal (ub8 25)) (from (ub8 25))
            (rec (list :png-ihdr (list :width 1 :height 1 :color-type :rgb))))
        (marshal png-ihdr-chunk literal :png-ihdr (:width 1 :height 2 :color-type :rgb))
        (marshal png-ihdr-chunk from (:from rec) :png-ihdr (:height 2))
        (check "png-ihdr-chunk, a literal pair for a nested field overrides the nested record"
               from literal))

      ;; 3. Other field kinds from a record.
      (check "case-probe, a :case selector and value from a record"
             (let ((rec (list :kind 4 :v 300 :after 9)))
               (to-list (lambda (d) (marshal case-probe d (:from rec)))))
             '(4 #xAC #x02 9))
      (check "case-enum-probe, an enumerated selector from a record, and its default"
             (list (let ((rec (list :kind :long :v -2 :after 9)))
                     (to-list (lambda (d) (marshal case-enum-probe d (:from rec)))))
                   (let ((rec (list :v #x102 :after 9)))
                     (to-list (lambda (d) (marshal case-enum-probe d (:from rec))))))
             '((2 #xFF #xFF #xFF #xFE 9) (1 1 2 9)))
      (let ((literal (ub8 15)) (from (ub8 15))
            (rec (list :data (->bytes #(1 2 3)))))
        ;; a chunk's length slot is patched, so it is written to a vector
        (marshal png-idat-chunk literal :data (->bytes #(1 2 3)))
        (marshal png-idat-chunk from (:from rec))
        (check "png-idat-chunk, a vector from a record" from literal))
      (check "cstring-probe, a terminated vector missing from the record writes the terminator"
             (let ((rec (list :after 9)))
               (to-list (lambda (d) (marshal cstring-probe d (:from rec)))))
             '(0 9))
      (check "leb-vec-probe, a vector with no default missing from the record signals an error"
             (let ((rec (list :after 9)))
               (signals-error-p (lambda () (to-list (lambda (d) (marshal leb-vec-probe d (:from rec)))))))
             t)
      (check "masque-probe-typed, masque groups from a record"
             (let ((rec (list :mode :rgba :delta -3 :tail 7)))
               (to-list (lambda (d) (marshal masque-probe-typed d (:from rec)))))
             '(6 #xFD 7))

      ;; 4. Plists read by unmarshal, written back.
      (check "unmarshal-probe, case-probe and png-ihdr-chunk round-trip through plists"
             (let ((a (->bytes #(#xFF #xFE #x02 #x01 #xD4 #xFE #xFF #xFF 7)))
                   (b (->bytes #(4 #xAC #x02 9)))
                   (c (let ((buf (ub8 25)))
                        (marshal png-ihdr-chunk buf :png-ihdr (:width 7 :height 9 :color-type :rgba))
                        buf)))
               (list (equalp a (->bytes (to-list (lambda (d) (marshal unmarshal-probe d
                                                                     (:from (unmarshal unmarshal-probe a)))))))
                     (equalp b (->bytes (to-list (lambda (d) (marshal case-probe d
                                                                     (:from (unmarshal case-probe b)))))))
                     (let ((buf (ub8 25)))  ; the chunk's length slot is patched in a vector
                       (marshal png-ihdr-chunk buf (:from (unmarshal png-ihdr-chunk c)))
                       (equalp c buf))))
             '(t t t))

      ;; 5. Record classes.
      (check "rec-ihdr, a class with a constructor and accessors named after the manifest"
             (let ((r (make-rec-ihdr :width 640 :color-type :rgba)))
               (list (class-name (class-of r)) (rec-ihdr-width r) (rec-ihdr-height r)
                     (rec-ihdr-color-type r)))
             '(rec-ihdr 640 nil :rgba))
      (check "rec-chunk, an explicit class name, with span children as slots"
             (let ((r (make-rec-chunk-record :length 1 :body 2 :crc 3)))
               (list (class-name (class-of r)) (rec-chunk-record-body r)
                     (field-value r :length) (field-value r :crc) (field-value r :type+data)))
             '(rec-chunk-record 2 1 3 nil))
      (check "rec-foreign, the accessors and constructor are interned in the class's package"
             (let ((accessor (find-symbol "RULE-RECORD-A" :specops.test.rules))
                   (constructor (find-symbol "MAKE-RULE-RECORD" :specops.test.rules)))
               (list (and accessor (fboundp accessor) t) (and constructor (fboundp constructor) t)
                     (find-symbol "RULE-RECORD-A" :specops)))
             '(t t nil))
      (check "rec-ihdr, a record prints with its fields"
             (let ((*package* (find-package :specops)))
               (prin1-to-string (make-rec-ihdr :width 1)))
             "#<REC-IHDR :WIDTH 1 :HEIGHT NIL :COLOR-TYPE NIL>")

      ;; 6. Records into marshal.
      (let ((literal (ub8 9)) (from (ub8 9)))
        (marshal rec-ihdr literal :width 640 :height 480 :color-type :rgba)
        (marshal rec-ihdr from (:from (make-rec-ihdr :width 640 :height 480 :color-type :rgba)))
        (check "rec-ihdr, a record writes the same bytes as literal values" from literal))
      (let ((r (make-rec-ihdr :width 1 :height 1)) (buf (ub8 9)))
        (setf (rec-ihdr-height r) 2)
        (marshal rec-ihdr buf (:from r))
        (check "rec-ihdr, a field edited through its accessor, and a missing one defaulted"
               (unmarshal rec-ihdr buf)
               '(:width 1 :height 2 :color-type :grayscale)))
      (let ((literal (ub8 21)) (from (ub8 21)))
        (marshal rec-chunk literal :body (:width 7 :height 9 :color-type :rgb))
        (marshal rec-chunk from (:from (make-rec-chunk-record
                                        :length 999 :crc 0
                                        :body (make-rec-ihdr :width 7 :height 9 :color-type :rgb))))
        (check "rec-chunk, a record holding a record; length and CRC recomputed" from literal))
      (let ((literal (ub8 10)) (from (ub8 10)))
        (marshal rec-parent literal :a 5 :child (:a -2 :b #x0102 :c -300 :d 7))
        (marshal rec-parent from (:from (make-rec-parent :a 5 :child (list :a -2 :b #x0102
                                                                            :c -300 :d 7))))
        (check "rec-parent, a record holding a plist for a child without a record type"
               from literal))
      (check "rec-masque, masque groups from a record"
             (to-list (lambda (d) (marshal rec-masque d (:from (make-rec-masque :mode :rgba :delta -3
                                                                                :tail 7)))))
             '(6 #xFD 7))

      ;; 7. Redefining a manifest with different fields redefines its class.
      (check "redefining a record manifest with a new field gives the class the new slot"
             (progn
               (eval '(defmanifest rec-redefined (:unit 8 :endian :big :record t)
                       (:a :u8 :default 0)))
               (eval '(defmanifest rec-redefined (:unit 8 :endian :big :record t)
                       (:a :u8 :default 0)
                       (:b :u8 :default 0)))
               (let ((r (funcall (find-symbol "MAKE-REC-REDEFINED" :specops) :a 1 :b 2)))
                 (list (field-value r :a) (field-value r :b)
                       (to-list (lambda (d) (funcall (compile nil `(lambda (d)
                                                                     (marshal rec-redefined d
                                                                       (:from ,r))))
                                                     d))))))
             '(1 2 (1 2)))

      ;; 8. Records out of unmarshal.
      (let ((buf (ub8 21)))
        (marshal rec-chunk buf :body (:width 7 :height 9 :color-type :rgb))
        (let ((r (unmarshal rec-chunk buf (:as :record))))
          (check "rec-chunk, read as a record holding a record"
                 (list (class-name (class-of r)) (rec-chunk-record-length r)
                       (class-name (class-of (rec-chunk-record-body r)))
                       (rec-ihdr-width (rec-chunk-record-body r))
                       (rec-ihdr-color-type (rec-chunk-record-body r))
                       (= (rec-chunk-record-crc r) (crc buf 4 17)))
                 '(rec-chunk-record 9 rec-ihdr 7 :rgb t))
          (check "rec-chunk, without :as the same bytes still read as a plist"
                 (unmarshal rec-chunk buf)
                 (list :length 9 :body '(:width 7 :height 9 :color-type :rgb)
                       :crc (rec-chunk-record-crc r)))
          (check "rec-chunk, a record read and written back reproduces the bytes"
                 (let ((again (ub8 21)))
                   (marshal rec-chunk again (:from r))
                   again)
                 buf)
          (check "rec-chunk, a record edited and written back gets a new CRC, which checks"
                 (let ((again (ub8 21)))
                   (setf (rec-ihdr-height (rec-chunk-record-body r)) 10)
                   (marshal rec-chunk again (:from r))
                   (let ((r2 (unmarshal rec-chunk again (:as :record))))
                     (list (rec-ihdr-height (rec-chunk-record-body r2))
                           (= (rec-chunk-record-crc r2) (crc again 4 17))
                           (/= (rec-chunk-record-crc r2) (rec-chunk-record-crc r)))))
                 '(10 t t))))
      (let ((buf (ub8 10)))
        (marshal rec-parent buf :a 5 :child (:a -2 :b #x0102 :c -300 :d 7))
        (check "rec-parent, read as a record holding a plist for a child without a record type"
               (let ((r (unmarshal rec-parent buf (:as :record))))
                 (list (rec-parent-a r) (rec-parent-child r)))
               '(5 (:a -2 :b #x0102 :c -300 :d 7))))
      (check "rec-masque, masque groups read into a record"
             (let ((r (unmarshal rec-masque (->bytes #(6 #xFD 7)) (:as :record))))
               (list (rec-masque-mode r) (rec-masque-delta r) (rec-masque-tail r)))
             '(:rgba -3 7))
      (check "unmarshal options: :as :record with keys, without a record class, an unknown :as or option"
             (mapcar (lambda (form) (signals-error-p (lambda () (macroexpand-1 form))))
                     '((unmarshal rec-ihdr s (:as :record) :width)
                       (unmarshal unmarshal-probe s (:as :record))
                       (unmarshal rec-ihdr s (:as :struct))
                       (unmarshal rec-ihdr s (:format :record))))
             '(t t t t))

      ;; 9. Round trips of whole files and records.
      (let ((png (make-1x1-png)))
        (check "the 1x1 PNG, read as a plist and written back, is reproduced"
               (let ((buf (ub8 72)))
                 (marshal png-file buf (:from (unmarshal png-file png)))
                 buf)
               png)
        (check "the 1x1 PNG, read as records and written back, is reproduced"
               (let ((buf (ub8 72)) (r (unmarshal png-file png (:as :record))))
                 (marshal png-file buf (:from r))
                 (list (class-name (class-of r))
                       (class-name (class-of (png-ihdr-chunk-png-ihdr (png-file-ihdr r))))
                       (equalp buf png)))
               '(png-file png-ihdr t))
        (check "the 1x1 PNG, IHDR edited to Adam7 interlacing: its CRC is recomputed and verifies"
               (let ((buf (ub8 72)) (r (unmarshal png-file png (:as :record))))
                 (setf (png-ihdr-interlace (png-ihdr-chunk-png-ihdr (png-file-ihdr r))) :adam7)
                 (marshal png-file buf (:from r))
                 (list (aref buf 28)
                       (/= (rd-u32-be buf 29) (rd-u32-be png 29))
                       (let ((*standard-output* (make-broadcast-stream))) (verify-png buf))))
               '(1 t t))
        (check "the 1x1 PNG, IDAT edited to a green pixel, matches a PNG built green"
               (let ((buf (ub8 72)) (r (unmarshal png-file png (:as :record))))
                 (setf (png-idat-chunk-data (png-file-idat r)) (zlib-store (->bytes #(0 0 255 0))))
                 (marshal png-file buf (:from r))
                 buf)
               (make-1x1-png :r 0 :g 255 :b 0)))
      (let ((goff (ub8 80)))
        (marshal goff-end-probe goff :t-tag #x40 :entry-flag 1 :amode 2
                                     :record-count 3 :entry-esdid 5 :entry-offset #x1000)
        (check "goff-end-probe, an 80-byte GOFF END record, read and written back, is reproduced"
               (let ((buf (ub8 80)))
                 (marshal goff-end-probe buf (:from (unmarshal goff-end-probe goff)))
                 buf)
               goff))

      ;; 10. Sources that are not canonical are written back canonical, not reproduced.
      (check "pad-probe, pad bytes other than the fill value are rewritten"
             (let ((buf (ub8 13)))
               (marshal pad-probe buf (:from (unmarshal pad-probe
                                                        (->bytes #(1 7 #xBB #xBB #x11 5 5 5 5 5 5 5 #x22)))))
               (coerce buf 'list))
             '(1 7 #xAA #xAA #x11 0 0 0 0 0 0 0 #x22))
      (check "bounded-probe, bytes after the terminator are rewritten as terminators"
             (let ((rec (unmarshal bounded-probe (->bytes #(97 0 122 122 122 9)))))
               (to-list (lambda (d) (marshal bounded-probe d (:from rec)))))
             '(97 0 0 0 0 9))
      (check "case-unnamed-probe, an unnamed field is rewritten with its default"
             (let ((rec (unmarshal case-unnamed-probe (->bytes #(1 0 0 0 7 9)))))
               (to-list (lambda (d) (marshal case-unnamed-probe d (:from rec)))))
             '(1 0 0 0 0 9)))
    (let ((pass (every #'identity results)))
      (format t "~&record checks ~:[FAILED~;PASSED~] (~a of ~a).~%"
              pass (count t results) (length results))
      pass)))

;;; ===========================================================================
;;; Part R — variable structure: sub-manifests chosen by :case, and repeated
;;; ===========================================================================

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

(defun run-variable-demo ()
  "Marshal and unmarshal sub-manifests chosen by :case, and repeated sub-manifests.
Returns T if all checks pass."
  (let ((results nil))
    (flet ((check (label got want)
             (let ((ok (equalp got want)))
               (push ok results)
               (format t "~&  ~:[FAIL~;ok  ~]  ~a~%" ok label)
               (unless ok
                 (format t "~&          got  ~s~%          want ~s~%" got want)))))
      (format t "~&variable structure checks:~%")

      ;; 1. A :case field whose branch is a sub-manifest.
      (let* ((png (make-1x1-png))
             (ihdr (subseq png 8 33)) (idat (subseq png 33 60)) (iend (subseq png 60 72)))
        (check "png-chunk, an IHDR chunk written with a constant selector and nested values"
               (let ((buf (ub8 25)))
                 (marshal png-chunk buf :type :ihdr :data (:width 1 :height 1 :color-type :rgb))
                 buf)
               ihdr)
        (check "png-chunk, an IDAT chunk takes the raw branch"
               (let ((buf (ub8 27)))
                 (marshal png-chunk buf :type :idat :data (subseq idat 8 23))
                 buf)
               idat)
        (check "png-chunk, the IHDR chunk read back: its data read by the sub-manifest"
               (unmarshal png-chunk ihdr)
               (list :length 13 :type :ihdr
                     :data '(:width 1 :height 1 :bit-depth 8 :color-type :rgb
                             :compression 0 :filter 0 :interlace :none)
                     :crc (rd-u32-be ihdr 21)))
        (check "png-chunk, the IDAT and IEND chunks read back as raw data"
               (list (getf (unmarshal png-chunk idat) :data)
                     (unmarshal png-chunk iend))
               (list (subseq idat 8 23)
                     (list :length 0 :type :iend :data #() :crc (rd-u32-be iend 8))))
        (check "png-chunk, a selector known only at run time chooses the sub-manifest"
               (let ((buf (ub8 25)) (type :ihdr)
                     (data (list :width 1 :height 1 :color-type :rgb)))
                 (marshal png-chunk buf :type type :data data)
                 buf)
               ihdr)
        (check "png-chunk, read as a record holding a png-ihdr record, and written back"
               (let* ((r (unmarshal png-chunk ihdr (:as :record))) (buf (ub8 25)))
                 (marshal png-chunk buf (:from r))
                 (list (class-name (class-of (png-chunk-data r)))
                       (png-ihdr-width (png-chunk-data r))
                       (equalp buf ihdr)))
               '(png-ihdr 1 t)))

      ;; 2. A chunk type the enum lacks: raw data, and its code read and written back.
      (let ((text (ub8 21)))
        ;; tEXt, "Title" NUL "png": a PNG ancillary chunk
        (marshal png-chunk text :type #x74455874
                                :data (->bytes (map 'vector #'char-code
                                                    (concatenate 'string "Title" (string (code-char 0))
                                                                 "png"))))
        (let ((plist (unmarshal png-chunk text)))
          (check "png-chunk, a tEXt chunk reads with its type's code and raw data"
                 (list (getf plist :length) (getf plist :type) (length (getf plist :data))
                       (= (getf plist :crc) (crc text 4 17)))
                 '(9 #x74455874 9 t))
          (check "png-chunk, the tEXt chunk read and written back"
                 (let ((buf (ub8 21)))
                   (marshal png-chunk buf (:from plist))
                   buf)
                 text)))

      ;; 3. Nested values for a branch that is not a sub-manifest.
      (check "png-chunk, nested values with a constant selector choosing raw data: an expansion error"
             (signals-error-p (lambda () (macroexpand-1 '(marshal png-chunk d :type :idat
                                                          :data (:width 1)))))
             t))
    (let ((pass (every #'identity results)))
      (format t "~&variable structure checks ~:[FAILED~;PASSED~] (~a of ~a).~%"
              pass (count t results) (length results))
      pass)))

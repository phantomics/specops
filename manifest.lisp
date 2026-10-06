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
    (str "IHDR")
    (manifest :png-ihdr png-ihdr))
  (:crc (:u 4) :default 0 :slot (:checksum-of :type+data :by #'crc)))

;; IDAT chunk: [length][type "IDAT"][opaque data][crc over type+data].
(defmanifest png-idat-chunk (:unit 8 :endian :big)
  (:length (:u 4) :slot (:length-of :data))
  (span :type+data
    (str "IDAT")
    (:data (:u 1 :vec)))
  (:crc (:u 4) :default 0 :slot (:checksum-of :type+data :by #'crc)))

;; IEND chunk: [length=0][type "IEND"][empty data][crc over type].
(defmanifest png-iend-chunk (:unit 8 :endian :big)
  (:length (:u 4) :slot (:length-of :data))
  (span :type+data
    (str "IEND")
    (:data (:u 1 :vec)))
  (:crc (:u 4) :default 0 :slot (:checksum-of :type+data :by #'crc)))

;; Whole file: signature + chunks (each a sub-manifest instance).
(defmanifest png-file (:unit 8 :endian :big)
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
               '(7))))
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

      ;; 5. IHDR chunk: span containing a sub-manifest.
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
               '(:mode :rgba :delta -3 :tail 7))))
    (let ((pass (every #'identity results)))
      (format t "~&masque checks ~:[FAILED~;PASSED~] (~a of ~a).~%"
              pass (count t results) (length results))
      pass)))

;;; ===========================================================================
;;; Part G — constant strings: chunk tags, codecs, terminators
;;; ===========================================================================

;; Constant string encoded in EBCDIC (code page 037).
(defmanifest ebcdic-probe (:unit 8 :endian :big)
  (str "HELLO" :codec #'specops/format.ebcdic:ebcdic-code-cp037)
  (:n :u8 :default 0))

;; Constant string with a terminator byte.
(defmanifest terminated-probe (:unit 8 :endian :big)
  (str "AB" :end-by 0)
  (:n :u8 :default 0))

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
                 t))))
    (let ((pass (every #'identity results)))
      (format t "~&string checks ~:[FAILED~;PASSED~] (~a of ~a).~%"
              pass (count t results) (length results))
      pass)))

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
         :codec #'specops/format.ebcdic:ebcdic-code-cp037))

;; A numeric default is already a code, so it is not passed through the codec.
(defmanifest codec-default-probe (:unit 8 :endian :big)
  (:fill (:u 1 :vec) :count 3 :default 64
         :codec #'specops/format.ebcdic:ebcdic-code-cp037))

;; A constant string with a character the codec does not map (a Tab).
(defmanifest codec-str-probe (:unit 8 :endian :big)
  (str #.(coerce (list #\A #\Tab) 'string)
       :codec #'specops/format.ebcdic:ebcdic-code-cp037))

;; A NUL-terminated string: the terminator is a raw code.
(defmanifest cstring-probe (:unit 8 :endian :big)
  (:s     (:u 1 :vec) :end-by 0)
  (:after :u8 :default 0))

;; A character terminator is encoded with the field's codec ($ is #x5B in CP037).
(defmanifest ebcdic-terminated-probe (:unit 8 :endian :big)
  (:s     (:u 1 :vec) :end-by #\$ :codec #'specops/format.ebcdic:ebcdic-code-cp037)
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
  (:kind  :u8 :default 1 :enumerate-by case-size)
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
      (check "case-late-probe, a selector after its field is an expansion error on both sides"
             (list (signals-error-p (lambda () (macroexpand-1 '(marshal case-late-probe d :kind 0))))
                   (signals-error-p (lambda () (macroexpand-1 '(unmarshal case-late-probe s)))))
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
  (:s (:u 1 :vec) :count 3 :codec #'specops/format.ebcdic:ebcdic-code-cp037))

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
             '(:s #(#x4142 #x43))))
    (let ((pass (every #'identity results)))
      (format t "~&vector scan checks ~:[FAILED~;PASSED~] (~a of ~a).~%"
              pass (count t results) (length results))
      pass)))

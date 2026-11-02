;;;; test/test-manifest-basics.lisp — Manifest basics: scalars and byte orders, nested sub-
;;;; manifests, masques, constant strings and pads. Pure: no files.

(in-package #:specops-tests)

(in-suite manifest-basics)

;;; ============================================================
;;; Probe manifests and helpers
;;; ============================================================

;; The manifest's byte order applies to every field that gives none of its own.
(defmanifest little-endian-probe (:unit 8 :endian :little)
  (:a :u16 :default 0)                    ; little-endian, from the manifest
  (:b (:s 4) :default 0)                  ; little-endian, from the manifest
  (:c :u16 :default 0 :endian :big))

;; An enumerated field whose default is a keyword. png-color-type has no code 1.
(defmanifest enum-default-probe (:unit 8 :endian :big)
  (:color :u8 :default :rgba :enumerate-by png-color-type))

;; One more level of nesting, to check the sub-lexicon is passed down.
(defmanifest nest-probe2 (:unit 8 :endian :big)
  (:head :u8 :default 0)
  (manifest :inner nest-probe))

(defmanifest be-child-probe (:unit 8 :endian :big)
  (:x :u16 :default 0))

(defmanifest mixed-order-probe (:unit 8 :endian :big)
  (:a :u16 :default 0)
  (manifest :le le-child-probe)
  (:b :u16 :default 0))

(defmanifest mixed-order-probe2 (:unit 8 :endian :little)
  (:a :u16 :default 0)
  (manifest :be be-child-probe))

;; Static group of zero, so the masque's leading byte is 0.
(defmanifest masque-probe-zero (:unit 8 :endian :big)
  (:lead :u8 :default 0)
  (masque "h:pptt" (p #x00) (t :kind :u8 :default 0))
  (:tail :u8 :default 0))

;; An enumerated masque group whose default is a keyword.
(defmanifest masque-default-probe (:unit 8 :endian :big)
  (masque "h:aabb" (a :mode  :u8 :default :rgb :enumerate-by png-color-type)
                   (b :delta (:s 1) :default 0))
  (:tail :u8 :default 0))

;; A decimal masque with constant digits: 5 digits, so 17 bits, in 3 units.
(defmanifest decimal-masque-probe (:unit 8 :endian :big)
  (masque "d:1a2bb" (a :hi :u8 :default 0) (b :lo :u8 :default 0))
  (:tail :u8 :default 0))

;; An enumerated masque group selecting a :case field's branch.
(defmanifest masque-selector-probe (:unit 8 :endian :big)
  (masque "h:kkxx" (k :kind :u8 :default :grayscale :enumerate-by png-color-type) (x :x :u8 :default 0))
  (:v (:case :kind (:grayscale :u8) (:rgb :u16)) :default 0))

;; Constant string encoded in EBCDIC (code page 037).
(defmanifest ebcdic-probe (:unit 8 :endian :big)
  (str "HELLO" :codec #'specops.map.ebcdic:ebcdic-code-cp037)
  (:n :u8 :default 0))

;; A constant string in a manifest whose fields are all little-endian.
(defmanifest le-string-probe (:unit 8 :endian :little)
  (str "AB")
  (:x :u16 :default 0))

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

(defmanifest pad-length-outer (:unit 8 :endian :big)
  (manifest :rec pad-length)
  (:after :u8 :default 0))

;;; ============================================================
;;; Flat, fixed-width manifests
;;; ============================================================

;; Round trip a png-ihdr body with distinct field values.
(def-test png-ihdr-all-fields-in-manifest-order-enums-decoded ()
  "png-ihdr reads back every field in manifest order, enumerated fields decoded to keywords."
  (let ((buf (ub8 13)))
    (marshal png-ihdr buf :width 640 :height 480 :bit-depth 8
                          :color-type :rgba :interlace :adam7)
    (is (equalp '(:width 640 :height 480 :bit-depth 8 :color-type :rgba
                  :compression 0 :filter 0 :interlace :adam7)
                (unmarshal png-ihdr buf)))))

(def-test png-ihdr-height-alone-skips-width ()
  "png-ihdr, asked for :height alone, reads past :width and returns just that value."
  (let ((buf (ub8 13)))
    (marshal png-ihdr buf :width 640 :height 480 :bit-depth 8
                          :color-type :rgba :interlace :adam7)
    (is (equalp '(480)
                (multiple-value-list (unmarshal png-ihdr buf nil :height))))))

(def-test png-ihdr-interlace-width-out-of-order-returned-in-call-order ()
  "png-ihdr, :interlace :width (out of order, returned in call order)."
  (let ((buf (ub8 13)))
    (marshal png-ihdr buf :width 640 :height 480 :bit-depth 8
                          :color-type :rgba :interlace :adam7)
    (is (equalp '(:adam7 640)
                (multiple-value-list (unmarshal png-ihdr buf nil :interlace :width))))))

(def-test png-ihdr-color-type-alone-enum-decode-mid-record ()
  "png-ihdr, asked for :color-type alone, decodes an enumerated field from the middle of the
record."
  (let ((buf (ub8 13)))
    (marshal png-ihdr buf :width 640 :height 480 :bit-depth 8
                          :color-type :rgba :interlace :adam7)
    (is (equalp '(:rgba)
                (multiple-value-list (unmarshal png-ihdr buf nil :color-type))))))

;; Read the IHDR body out of the demo PNG (bytes 16..28).
(def-test ihdr-body-from-the-marshalled-1x1-png ()
  "The IHDR body of the 1x1 PNG built from manifests reads as a 1x1 RGB image."
  (let ((ihdr (subseq (make-1x1-png) 16 29)))
    (is (equalp '(1 1 :rgb)
                (multiple-value-list (unmarshal png-ihdr ihdr nil :width :height :color-type))))))

;; Signed values and per-field endianness.
(def-test marshalled-bytes-for-unmarshal-probe ()
  "unmarshal-probe writes signed big-endian, unsigned little-endian and signed little-endian
fields in two's complement, each in its own byte order."
  (let ((buf (ub8 9)))
    (marshal unmarshal-probe buf :a -2 :b #x0102 :c -300 :d 7)
    (is (equalp '(#xFF #xFE  #x02 #x01  #xD4 #xFE #xFF #xFF  7)
                (coerce buf 'list)))))

(def-test unmarshal-probe-all-fields-signed-be-unsigned-le-signed-le ()
  "unmarshal-probe reads signed big-endian, unsigned little-endian and signed little-endian
fields back to the values written, signs included."
  (let ((buf (ub8 9)))
    (marshal unmarshal-probe buf :a -2 :b #x0102 :c -300 :d 7)
    (is (equalp '(:a -2 :b #x0102 :c -300 :d 7)
                (unmarshal unmarshal-probe buf)))))

(def-test unmarshal-probe-d-alone-cursor-passes-three-fields ()
  "unmarshal-probe, asked for :d alone, passes over the three fields before it."
  (let ((buf (ub8 9)))
    (marshal unmarshal-probe buf :a -2 :b #x0102 :c -300 :d 7)
    (is (equalp '(7)
                (multiple-value-list (unmarshal unmarshal-probe buf nil :d))))))

;; A byte order given once, by the manifest.
(def-test little-endian-probe-fields-follow-the-manifest-s-endian-unless ()
  "little-endian-probe, fields follow the manifest's :endian unless they give one."
  (let ((buf (ub8 8)))
    (marshal little-endian-probe buf :a #x0102 :b -300 :c #x0304)
    (is (equalp '(#x02 #x01  #xD4 #xFE #xFF #xFF  #x03 #x04)
                (coerce buf 'list)))))

(def-test little-endian-probe-read-back ()
  "little-endian-probe reads back the values it was written with."
  (let ((buf (ub8 8)))
    (marshal little-endian-probe buf :a #x0102 :b -300 :c #x0304)
    (is (equalp '(:a #x0102 :b -300 :c #x0304)
                (unmarshal little-endian-probe buf)))))

;; Enums: a keyword default, and a code the enum lacks.
(def-test enum-default-probe-a-keyword-default-written-as-its-code ()
  "enum-default-probe, a keyword default written as its code."
  (let ((buf (ub8 1)))
    (marshal enum-default-probe buf)
    (is (equalp '(6)
                (coerce buf 'list)))
    (is (equalp '(:color :rgba)
                (unmarshal enum-default-probe buf)))))

(def-test enum-default-probe-a-code-the-enum-lacks-reads-as-itself ()
  "enum-default-probe, a code the enum lacks reads as itself and writes back."
  (let ((buf (ub8 1)))
    (marshal enum-default-probe buf :color 1)
    (is (equalp '(:color 1)
                (unmarshal enum-default-probe (->bytes #(1)))))
    (is (equalp '(1)
                (coerce buf 'list)))))

;;; ============================================================
;;; Chunks: spans, vectors and sub-manifests
;;; ============================================================

;; IDAT: span, constant "IDAT" tag, :data counted by the :length field.
(def-test idat-all-fields ()
  "png-idat-chunk reads the 1x1 PNG's IDAT as its length, its compressed data and its stored
CRC."
  (let* ((png  (make-1x1-png))
         (blob (zlib-store (->bytes (vector 0 255 0 0))))
         (idat-chunk (subseq png 33 60)))
    (is (equalp (list :length 15 :data blob
                      :crc (rd-u32-be idat-chunk 23))
                (unmarshal png-idat-chunk idat-chunk)))))

(def-test idat-data-alone-count-taken-from-unrequested-length ()
  "png-idat-chunk, asked for :data alone, takes the data's count from the :length field it was
not asked for."
  (let* ((png  (make-1x1-png))
         (blob (zlib-store (->bytes (vector 0 255 0 0))))
         (idat-chunk (subseq png 33 60)))
    (is (equalp blob
                (unmarshal png-idat-chunk idat-chunk nil :data)))))

(def-test idat-data-length-out-of-order ()
  "IDAT, :data :length (out of order)."
  (let* ((png  (make-1x1-png))
         (blob (zlib-store (->bytes (vector 0 255 0 0))))
         (idat-chunk (subseq png 33 60)))
    (is (equalp (list blob 15)
                (multiple-value-list (unmarshal png-idat-chunk idat-chunk nil :data :length))))))

(def-test idat-stored-crc-matches-recomputed-crc-over-type-data ()
  "IDAT, stored :crc matches recomputed CRC over type+data."
  (let* ((png  (make-1x1-png))
         (idat-chunk (subseq png 33 60)))
    (is (equalp (crc idat-chunk 4 23)
                (unmarshal png-idat-chunk idat-chunk nil :crc)))))

;; IEND: zero-length :data.
(def-test iend-all-fields-empty-data ()
  "png-iend-chunk reads the IEND chunk as a zero length, empty data and its CRC."
  (let* ((png  (make-1x1-png))
         (iend-chunk (subseq png 60 72)))
    (is (equalp (list :length 0 :data #()
                      :crc (rd-u32-be iend-chunk 8))
                (unmarshal png-iend-chunk iend-chunk)))))

;; Sub-manifest between scalars.
(def-test nest-probe-all-fields-nested-plist ()
  "nest-probe reads a sub-manifest between two scalars back as a nested plist."
  (let ((buf (ub8 15)))
    (marshal nest-probe buf :marker 238
                            :ihdr (:width 640 :height 480 :bit-depth 8
                                   :color-type :rgba :interlace :adam7)
                            :trailer 170)
    (is (equalp '(:marker 238
                  :ihdr (:width 640 :height 480 :bit-depth 8 :color-type :rgba
                         :compression 0 :filter 0 :interlace :adam7)
                  :trailer 170)
                (unmarshal nest-probe buf)))))

(def-test nest-probe-trailer-alone-cursor-passes-the-sub-manifest ()
  "nest-probe, asked for :trailer alone, passes over the sub-manifest before it."
  (let ((buf (ub8 15)))
    (marshal nest-probe buf :marker 238
                            :ihdr (:width 640 :height 480 :bit-depth 8
                                   :color-type :rgba :interlace :adam7)
                            :trailer 170)
    (is (equalp 170
                (unmarshal nest-probe buf nil :trailer)))))

;; Two levels of nesting.
(def-test nest-probe2-two-levels ()
  "nest-probe2 writes and reads sub-manifests two levels deep, the innermost one's defaults
filled in."
  (let ((buf (ub8 16)))
    (marshal nest-probe2 buf :head 9
                             :inner (:marker 238
                                     :ihdr (:width 1 :height 2 :color-type :rgb)
                                     :trailer 170))
    (is (equalp '(:head 9
                  :inner (:marker 238
                          :ihdr (:width 1 :height 2 :bit-depth 8 :color-type :rgb
                                 :compression 0 :filter 0 :interlace :none)
                          :trailer 170))
                (unmarshal nest-probe2 buf)))))

;; Children with a byte order of their own.
(def-test mixed-order-probe-a-little-endian-child-inside-a-big-endian ()
  "mixed-order-probe, a little-endian child inside a big-endian parent."
  (let ((buf (ub8 6)))
    (marshal mixed-order-probe buf :a #x0102 :le (:x #x0304) :b #x0506)
    (is (equalp '(#x01 #x02 #x04 #x03 #x05 #x06)
                (coerce buf 'list)))))

(def-test mixed-order-probe-read-back ()
  "mixed-order-probe reads a little-endian child inside a big-endian parent back to the values
written."
  (let ((buf (ub8 6)))
    (marshal mixed-order-probe buf :a #x0102 :le (:x #x0304) :b #x0506)
    (is (equalp '(:a #x0102 :le (:x #x0304) :b #x0506)
                (unmarshal mixed-order-probe buf)))))

(def-test mixed-order-probe2-a-big-endian-child-inside-a-little-endian ()
  "mixed-order-probe2, a big-endian child inside a little-endian parent."
  (let ((buf (ub8 4)))
    (marshal mixed-order-probe2 buf :a #x0102 :be (:x #x0304))
    (is (equalp '(#x02 #x01 #x03 #x04)
                (coerce buf 'list)))))

(def-test mixed-order-probe2-read-back ()
  "mixed-order-probe2 reads a big-endian child inside a little-endian parent back to the values
written."
  (let ((buf (ub8 4)))
    (marshal mixed-order-probe2 buf :a #x0102 :be (:x #x0304))
    (is (equalp '(:a #x0102 :be (:x #x0304))
                (unmarshal mixed-order-probe2 buf)))))

;; IHDR chunk: span containing a sub-manifest.
(def-test ihdr-chunk-all-fields-span-sub-manifest ()
  "png-ihdr-chunk reads the 1x1 PNG's IHDR chunk: a length over a span, the body through a
sub-manifest, and the CRC."
  (let* ((png  (make-1x1-png))
         (ihdr-chunk (subseq png 8 33)))
    (is (equalp (list :length 13
                      :png-ihdr '(:width 1 :height 1 :bit-depth 8 :color-type :rgb
                                  :compression 0 :filter 0 :interlace :none)
                      :crc (rd-u32-be ihdr-chunk 21))
                (unmarshal png-ihdr-chunk ihdr-chunk)))))

;;; ============================================================
;;; Masque fields
;;; ============================================================

;; Static and keyword groups.
(def-test masque-probe-marshalled-bytes ()
  "masque-probe writes its two-byte masque, the static group then :kind, between two scalars."
  (let ((buf (ub8 4)))
    (marshal masque-probe buf :lead 1 :kind 5 :tail 9)
    (is (equalp '(1 3 5 9)
                (coerce buf 'list)))))

(def-test masque-probe-all-fields ()
  "masque-probe reads back the values it was written with, the masque's keyword group as a
field."
  (let ((buf (ub8 4)))
    (marshal masque-probe buf :lead 1 :kind 5 :tail 9)
    (is (equalp '(:lead 1 :kind 5 :tail 9)
                (unmarshal masque-probe buf)))))

(def-test masque-probe-tail-alone-cursor-passes-the-masque ()
  "masque-probe, asked for :tail alone, passes over the masque before it."
  (let ((buf (ub8 4)))
    (marshal masque-probe buf :lead 1 :kind 5 :tail 9)
    (is (equalp 9
                (unmarshal masque-probe buf nil :tail)))))

(def-test masque-probe-kind-alone ()
  "masque-probe, asked for :kind alone, extracts that group from the masque."
  (let ((buf (ub8 4)))
    (marshal masque-probe buf :lead 1 :kind 5 :tail 9)
    (is (equalp 5
                (unmarshal masque-probe buf nil :kind)))))

(def-test masque-probe-wrong-static-group-signals-an-error ()
  "masque-probe, wrong static group signals an error."
  (let ((buf (ub8 4)))
    (marshal masque-probe buf :lead 1 :kind 5 :tail 9)
    (let ((bad (copy-seq buf)))
      (setf (aref bad 1) #x04)
      (is (equalp :error
                  (handler-case (progn (unmarshal masque-probe bad) :no-error)
                    (error () :error)))))))

;; Keyword group omitted: its default is written.
(def-test masque-probe-omitted-kind-uses-its-default ()
  "masque-probe, omitted :kind uses its default."
  (let ((buf (ub8 4)))
    (marshal masque-probe buf :lead 1 :tail 9)
    (is (equalp '(1 3 0 9)
                (coerce buf 'list)))))

;; Leading zero byte inside the masque.
(def-test masque-probe-zero-marshalled-bytes-leading-zero-kept ()
  "masque-probe-zero writes the zero static group as a leading zero byte, over what the buffer
held."
  (let ((buf (make-array 4 :element-type '(unsigned-byte 8) :initial-element #xEE)))
    (marshal masque-probe-zero buf :lead 1 :kind 0 :tail 9)
    (is (equalp '(1 0 0 9)
                (coerce buf 'list)))))

(def-test masque-probe-zero-all-fields ()
  "masque-probe-zero reads back a masque whose leading byte is zero."
  (let ((buf (make-array 4 :element-type '(unsigned-byte 8) :initial-element #xEE)))
    (marshal masque-probe-zero buf :lead 1 :kind 0 :tail 9)
    (is (equalp '(:lead 1 :kind 0 :tail 9)
                (unmarshal masque-probe-zero buf)))))

;; Enum and signed groups.
(def-test masque-probe-typed-marshalled-bytes-enum-6-3-as-xfd ()
  "masque-probe-typed writes an enumerated group as its code and a signed group in two's
complement."
  (let ((buf (ub8 3)))
    (marshal masque-probe-typed buf :mode :rgba :delta -3 :tail 7)
    (is (equalp '(6 #xFD 7)
                (coerce buf 'list)))))

(def-test masque-probe-typed-all-fields-enum-decoded-sign-extended ()
  "masque-probe-typed reads an enumerated group back as its keyword and a signed group
sign-extended."
  (let ((buf (ub8 3)))
    (marshal masque-probe-typed buf :mode :rgba :delta -3 :tail 7)
    (is (equalp '(:mode :rgba :delta -3 :tail 7)
                (unmarshal masque-probe-typed buf)))))

;; A keyword default in a masque group.
(def-test masque-default-probe-the-default-rgb-written-as-2-and-read-back ()
  "masque-default-probe, the default :rgb written as 2, and read back."
  (let ((buf (ub8 3)))
    (marshal masque-default-probe buf :tail 7)
    (is (equalp '(2 0 7)
                (coerce buf 'list)))
    (is (equalp '(:mode :rgb :delta 0 :tail 7)
                (unmarshal masque-default-probe buf)))))

;; Decimal masques in manifests.
(def-test decimal-masque-probe-17245-in-3-units-read-back-and-a-constant ()
  "decimal-masque-probe: 17245 in 3 units, read back, and a constant digit that differs."
  (let ((buf (ub8 4)))
    (marshal decimal-masque-probe buf :hi 7 :lo 45 :tail 9)
    (is (equalp '(0 #x43 #x5D 9)
                (coerce buf 'list)))
    (is (equalp '(:hi 7 :lo 45 :tail 9)
                (unmarshal decimal-masque-probe buf)))
    (is (equalp '(:constant)
                (mapcar #'first (failed-checks (lambda () (unmarshal decimal-masque-probe
                                                                     (->bytes #(1 #x7B #xDD 9))))))))))

;; Values too wide for their groups.
(def-test a-group-value-too-wide-is-an-error-on-write-a-negative-one-fits ()
  "A group value too wide is an error on write; a negative one fits as two's complement."
  (let ((k 300))
    (signals error (marshal masque-probe (ub8 4) :lead 1 :kind k :tail 9)))
  (let ((lo 123))
    (signals error (marshal decimal-masque-probe (ub8 4) :hi 1 :lo lo)))
  (is (equalp '(1 3 255 9)
              (let ((k -1) (buf (ub8 4))) (marshal masque-probe buf :lead 1 :kind k :tail 9) (coerce buf 'list)))))

;; A masque group as a :case selector.
(def-test masque-selector-probe-an-enumerated-group-selects-the-branch ()
  "masque-selector-probe: an enumerated group selects the branch, written and read, in full and
selectively."
  (is (equalp '(2 0 2 1)
              (let ((k :rgb) (buf (ub8 4))) (marshal masque-selector-probe buf :kind k :v 513) (coerce buf 'list))))
  (is (equalp '(0 0 5)
              (let ((buf (ub8 3))) (marshal masque-selector-probe buf :v 5) (coerce buf 'list))))
  (is (equalp '(:kind :rgb :x 0 :v 513)
              (unmarshal masque-selector-probe (->bytes #(2 0 2 1)))))
  (is (equalp 513
              (unmarshal masque-selector-probe (->bytes #(2 0 2 1)) nil :v))))

;;; ============================================================
;;; Constant strings: chunk tags, codecs and terminators
;;; ============================================================

;; Chunk tags in the marshalled PNG.
(def-test ihdr-idat-iend-tags-written-by-constant-str-fields ()
  "The IHDR, IDAT and IEND tags of the 1x1 PNG are written by constant str fields."
  (let ((png (make-1x1-png)))
    (is (equalp "IHDR"
                (map 'string #'code-char (subseq png 12 16))))
    (is (equalp "IDAT"
                (map 'string #'code-char (subseq png 37 41))))
    (is (equalp "IEND"
                (map 'string #'code-char (subseq png 64 68))))))

(def-test idat-with-a-corrupted-tag-signals-an-error ()
  "IDAT with a corrupted tag signals an error."
  (let* ((png  (make-1x1-png))
         (idat (subseq png 33 60)))
    (let ((bad (copy-seq idat)))
      (setf (aref bad 7) (char-code #\X))           ; "IDAT" -> "IDAX"
      (signals error
        (unmarshal png-idat-chunk bad)))))

(def-test png-file-whole-file-read-back ()
  "png-file reads the whole 1x1 PNG: the signature and each chunk's fields."
  (let* ((png  (make-1x1-png)))
    (is (equalp (list :signature +png-signature+
                      :ihdr (list :length 13
                                  :png-ihdr '(:width 1 :height 1 :bit-depth 8 :color-type :rgb
                                              :compression 0 :filter 0 :interlace :none)
                                  :crc (rd-u32-be png 29))
                      :idat (list :length 15 :data (subseq png 41 56) :crc (rd-u32-be png 56))
                      :iend (list :length 0 :data #() :crc (rd-u32-be png 68)))
                (unmarshal png-file png)))))

;; EBCDIC codec.
(def-test ebcdic-probe-hello-written-in-code-page-037 ()
  "ebcdic-probe, \"HELLO\" written in code page 037."
  (let ((buf (ub8 6)))
    (marshal ebcdic-probe buf :n 7)
    (is (equalp '(#xC8 #xC5 #xD3 #xD3 #xD6 7)
                (coerce buf 'list)))))

(def-test ebcdic-probe-read-back-string-validated ()
  "ebcdic-probe reads past its constant EBCDIC string, which matches, to the field after it."
  (let ((buf (ub8 6)))
    (marshal ebcdic-probe buf :n 7)
    (is (equalp '(:n 7)
                (unmarshal ebcdic-probe buf)))))

(def-test ebcdic-probe-ascii-byte-in-place-of-ebcdic-signals-an-error ()
  "ebcdic-probe, ASCII byte in place of EBCDIC signals an error."
  (let ((buf (ub8 6)))
    (marshal ebcdic-probe buf :n 7)
    (let ((bad (copy-seq buf)))
      (setf (aref bad 0) (char-code #\H))           ; ASCII H, not EBCDIC
      (signals error
        (unmarshal ebcdic-probe bad)))))

;; Terminator.
(def-test terminated-probe-bytes-include-the-terminator ()
  "terminated-probe, bytes include the terminator."
  (let ((buf (ub8 4)))
    (marshal terminated-probe buf :n 9)
    (is (equalp '(#x41 #x42 0 9)
                (coerce buf 'list)))))

(def-test terminated-probe-read-back-cursor-passes-the-terminator ()
  "terminated-probe reads past its constant string and terminator to the field after it."
  (let ((buf (ub8 4)))
    (marshal terminated-probe buf :n 9)
    (is (equalp '(:n 9)
                (unmarshal terminated-probe buf)))))

(def-test terminated-probe-missing-terminator-signals-an-error ()
  "terminated-probe, missing terminator signals an error."
  (let ((buf (ub8 4)))
    (marshal terminated-probe buf :n 9)
    (let ((bad (copy-seq buf)))
      (setf (aref bad 2) 1)
      (signals error
        (unmarshal terminated-probe bad)))))

;; A string where no field is big-endian.
(def-test le-string-probe-the-string-in-stream-order-the-field-little ()
  "le-string-probe, the string in stream order, the field little-endian."
  (let ((buf (ub8 4)))
    (marshal le-string-probe buf :x #x0102)
    (is (equalp '(#x41 #x42 #x02 #x01)
                (coerce buf 'list)))))

(def-test le-string-probe-read-back ()
  "le-string-probe reads back the little-endian field after its constant string."
  (let ((buf (ub8 4)))
    (marshal le-string-probe buf :x #x0102)
    (is (equalp '(:x #x0102)
                (unmarshal le-string-probe buf)))))

;;; ============================================================
;;; Padding: counts, :to, :align and :length
;;; ============================================================

;; :align and :to depend on the length of the preceding vector.
(def-test pad-probe-1-byte-data-2-align-bytes-to-12-fills-7 ()
  "pad-probe, 1-byte data: 2 align bytes, :to 12 fills 7."
  (let ((buf (ub8 13)))
    (marshal pad-probe buf :len 1 :data #(7) :x #x11 :y #x22)
    (is (equalp '(1 7 #xAA #xAA #x11 0 0 0 0 0 0 0 #x22)
                (coerce buf 'list)))))

(def-test pad-probe-1-byte-data-read-back ()
  "pad-probe reads back past both pads when one byte of data leaves two to align."
  (let ((buf (ub8 13)))
    (marshal pad-probe buf :len 1 :data #(7) :x #x11 :y #x22)
    (is (equalp '(:len 1 :data #(7) :x #x11 :y #x22)
                (unmarshal pad-probe buf)))))

(def-test pad-probe-3-byte-data-already-aligned-no-align-bytes ()
  "pad-probe, 3-byte data: already aligned, no align bytes."
  (let ((buf (ub8 13)))
    (marshal pad-probe buf :len 3 :data #(7 8 9) :x #x11 :y #x22)
    (is (equalp '(3 7 8 9 #x11 0 0 0 0 0 0 0 #x22)
                (coerce buf 'list)))))

(def-test pad-probe-3-byte-data-read-back ()
  "pad-probe reads back past both pads when three bytes of data need no alignment."
  (let ((buf (ub8 13)))
    (marshal pad-probe buf :len 3 :data #(7 8 9) :x #x11 :y #x22)
    (is (equalp '(:len 3 :data #(7 8 9) :x #x11 :y #x22)
                (unmarshal pad-probe buf)))))

(def-test pad-probe-y-alone-cursor-passes-both-pads ()
  "pad-probe, asked for :y alone, passes over both pads before it."
  (let ((buf (ub8 13)))
    (marshal pad-probe buf :len 3 :data #(7 8 9) :x #x11 :y #x22)
    (is (equalp #x22
                (unmarshal pad-probe buf nil :y)))))

;; :to past its target signals an error in both directions.
(def-test pad-probe-to-overrun-on-marshal-signals-an-error ()
  "pad-probe, :to overrun on marshal signals an error."
  (signals error
    (marshal pad-probe (ub8 32) :len 9 :data #(1 2 3 4 5 6 7 8 9)
                                :x 1 :y 2)))

(def-test pad-probe-to-overrun-on-unmarshal-signals-an-error ()
  "pad-probe, :to overrun on unmarshal signals an error."
  (let ((bad (ub8 16)))
    (replace bad '(9 1 2 3 4 5 6 7 8 9 #xAA #xAA #x11))
    (signals error
      (unmarshal pad-probe bad))))

;; :to inside a sub-manifest is relative to the child's start.
(def-test pad-outer-child-to-6-counts-from-the-child-byte-2 ()
  "pad-outer, child :to 6 counts from the child (byte 2)."
  (let ((buf (ub8 10)))
    (marshal pad-outer buf :h1 1 :h2 2 :inner (:a 3 :b 4) :tail 5)
    (is (equalp '(1 2 3 0 0 0 0 0 4 5)
                (coerce buf 'list)))))

(def-test pad-outer-read-back ()
  "pad-outer reads back past a :to pad that counts from the start of its sub-manifest."
  (let ((buf (ub8 10)))
    (marshal pad-outer buf :h1 1 :h2 2 :inner (:a 3 :b 4) :tail 5)
    (is (equalp '(:h1 1 :h2 2 :inner (:a 3 :b 4) :tail 5)
                (unmarshal pad-outer buf)))))

;; Trailing :length.
(def-test pad-length-marshal-stops-at-length-16 ()
  "pad-length, marshal stops at :length 16."
  (let* ((buf (make-array 20 :element-type '(unsigned-byte 8) :initial-element #xEE))
         (end (marshal pad-length buf :len 3 :data #(1 2 3))))
    (is (equalp 16
                end))))

(def-test pad-length-bytes-padded-to-16-rest-of-buffer-untouched ()
  "pad-length, bytes padded to 16, rest of buffer untouched."
  (let ((buf (make-array 20 :element-type '(unsigned-byte 8) :initial-element #xEE)))
    (marshal pad-length buf :len 3 :data #(1 2 3))
    (is (equalp '(3 1 2 3 0 0 0 0 0 0 0 0 0 0 0 0 #xEE #xEE #xEE #xEE)
                (coerce buf 'list)))))

(def-test pad-length-outer-field-after-a-padded-sub-manifest-lands-at-byte ()
  "pad-length-outer, field after a padded sub-manifest lands at byte 16."
  (let ((buf (ub8 17)))
    (marshal pad-length-outer buf :rec (:len 3 :data #(1 2 3)) :after 9)
    (is (equalp 9
                (aref buf 16)))))

(def-test pad-length-outer-read-back ()
  "pad-length-outer reads back the field after a sub-manifest padded out by :length."
  (let ((buf (ub8 17)))
    (marshal pad-length-outer buf :rec (:len 3 :data #(1 2 3)) :after 9)
    (is (equalp '(:rec (:len 3 :data #(1 2 3)) :after 9)
                (unmarshal pad-length-outer buf)))))

;; GOFF END record layout.
(def-test goff-end-probe-record-is-80-bytes ()
  "goff-end-probe writes an 80-byte record, ending at its :length."
  (let* ((buf (make-array 96 :element-type '(unsigned-byte 8) :initial-element #xEE))
         (end (marshal goff-end-probe buf :t-tag #x40 :entry-flag 1 :amode 2
                                          :record-count 7 :entry-esdid 5
                                          :entry-offset #x100)))
    (is (equalp 80
                end))))

(def-test goff-end-probe-first-24-bytes ()
  "goff-end-probe lays its fields out in the first 24 bytes as the GOFF END record does."
  (let ((buf (make-array 96 :element-type '(unsigned-byte 8) :initial-element #xEE)))
    (marshal goff-end-probe buf :t-tag #x40 :entry-flag 1 :amode 2
                                :record-count 7 :entry-esdid 5
                                :entry-offset #x100)
    (is (equalp '(#x03 #x40 0  1 2  0 0 0  0 0 0 7  0 0 0 5  0 0 0 0  0 0 1 0)
                (coerce (subseq buf 0 24) 'list)))))

(def-test goff-end-probe-bytes-24-79-are-zero ()
  "goff-end-probe fills the rest of its 80 bytes with its pad value, zero."
  (let ((buf (make-array 96 :element-type '(unsigned-byte 8) :initial-element #xEE)))
    (marshal goff-end-probe buf :t-tag #x40 :entry-flag 1 :amode 2
                                :record-count 7 :entry-esdid 5
                                :entry-offset #x100)
    (is-true (every #'zerop (subseq buf 24 80)))))

(def-test goff-end-probe-read-back ()
  "goff-end-probe reads back the values it was written with."
  (let ((buf (make-array 96 :element-type '(unsigned-byte 8) :initial-element #xEE)))
    (marshal goff-end-probe buf :t-tag #x40 :entry-flag 1 :amode 2
                                :record-count 7 :entry-esdid 5
                                :entry-offset #x100)
    (is (equalp '(:t-tag #x40 :entry-flag 1 :amode 2 :record-count 7
                  :entry-esdid 5 :entry-offset #x100)
                (unmarshal goff-end-probe buf)))))

;; A function destination produces the same bytes as a vector.
(def-test pad-probe-to-a-function-destination-matches-the-vector-bytes ()
  "pad-probe to a function destination matches the vector bytes."
  (let ((acc nil))
    (marshal pad-probe (lambda (b) (push b acc)) :len 1 :data #(7) :x #x11 :y #x22)
    (is (equalp '(1 7 #xAA #xAA #x11 0 0 0 0 0 0 0 #x22)
                (reverse acc)))))

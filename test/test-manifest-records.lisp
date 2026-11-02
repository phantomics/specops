;;;; test/test-manifest-records.lisp — Record classes, values taken from records, and round
;;;; trips through records. Pure: no files.

(in-package #:specops-tests)

(in-suite manifest-records)

;;; ============================================================
;;; Probe manifests and helpers
;;; ============================================================

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

;;; ============================================================
;;; Tests
;;; ============================================================

;; Flat records known only at run time.
(def-test png-ihdr-a-plist-record-writes-the-same-bytes-as-literal-values ()
  "png-ihdr, a plist record writes the same bytes as literal values."
  (let ((literal (ub8 13)))
    (marshal png-ihdr literal :width 640 :height 480 :bit-depth 8
                              :color-type :rgba :interlace :adam7)
    (is (equalp literal
                (write-ihdr-from (list :width 640 :height 480 :bit-depth 8
                                       :color-type :rgba :interlace :adam7))))))

(def-test png-ihdr-one-compiled-marshal-writes-two-different-records ()
  "png-ihdr, one compiled marshal writes two different records."
  (is (equalp '(0 0 0 1  0 0 0 1  8 0 0 0 0)
              (coerce (write-ihdr-from (list :width 1 :height 1)) 'list)))
  (is (equalp '(0 0 0 2  0 0 0 3  8 2 0 0 0)
              (coerce (write-ihdr-from (list :width 2 :height 3 :color-type :rgb)) 'list))))

(def-test png-ihdr-fields-the-record-lacks-take-their-defaults ()
  "png-ihdr, fields the record lacks take their defaults."
  (is (equalp '(:width 1 :height 1 :bit-depth 8 :color-type :grayscale
                :compression 0 :filter 0 :interlace :none)
              (unmarshal png-ihdr (write-ihdr-from (list :width 1 :height 1))))))

(def-test png-ihdr-enum-values-in-a-record-as-a-keyword-a-known-code ()
  "png-ihdr, enum values in a record as a keyword, a known code, and an unknown code."
  (is (equalp '(6 2 1)
              (mapcar (lambda (c) (aref (write-ihdr-from (list :color-type c)) 9))
                      '(:rgba 2 1)))))

(def-test png-ihdr-a-literal-pair-overrides-the-record ()
  "png-ihdr, a literal pair overrides the record."
  (is (equalp 600
              (let ((buf (ub8 13)) (rec (list :width 1 :height 1)))
                (marshal png-ihdr buf (:from rec) :height 600)
                (getf (unmarshal png-ihdr buf) :height)))))

(def-test record-required-probe-a-field-with-no-default-and-no-value ()
  "record-required-probe, a field with no default and no value in the record."
  (is (equalp '(7 1)
              (to-list (lambda (d) (marshal record-required-probe d (:from (list :a 7 :b 1)))))))
  (let ((rec (list :b 1)))
    (signals error
      (to-list (lambda (d) (marshal record-required-probe d (:from rec)))))))

(def-test an-unknown-marshal-option-is-an-expansion-error ()
  "An unknown marshal option is an expansion error."
  (signals error
    (macroexpand-1 '(marshal png-ihdr d (:form rec)))))

;; Nested records; slot fields are recomputed.
(def-test png-ihdr-chunk-a-nested-record-its-length-and-crc-recomputed-not ()
  "png-ihdr-chunk, a nested record; its length and CRC recomputed, not copied."
  (let ((literal (ub8 25))
        (from (ub8 25))
        (rec (list :length 999 :crc 12345
                   :png-ihdr (list :width 1 :height 1 :color-type :rgb))))
    (marshal png-ihdr-chunk literal :png-ihdr (:width 1 :height 1 :color-type :rgb))
    (marshal png-ihdr-chunk from (:from rec))
    (is (equalp literal
                from))))

(def-test png-ihdr-chunk-a-literal-pair-for-a-nested-field-overrides ()
  "png-ihdr-chunk, a literal pair for a nested field overrides the nested record."
  (let ((literal (ub8 25))
        (from (ub8 25))
        (rec (list :png-ihdr (list :width 1 :height 1 :color-type :rgb))))
    (marshal png-ihdr-chunk literal :png-ihdr (:width 1 :height 2 :color-type :rgb))
    (marshal png-ihdr-chunk from (:from rec) :png-ihdr (:height 2))
    (is (equalp literal
                from))))

;; Other field kinds from a record.
(def-test case-probe-a-case-selector-and-value-from-a-record ()
  "case-probe, a :case selector and value from a record."
  (is (equalp '(4 #xAC #x02 9)
              (let ((rec (list :kind 4 :v 300 :after 9)))
                (to-list (lambda (d) (marshal case-probe d (:from rec))))))))

(def-test case-enum-probe-an-enumerated-selector-from-a-record ()
  "case-enum-probe, an enumerated selector from a record, and its default."
  (is (equalp '(2 #xFF #xFF #xFF #xFE 9)
              (let ((rec (list :kind :long :v -2 :after 9)))
                (to-list (lambda (d) (marshal case-enum-probe d (:from rec)))))))
  (is (equalp '(1 1 2 9)
              (let ((rec (list :v #x102 :after 9)))
                (to-list (lambda (d) (marshal case-enum-probe d (:from rec))))))))

(def-test png-idat-chunk-a-vector-from-a-record ()
  "png-idat-chunk, a vector from a record."
  (let ((literal (ub8 15))
        (from (ub8 15))
        (rec (list :data (->bytes #(1 2 3)))))
    (marshal png-idat-chunk literal :data (->bytes #(1 2 3)))
    (marshal png-idat-chunk from (:from rec))
    (is (equalp literal
                from))))

(def-test cstring-probe-a-terminated-vector-missing-from-the-record-writes ()
  "cstring-probe, a terminated vector missing from the record writes the terminator."
  (is (equalp '(0 9)
              (let ((rec (list :after 9)))
                (to-list (lambda (d) (marshal cstring-probe d (:from rec))))))))

(def-test leb-vec-probe-a-vector-with-no-default-missing-from-the-record ()
  "leb-vec-probe, a vector with no default missing from the record signals an error."
  (let ((rec (list :after 9)))
    (signals error
      (to-list (lambda (d) (marshal leb-vec-probe d (:from rec)))))))

(def-test masque-probe-typed-masque-groups-from-a-record ()
  "masque-probe-typed, masque groups from a record."
  (is (equalp '(6 #xFD 7)
              (let ((rec (list :mode :rgba :delta -3 :tail 7)))
                (to-list (lambda (d) (marshal masque-probe-typed d (:from rec))))))))

;; Plists read by unmarshal, written back.
(def-test unmarshal-probe-case-probe-and-png-ihdr-chunk-round-trip-through ()
  "unmarshal-probe, case-probe and png-ihdr-chunk round-trip through plists."
  (is (equalp '(t t t)
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
                        (equalp c buf)))))))

;; Record classes.
(def-test rec-ihdr-a-class-with-a-constructor-and-accessors-named-after ()
  "rec-ihdr, a class with a constructor and accessors named after the manifest."
  (is (equalp '(rec-ihdr 640 nil :rgba)
              (let ((r (make-rec-ihdr :width 640 :color-type :rgba)))
                (list (class-name (class-of r)) (rec-ihdr-width r) (rec-ihdr-height r)
                      (rec-ihdr-color-type r))))))

(def-test rec-chunk-an-explicit-class-name-with-span-children-as-slots ()
  "rec-chunk, an explicit class name, with span children as slots."
  (is (equalp '(rec-chunk-record 2 1 3 nil)
              (let ((r (make-rec-chunk-record :length 1 :body 2 :crc 3)))
                (list (class-name (class-of r)) (rec-chunk-record-body r)
                      (field-value r :length) (field-value r :crc) (field-value r :type+data))))))

(def-test rec-foreign-the-accessors-and-constructor-are-interned ()
  "rec-foreign, the accessors and constructor are interned in the class's package."
  (is (equalp '(t t nil)
              (let ((accessor (find-symbol "RULE-RECORD-A" :specops.test.rules))
                    (constructor (find-symbol "MAKE-RULE-RECORD" :specops.test.rules)))
                (list (and accessor (fboundp accessor) t) (and constructor (fboundp constructor) t)
                      (find-symbol "RULE-RECORD-A" '#:specops-tests))))))

(def-test rec-ihdr-a-record-prints-with-its-fields ()
  "rec-ihdr, a record prints with its fields."
  (is (equalp "#<REC-IHDR :WIDTH 1 :HEIGHT NIL :COLOR-TYPE NIL>"
              (let ((*package* (find-package '#:specops-tests)))
                (prin1-to-string (make-rec-ihdr :width 1))))))

;; Records into marshal.
(def-test rec-ihdr-a-record-writes-the-same-bytes-as-literal-values ()
  "rec-ihdr, a record writes the same bytes as literal values."
  (let ((literal (ub8 9))
        (from (ub8 9)))
    (marshal rec-ihdr literal :width 640 :height 480 :color-type :rgba)
    (marshal rec-ihdr from (:from (make-rec-ihdr :width 640 :height 480 :color-type :rgba)))
    (is (equalp literal
                from))))

(def-test rec-ihdr-a-field-edited-through-its-accessor-and-a-missing-one ()
  "rec-ihdr, a field edited through its accessor, and a missing one defaulted."
  (let ((r (make-rec-ihdr :width 1 :height 1))
        (buf (ub8 9)))
    (setf (rec-ihdr-height r) 2)
    (marshal rec-ihdr buf (:from r))
    (is (equalp '(:width 1 :height 2 :color-type :grayscale)
                (unmarshal rec-ihdr buf)))))

(def-test rec-chunk-a-record-holding-a-record-length-and-crc-recomputed ()
  "rec-chunk, a record holding a record; length and CRC recomputed."
  (let ((literal (ub8 21))
        (from (ub8 21)))
    (marshal rec-chunk literal :body (:width 7 :height 9 :color-type :rgb))
    (marshal rec-chunk from (:from (make-rec-chunk-record
                                    :length 999 :crc 0
                                    :body (make-rec-ihdr :width 7 :height 9 :color-type :rgb))))
    (is (equalp literal
                from))))

(def-test rec-parent-a-record-holding-a-plist-for-a-child-without-a-record ()
  "rec-parent, a record holding a plist for a child without a record type."
  (let ((literal (ub8 10))
        (from (ub8 10)))
    (marshal rec-parent literal :a 5 :child (:a -2 :b #x0102 :c -300 :d 7))
    (marshal rec-parent from (:from (make-rec-parent :a 5 :child (list :a -2 :b #x0102
                                                                        :c -300 :d 7))))
    (is (equalp literal
                from))))

(def-test rec-masque-masque-groups-from-a-record ()
  "rec-masque, masque groups from a record."
  (is (equalp '(6 #xFD 7)
              (to-list (lambda (d) (marshal rec-masque d (:from (make-rec-masque :mode :rgba :delta -3
                                                                                 :tail 7))))))))

;; Redefining a manifest with different fields redefines its class.
(def-test redefining-a-record-manifest-with-a-new-field-gives-the-class ()
  "Redefining a record manifest with a new field gives the class the new slot."
  (is (equalp '(1 2 (1 2))
              (progn
                (eval '(defmanifest rec-redefined (:unit 8 :endian :big :record t)
                        (:a :u8 :default 0)))
                (eval '(defmanifest rec-redefined (:unit 8 :endian :big :record t)
                        (:a :u8 :default 0)
                        (:b :u8 :default 0)))
                (let ((r (funcall 'make-rec-redefined :a 1 :b 2)))
                  (list (field-value r :a) (field-value r :b)
                        (to-list (lambda (d) (funcall (compile nil `(lambda (d)
                                                                      (marshal rec-redefined d
                                                                        (:from ,r))))
                                                      d)))))))))

;; Records out of unmarshal.
(def-test rec-chunk-read-as-a-record-holding-a-record ()
  "rec-chunk, read as a record, holds its body as a png-ihdr record, the length and CRC as
read."
  (let ((buf (ub8 21)))
    (marshal rec-chunk buf :body (:width 7 :height 9 :color-type :rgb))
    (assert-that (unmarshal rec-chunk buf (:as :record))
                 (has-type 'rec-chunk-record)
                 (has-slots 'length 9
                            'body (has-all (has-type 'rec-ihdr)
                                           (has-slots 'width 7 'height 9 'color-type :rgb))
                            'crc (crc buf 4 17)))))

(def-test rec-chunk-without-as-the-same-bytes-still-read-as-a-plist ()
  "rec-chunk, without :as the same bytes still read as a plist."
  (let ((buf (ub8 21)))
    (marshal rec-chunk buf :body (:width 7 :height 9 :color-type :rgb))
    (let ((r (unmarshal rec-chunk buf (:as :record))))
      (is (equalp (list :length 9 :body '(:width 7 :height 9 :color-type :rgb)
                        :crc (rec-chunk-record-crc r))
                  (unmarshal rec-chunk buf))))))

(def-test rec-chunk-a-record-read-and-written-back-reproduces-the-bytes ()
  "rec-chunk, a record read and written back reproduces the bytes."
  (let ((buf (ub8 21)))
    (marshal rec-chunk buf :body (:width 7 :height 9 :color-type :rgb))
    (let ((r (unmarshal rec-chunk buf (:as :record))))
      (is (equalp buf
                  (let ((again (ub8 21)))
                    (marshal rec-chunk again (:from r))
                    again))))))

(def-test rec-chunk-a-record-edited-and-written-back-gets-a-new-crc ()
  "rec-chunk, a record edited and written back gets a new CRC, which checks."
  (let ((buf (ub8 21)))
    (marshal rec-chunk buf :body (:width 7 :height 9 :color-type :rgb))
    (let ((r (unmarshal rec-chunk buf (:as :record))))
      (is (equalp '(10 t t)
                  (let ((again (ub8 21)))
                    (setf (rec-ihdr-height (rec-chunk-record-body r)) 10)
                    (marshal rec-chunk again (:from r))
                    (let ((r2 (unmarshal rec-chunk again (:as :record))))
                      (list (rec-ihdr-height (rec-chunk-record-body r2))
                            (= (rec-chunk-record-crc r2) (crc again 4 17))
                            (/= (rec-chunk-record-crc r2) (rec-chunk-record-crc r))))))))))

(def-test rec-parent-read-as-a-record-holding-a-plist-for-a-child-without ()
  "rec-parent, read as a record holding a plist for a child without a record type."
  (let ((buf (ub8 10)))
    (marshal rec-parent buf :a 5 :child (:a -2 :b #x0102 :c -300 :d 7))
    (assert-that (unmarshal rec-parent buf (:as :record))
                 (has-type 'rec-parent)
                 (has-slots 'a 5 'child '(:a -2 :b #x0102 :c -300 :d 7)))))

(def-test rec-masque-masque-groups-read-into-a-record ()
  "rec-masque reads each masque group into its own slot of the record, enumerated and signed
groups decoded."
  (assert-that (unmarshal rec-masque (->bytes #(6 #xFD 7)) (:as :record))
               (has-type 'rec-masque)
               (has-slots 'mode :rgba 'delta -3 'tail 7)))

(def-test unmarshal-options-as-record-with-keys-without-a-record-class ()
  "Unmarshal options: :as :record with keys, without a record class, an unknown :as or option."
  (signals error (macroexpand-1 '(unmarshal rec-ihdr s (:as :record) :width)))
  (signals error (macroexpand-1 '(unmarshal unmarshal-probe s (:as :record))))
  (signals error (macroexpand-1 '(unmarshal rec-ihdr s (:as :struct))))
  (signals error (macroexpand-1 '(unmarshal rec-ihdr s (:format :record)))))

;; Round trips of whole files and records.
(def-test the-1x1-png-read-as-a-plist-and-written-back-is-reproduced ()
  "The 1x1 PNG, read as a plist and written back, is reproduced."
  (let ((png (make-1x1-png)))
    (is (equalp png
                (let ((buf (ub8 72)))
                  (marshal png-file buf (:from (unmarshal png-file png)))
                  buf)))))

(def-test the-1x1-png-read-as-records-and-written-back-is-reproduced ()
  "The 1x1 PNG, read as records and written back, is reproduced."
  (let ((png (make-1x1-png)))
    (is (equalp '(png-file png-ihdr t)
                (let ((buf (ub8 72)) (r (unmarshal png-file png (:as :record))))
                  (marshal png-file buf (:from r))
                  (list (class-name (class-of r))
                        (class-name (class-of (png-ihdr-chunk-png-ihdr (png-file-ihdr r))))
                        (equalp buf png)))))))

(def-test the-1x1-png-ihdr-edited-to-adam7-interlacing-its-crc ()
  "The 1x1 PNG, IHDR edited to Adam7 interlacing: its CRC is recomputed and verifies."
  (let ((png (make-1x1-png)))
    (is (equalp '(1 t t)
                (let ((buf (ub8 72)) (r (unmarshal png-file png (:as :record))))
                  (setf (png-ihdr-interlace (png-ihdr-chunk-png-ihdr (png-file-ihdr r))) :adam7)
                  (marshal png-file buf (:from r))
                  (list (aref buf 28)
                        (/= (rd-u32-be buf 29) (rd-u32-be png 29))
                        (let ((*standard-output* (make-broadcast-stream))) (verify-png buf))))))))

(def-test the-1x1-png-idat-edited-to-a-green-pixel-matches-a-png-built ()
  "The 1x1 PNG, IDAT edited to a green pixel, matches a PNG built green."
  (let ((png (make-1x1-png)))
    (is (equalp (make-1x1-png :r 0 :g 255 :b 0)
                (let ((buf (ub8 72)) (r (unmarshal png-file png (:as :record))))
                  (setf (png-idat-chunk-data (png-file-idat r)) (zlib-store (->bytes #(0 0 255 0))))
                  (marshal png-file buf (:from r))
                  buf)))))

(def-test goff-end-probe-an-80-byte-goff-end-record-read-and-written-back ()
  "goff-end-probe, an 80-byte GOFF END record, read and written back, is reproduced."
  (let ((goff (ub8 80)))
    (marshal goff-end-probe goff :t-tag #x40 :entry-flag 1 :amode 2
                                 :record-count 3 :entry-esdid 5 :entry-offset #x1000)
    (is (equalp goff
                (let ((buf (ub8 80)))
                  (marshal goff-end-probe buf (:from (unmarshal goff-end-probe goff)))
                  buf)))))

;; Sources that are not canonical are written back canonical, not reproduced.
(def-test pad-probe-pad-bytes-other-than-the-fill-value-are-rewritten ()
  "pad-probe, pad bytes other than the fill value are rewritten."
  (is (equalp '(1 7 #xAA #xAA #x11 0 0 0 0 0 0 0 #x22)
              (let ((buf (ub8 13)))
                (marshal pad-probe buf (:from (unmarshal pad-probe
                                                         (->bytes #(1 7 #xBB #xBB #x11 5 5 5 5 5 5 5 #x22)))))
                (coerce buf 'list)))))

(def-test bounded-probe-bytes-after-the-terminator-are-rewritten ()
  "bounded-probe, bytes after the terminator are rewritten as terminators."
  (is (equalp '(97 0 0 0 0 9)
              (let ((rec (unmarshal bounded-probe (->bytes #(97 0 122 122 122 9)))))
                (to-list (lambda (d) (marshal bounded-probe d (:from rec))))))))

(def-test case-unnamed-probe-an-unnamed-field-is-rewritten ()
  "case-unnamed-probe, an unnamed field is rewritten with its default."
  (is (equalp '(1 0 0 0 0 9)
              (let ((rec (unmarshal case-unnamed-probe (->bytes #(1 0 0 0 7 9)))))
                (to-list (lambda (d) (marshal case-unnamed-probe d (:from rec))))))))

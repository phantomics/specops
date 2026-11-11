;;;; test/test-manifest-ref.lisp — The examples of the Manifest Reference, doc/Ref.Manifest.md,
;;;; each a test, so that the document cannot drift from the code. They use only the names
;;;; specops exports, and OCTETS below, which the Reference defines in its conventions. The
;;;; stream examples write temporary files, deleted afterwards; the rest are pure.

(in-package #:specops-tests)

(in-suite manifest-ref)

;;; ============================================================
;;; Conventions
;;; ============================================================

(defun octets (&rest units)
  "A vector of (unsigned-byte 8) holding UNITS, the source type of an 8-bit manifest."
  (make-array (length units) :element-type '(unsigned-byte 8) :initial-contents units))

;;; ============================================================
;;; Declaring manifests: scalars, byte orders and enumerations
;;; ============================================================

(defenum shape-kind nil :circle 1 :square 2 :triangle 3)

(defmanifest shape (:unit 8 :endian :big :record t)
  (:kind (:u 1) :default :circle :enumerate-by shape-kind)
  (:x    (:s 2) :default 0)
  (:y    (:s 2) :default 0 :endian :little)
  (:size :u16   :default 1))

(def-test ref-shape-written-and-read ()
  "marshal writes each field at its width, signed fields in two's complement and :y
little-endian, returns the position after the last unit, and unmarshal reads the fields
back as a plist, the enumerated :kind as its keyword."
  (let ((buf (make-array 7 :element-type '(unsigned-byte 8))))
    (is (eql 7 (marshal shape buf :kind :square :x -2 :y 300 :size 10)))
    (is (equalp (octets 2 #xFF #xFE #x2C #x01 0 10) buf))
    (is (equalp '(:kind :square :x -2 :y 300 :size 10)
                (unmarshal shape buf)))))

(def-test ref-shape-defaults ()
  "A field given no value takes its :default, a keyword default through its enumeration."
  (let ((buf (make-array 7 :element-type '(unsigned-byte 8))))
    (marshal shape buf :x 5)
    (is (equalp (octets 1 0 5 0 0 0 1) buf))))

(def-test ref-shape-unknown-code ()
  "A code its enumeration lacks reads as the integer itself."
  (is (eql 9 (unmarshal shape (octets 9 0 0 0 0 0 1) nil :kind))))

(defmanifest word-sample (:unit 16 :endian :big)
  (:a (:u 1) :default 0)
  (:b (:u 2) :default 0))

(def-test ref-word-sample-widths-count-units ()
  "Widths count the manifest's units: in 16-bit units, (:u 1) is one unit and (:u 2) two."
  (let ((buf (make-array 3 :element-type '(unsigned-byte 16))))
    (marshal word-sample buf :a #x1234 :b #x56789ABC)
    (is (equalp '(#x1234 #x5678 #x9ABC) (coerce buf 'list)))))

;;; ============================================================
;;; Constant strings, masques and pads
;;; ============================================================

(defmanifest frame-header (:unit 8 :endian :big :length 12)
  (str "FR")
  (masque "h:vvff" (v #x01) (f :flags :u :default 0))
  (:count :u8 :default 0)
  (pad 0 :align 4)
  (:id :u8 :default 0))

(def-test ref-frame-header-layout ()
  "A constant string, a masque of a static group and a keyword group, a pad to a 4-unit
boundary, and the manifest's :length, which pads the record to 12 units."
  (let ((buf (make-array 12 :element-type '(unsigned-byte 8) :initial-element #xEE)))
    (is (eql 12 (marshal frame-header buf :flags 3 :count 2 :id 9)))
    (is (equalp (octets #x46 #x52 #x01 3 2 0 0 0 9 0 0 0) buf))
    (is (equalp '(:flags 3 :count 2 :id 9)
                (unmarshal frame-header buf)))))

(def-test ref-frame-header-constant-checked ()
  "A constant string that differs on read signals manifest-check-failed of kind :constant,
at the first unit that differs."
  (let ((c (handler-case (unmarshal frame-header (octets #x46 #x51 1 3 2 0 0 0 9 0 0 0))
             (manifest-check-failed (c) c))))
    (is (typep c 'manifest-check-failed))
    (is (eq :constant (check-kind c)))
    (is (eql 1 (check-position c)))))

;;; ============================================================
;;; Vectors, terminators, LEB128, codecs and floats
;;; ============================================================

(defmanifest entry-name (:unit 8 :endian :big)
  (:len   :u8 :slot (:count-of :name))
  (:name  (:u 1 :vec) :count :len)
  (:path  (:u 1 :vec) :end-by 0)
  (:tag   (:u 1 :vec) :count 4 :end-by 0)
  (:size  (:u 1 :leb) :default 0)
  (:title (:u 1 :vec) :count 3 :default "   " :codec #'specops.map.ebcdic:ebcdic-code-cp037)
  (:ratio :f32 :default 0))

(def-test ref-entry-name-written ()
  "A vector counted by an earlier field, which a :count-of slot fills in; a NUL-terminated
vector; a four-unit buffer ended by NUL; an unsigned LEB128; three EBCDIC characters; and a
single float."
  (let ((buf (make-array 19 :element-type '(unsigned-byte 8))))
    (marshal entry-name buf :name "ab" :path "/x" :tag "id" :size 300 :title "HI " :ratio 0.5)
    (is (equalp (octets 2 97 98  47 120 0  105 100 0 0  #xAC #x02  #xC8 #xC9 #x40
                        #x3F 0 0 0)
                buf))))

(def-test ref-entry-name-read ()
  "Vectors read as vectors of codes, a field with a codec as a string, and the float as a
single float."
  (is (equalp '(:len 2 :name #(97 98) :path #(47 120) :tag #(105 100) :size 300
                :title "HI " :ratio 0.5)
              (unmarshal entry-name (octets 2 97 98  47 120 0  105 100 0 0  #xAC #x02
                                            #xC8 #xC9 #x40  #x3F 0 0 0)))))

;;; ============================================================
;;; Widths chosen by an earlier field: :case
;;; ============================================================

(defmanifest value-cell (:unit 8 :endian :big)
  (:kind  :u8 :default 0)
  (:value (:case :kind (0 :u8) (1 :s 4) (2 :f 8) (otherwise :u 1 :leb)) :default 0))

(def-test ref-value-cell-branches ()
  "The selector's value chooses the branch on write, whether it is known at expansion or
only when the code runs, and on read."
  (let ((buf (make-array 5 :element-type '(unsigned-byte 8))))
    (marshal value-cell buf :kind 1 :value -5)
    (is (equalp (octets 1 #xFF #xFF #xFF #xFB) buf)))
  (let ((buf (make-array 3 :element-type '(unsigned-byte 8)))
        (kind 9))
    (marshal value-cell buf :kind kind :value 300)
    (is (equalp (octets 9 #xAC #x02) buf)))
  (is (equalp '(:kind 2 :value 1.5d0)
              (unmarshal value-cell (octets 2 #x3F #xF8 0 0 0 0 0 0)))))

;;; ============================================================
;;; Spans and slots: lengths and checksums
;;; ============================================================

(defmanifest chunk (:unit 8 :endian :big :record t)
  (:length (:u 4) :slot (:length-of :data))
  (span :body
    (str "DATA")
    (:data (:u 1 :vec)))
  (:crc (:u 4) :slot (:checksum-of :body :by #'crc)))

(def-test ref-chunk-slots-written ()
  "A :length-of slot holds the length of its field and a :checksum-of slot the checksum of
its span, both computed on write."
  (let ((buf (make-array 15 :element-type '(unsigned-byte 8))))
    (marshal chunk buf :data #(1 2 3))
    (is (equalp (octets 0 0 0 3  #x44 #x41 #x54 #x41  1 2 3  #xAD #xEA #x58 #x81) buf))
    (is (eql (crc buf 4 11) (unmarshal chunk buf nil :crc)))))

(def-test ref-chunk-read ()
  "On read, the length slot gives the vector's count, and the checksum is checked."
  (is (equalp '(:length 3 :data #(1 2 3) :crc #xADEA5881)
              (unmarshal chunk (octets 0 0 0 3  #x44 #x41 #x54 #x41  1 2 3
                                       #xAD #xEA #x58 #x81)))))

(def-test ref-chunk-check-failed ()
  "A checksum that fails signals manifest-check-failed, whose readers describe the failure;
CONTINUE accepts the value read, USE-VALUE substitutes another, and (:check nil) leaves the
check out."
  (let ((bad (octets 0 0 0 3  #x44 #x41 #x54 #x41  1 7 3  #xAD #xEA #x58 #x81)))
    (let ((c (handler-case (unmarshal chunk bad)
               (manifest-check-failed (c) c))))
      (is (eq :checksum (check-kind c)))
      (is (eq :crc (check-field c)))
      (is (eq 'chunk (check-manifest c)))
      (is (eql 11 (check-position c)))
      (is (eql #xADEA5881 (check-actual c)))
      (is (eql (crc bad 4 11) (check-expected c)))
      (is-true (search "field :CRC of manifest CHUNK" (princ-to-string c))))
    (is (eql #xADEA5881
             (handler-bind ((manifest-check-failed #'continue))
               (unmarshal chunk bad nil :crc))))
    (is (eql (crc bad 4 11)
             (handler-bind ((manifest-check-failed
                              (lambda (c) (use-value (check-expected c) c))))
               (getf (unmarshal chunk bad) :crc))))
    (is (eql #xADEA5881 (getf (unmarshal chunk bad (:check nil)) :crc)))))

;;; ============================================================
;;; Sub-manifests and repeats
;;; ============================================================

(defmanifest point (:unit 8 :endian :big :record t)
  (:x :s8 :default 0)
  (:y :s8 :default 0))

(defmanifest polygon (:unit 8 :endian :big :record t)
  (:n :u8 :slot (:count-of :points))
  (manifest :points point :count :n))

(defmanifest path (:unit 8 :endian :big)
  (:size   :u8 :slot (:length-of :points))
  (manifest :points point :until :length)
  (:closed :u8 :default 0))

(defmanifest track (:unit 8 :endian :big)
  (manifest :points point :until (:x 0)))

(defmanifest scatter (:unit 8 :endian :big)
  (manifest :points point :until :end))

(def-test ref-polygon-counted ()
  "A sub-manifest repeated :count times, its instances given as plists or records and
counted by a :count-of slot, reads back as a list of plists, or of records under
(:as :record)."
  (let ((buf (make-array 5 :element-type '(unsigned-byte 8))))
    (marshal polygon buf :points (list (list :x 1 :y 2) (make-point :x -1 :y 3)))
    (is (equalp (octets 2  1 2  #xFF 3) buf))
    (is (equalp '(:n 2 :points ((:x 1 :y 2) (:x -1 :y 3)))
                (unmarshal polygon buf)))
    (let ((r (unmarshal polygon buf (:as :record))))
      (assert-that r (has-type 'polygon) (has-slots 'n 2))
      (is (equal '(1 -1) (mapcar #'point-x (polygon-points r)))))))

(def-test ref-repeats-until ()
  "A repeat may end where its length slot says, at an instance whose fields have given
values, which is kept, or at the end of the source."
  (let ((buf (make-array 6 :element-type '(unsigned-byte 8))))
    (marshal path buf :points (list (list :x 1 :y 2) (list :x 3 :y 4)) :closed 1)
    (is (equalp (octets 4  1 2  3 4  1) buf))
    (is (equalp '(:size 4 :points ((:x 1 :y 2) (:x 3 :y 4)) :closed 1)
                (unmarshal path buf))))
  (is (equalp '(:points ((:x 1 :y 2) (:x 3 :y 4) (:x 0 :y 0)))
              (unmarshal track (octets 1 2  3 4  0 0))))
  (is (equalp '(:points ((:x 1 :y 2) (:x 3 :y 4)))
              (unmarshal scatter (octets 1 2  3 4)))))

;;; ============================================================
;;; Placed fields
;;; ============================================================

(defmanifest archive-entry (:unit 8 :endian :big :record t)
  (:offset :u8 :slot (:offset-of :data))
  (:size   :u8 :slot (:length-of :data))
  (:data   (:u 1 :vec) :place :blobs))

(defmanifest archive (:unit 8 :endian :big :record t)
  (:n :u8 :slot (:count-of :entries))
  (manifest :entries archive-entry :count :n)
  (place :blobs :align 4))

(def-test ref-archive-placed ()
  "Placed fields are written where their place directive stands, each aligned, with their
offset and length slots filled in; on read each is found at its offset."
  (let ((buf (make-array 16 :element-type '(unsigned-byte 8))))
    (is (eql 16 (marshal archive buf :entries (list (list :data #(1 2 3)) (list :data #(4 5))))))
    (is (equalp (octets 2  8 3  12 2  0 0 0  1 2 3 0  4 5 0 0) buf))
    (is (equalp '(:n 2 :entries ((:offset 8 :size 3 :data #(1 2 3))
                                 (:offset 12 :size 2 :data #(4 5))))
                (unmarshal archive buf)))))

;;; ============================================================
;;; Writing: destinations, records and buffers
;;; ============================================================

(def-test ref-marshal-to-a-function ()
  "A function destination is called with each unit in turn."
  (let ((units nil))
    (marshal shape (lambda (unit) (push unit units)) :kind :square :x 1)
    (is (equal '(2 0 1 0 0 0 1) (reverse units)))))

(def-test ref-marshal-to-and-from-a-stream ()
  "A stream destination receives the units in order; a stream source is read to the end of
the record and no further, so records can be read one after another."
  (uiop:with-temporary-file (:pathname file :type "bin")
    (with-open-file (out file :direction :output :if-exists :supersede
                              :element-type '(unsigned-byte 8))
      (marshal chunk out :data #(1 2 3))
      (marshal chunk out :data #(4)))
    (with-open-file (in file :element-type '(unsigned-byte 8))
      (is (equalp #(1 2 3) (unmarshal chunk in nil :data)))
      (is (equalp #(4) (unmarshal chunk in nil :data)))
      (is (eql 28 (file-position in))))))

(def-test ref-marshal-from-a-record ()
  "(:from R) takes each field not given from R, a plist or a record; a field R lacks takes
its default."
  (let ((buf (make-array 7 :element-type '(unsigned-byte 8))))
    (marshal shape buf (:from (list :kind :square :y 4)) :x 9)
    (is (equalp (octets 2 0 9 4 0 0 1) buf))
    (marshal shape buf (:from (make-shape :kind :triangle :x 3 :size 2)))
    (is (equalp (octets 3 0 3 0 0 0 2) buf))))

(def-test ref-marshal-buffer-reused ()
  "A manifest-buffer given as (:buffer B) is kept and reused across calls that write a
manifest with slots to a stream or a function."
  (let ((b (make-manifest-buffer))
        (units nil))
    (dolist (data '(#(1) #(2 3)))
      (marshal chunk (lambda (u) (push u units)) (:buffer b) :data data))
    (is (eql 27 (length units)))))

(def-test ref-marshal-overrun ()
  "A vector too small signals manifest-overrun, whose readers give the manifest, the
vector's size and the unit that would not fit."
  (let ((c (handler-case (marshal shape (make-array 4 :element-type '(unsigned-byte 8)) :x 1)
             (manifest-overrun (c) c))))
    (is (eq 'shape (overrun-manifest c)))
    (is (eql 4 (overrun-size c)))
    (is (eql 4 (overrun-position c)))))

;;; ============================================================
;;; Reading: sources, selective reads and records
;;; ============================================================

(def-test ref-unmarshal-sources ()
  "A source may be a vector of the manifest's units, an integer, whose first unit is its
most significant, or a (units . integer) pair, which gives the integer's length."
  (let ((plist '(:kind :square :x -2 :y 300 :size 10)))
    (is (equalp plist (unmarshal shape (octets 2 #xFF #xFE #x2C #x01 0 10))))
    (is (equalp plist (unmarshal shape #x02FFFE2C01000A)))
    (is (equalp plist (unmarshal shape '(7 . #x02FFFE2C01000A))))))

(def-test ref-unmarshal-selective ()
  "Keys after the parameters return those fields' values, in the order given."
  (is (equal '(10 :square)
             (multiple-value-list
              (unmarshal shape (octets 2 #xFF #xFE #x2C #x01 0 10) nil :size :kind)))))

(def-test ref-records ()
  "A manifest with (:record t) defines a class of its name, with a constructor, an accessor
and an initarg for each field; field-value reads a field from a record or a plist, and a
record prints with its fields."
  (let ((s (make-shape :kind :triangle :x 3)))
    (is (eql 3 (shape-x s)))
    (setf (shape-y s) 4)
    (is (eql 4 (field-value s :y)))
    (is (eql 4 (field-value '(:y 4) :y)))
    (is (null (field-value s :size)))
    (is-true (search ":KIND :TRIANGLE :X 3 :Y 4 :SIZE NIL>" (prin1-to-string s)))
    (assert-that (unmarshal shape (octets 2 #xFF #xFE #x2C #x01 0 10) (:as :record))
                 (has-type 'shape)
                 (has-slots 'kind :square 'x -2 'y 300 'size 10))))

;;; ============================================================
;;; Stop rules
;;; ============================================================

;; A stop rule's class must exist when a field that names it is expanded.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass until-byte (stop-rule)
    ((byte :initarg :byte :reader until-byte-of))))

(defmethod feed ((r until-byte) element)
  (and (eql element (until-byte-of r)) :drop))

(defmethod frame-trailer ((r until-byte))
  (list (until-byte-of r)))

(defmanifest marked-list (:unit 8 :endian :big)
  (:items (:u 1 :vec) :stop-by (until-byte :byte #xFF))
  (:after :u8 :default 0))

(def-test ref-stop-rule ()
  "A user's stop rule ends a :vec field: feed returns :DROP at the byte that ends it, and
frame-trailer gives the codes written after the data."
  (let ((buf (make-array 4 :element-type '(unsigned-byte 8))))
    (marshal marked-list buf :items #(1 2) :after 9)
    (is (equalp (octets 1 2 #xFF 9) buf))
    (is (equalp '(:items #(1 2) :after 9) (unmarshal marked-list buf)))))

;;; ============================================================
;;; Serialization primitives
;;; ============================================================

(def-test ref-masque-and-unmasque ()
  "masque composes an integer from a mask's groups, in hexadecimal, binary or decimal
digits, with constant digits kept; unmasque takes it apart, or returns NIL when a constant
digit differs."
  (is (eql #x1234 (masque "h:aabb" (a #x12) (b #x34))))
  (is (eql #b01011010 (masque "b:aaaa1bbb" (a 5) (b 2))))
  (is (eql 30009 (masque "d:aa00bb" (a 3) (b 9))))
  (is (equal '(#x12 #x34) (unmasque "h:aabb" #x1234 (a b) (list a b))))
  (is (null (unmasque "b:aaaa1bbb" #b01010010 (a b) (list a b)))))

(def-test ref-masque-overflow ()
  "A value too wide for its group is masked to the group's width by default; (:overflow
. :error) makes it an error."
  (is (eql #x2304 (masque "h:aabb" (a #x123) (b 4))))
  (let ((a #x123))
    (signals error (masque "h:aabb" ((:overflow . :error)) (a a) (b 4)))))

(def-test ref-serialize ()
  "serialize passes the units of an integer, most significant first, a (width . value)
pair at that width, or a vector's elements, to a collector."
  (let ((units nil))
    (flet ((collect (u) (push u units)))
      (serialize #x1234 8 #'collect)
      (serialize '(3 . 5) 8 #'collect)
      (serialize #(7 8) 8 #'collect))
    (is (equal '(#x12 #x34 0 0 5 7 8) (reverse units)))))

(def-test ref-checksums ()
  "crc, adler32 and sfnt-checksum take a vector and a range, as :checksum-of slots call
them; adler32-checksum takes keyword bounds."
  (let ((v (map '(vector (unsigned-byte 8)) #'char-code "123456789")))
    (is (eql #xCBF43926 (crc v 0 9)))
    (is (eql #x091E01DE (adler32 v 0 9)))
    (is (eql #x091E01DE (adler32-checksum v)))
    (is (eql #x9F686A6C (sfnt-checksum v 0 9)))))

(def-test ref-float-bits ()
  "encode-float-bits gives the IEEE bit pattern of a number at 16, 32, 64 or 128 bits,
rounded to nearest; decode-float-bits reads one back."
  (is (eql #x3FC00000 (encode-float-bits 1.5 32)))
  (is (eql #x3555 (encode-float-bits 1/3 16)))
  (is (eql 1.5 (decode-float-bits #x3FC00000 32)))
  (is (eql 1.0 (decode-float-bits #x3C00 16)))
  (is (eq :infinity (decode-float-bits #x7FFF0000000000000000000000000000 128))))

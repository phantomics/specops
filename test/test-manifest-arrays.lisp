;;;; test/test-manifest-arrays.lisp — Lexter's cell model as a test format, the prototype of the
;;;; plan for manifests over arrays of records: the plain, inline-colour and tile cells, and a
;;;; view of the fields every format shares, checked against the worked records of Lexter's
;;;; Cell Model Architecture (Lexter's doc/Arch.CellModel.md at commit 76741bc). Pure: no
;;;; files.

(in-package #:specops-tests)

(in-suite manifest-arrays)

;;; ============================================================
;;; Probe manifests and helpers
;;; ============================================================

;; A record is eight 32-bit words, little-endian, in an array of bytes. A cell's format is its
;; first byte, so a reader dispatches on it and reads the cell through that format's manifest,
;; whose constant first byte is checked.
(defenum cell-format nil :plain 0 :void 1 :inline-colours 2 :effect 3 :tiles 4)

(defenum mask-operation nil :union 0 :intersection 1 :exclusion 2 :mask-less-coverage 3)

;; A glyph word: a 24-bit atlas position, then its settings byte, as a layer or as a mask.
(defmanifest glyph-layer (:unit 8 :endian :little :record t)
  (:glyph (:u 3) :default 0)
  (masque "b:00000iis" (s :selector :u :default 0) (i :ink :u :default 0)))

(defmanifest glyph-mask (:unit 8 :endian :little :record t)
  (:glyph (:u 3) :default 0)
  (masque "b:00rrroos" (s :selector :u :default 0)
          (o :operation :u :default :union :enumerate-by mask-operation)
          (r :reach :u :default 0)))

;; The header word: the swatch, the palette slot, then a byte of the background slot and the
;; colour parameters, which the plain and inline-colour formats leave zero.
(defmanifest plain-cell (:unit 8 :endian :little :record t)
  0                                                          ; the plain format
  (masque "b:edcbannn" (n :glyph-count :u :default 0)
          (a :type-1 :u :default 0) (b :type-2 :u :default 0) (c :type-3 :u :default 0)
          (d :type-4 :u :default 0) (e :type-5 :u :default 0))
  (masque "b:00000kqw" (w :wide :u :default 0) (q :right-half :u :default 0)
          (k :blink :u :default 0))
  0                                                          ; no parameters
  (:swatch :u16 :default 0)
  (:palette :u8 :default 0)
  (masque "b:000000gg" (g :background :u :default 0))
  (manifest :g0 glyph-layer)                                 ; glyph 0 is always a layer
  (:g1 (:case :type-1 (0 (manifest glyph-layer)) (1 (manifest glyph-mask))))
  (:g2 (:case :type-2 (0 (manifest glyph-layer)) (1 (manifest glyph-mask))))
  (:g3 (:case :type-3 (0 (manifest glyph-layer)) (1 (manifest glyph-mask))))
  (:g4 (:case :type-4 (0 (manifest glyph-layer)) (1 (manifest glyph-mask))))
  (:g5 (:case :type-5 (0 (manifest glyph-layer)) (1 (manifest glyph-mask)))))

(defmanifest inline-cell (:unit 8 :endian :little :record t)
  2                                                          ; inline colours
  (masque "b:000bannn" (n :glyph-count :u :default 0)
          (a :type-1 :u :default 0) (b :type-2 :u :default 0))
  (masque "b:00000kqw" (w :wide :u :default 0) (q :right-half :u :default 0)
          (k :blink :u :default 0))
  0
  (:swatch :u16 :default 0)
  (:palette :u8 :default 0)
  (masque "b:000000gg" (g :background :u :default 0))
  (manifest :g0 glyph-layer)
  (:g1 (:case :type-1 (0 (manifest glyph-layer)) (1 (manifest glyph-mask))))
  (:g2 (:case :type-2 (0 (manifest glyph-layer)) (1 (manifest glyph-mask))))
  (:colours (:u 1 :vec) :count 12 :default #(0 0 0 0 0 0 0 0 0 0 0 0)))  ; slots 0-3, RGB

;; A tile field: a 14-bit index into the tile texture, and its flips.
(defmanifest tile-ref (:unit 8 :endian :little :record t)
  (masque "b:vhiiiiiiiiiiiiii" (i :index :u :default 0)
          (h :horizontal-flip :u :default 0) (v :vertical-flip :u :default 0)))

;; Byte 2 claims bit 23 for the top tile above the glyphs; byte 3 holds the transparency field
;; and the two colour offsets; the header's last byte, the colour cycles.
(defmanifest tile-cell (:unit 8 :endian :little :record t)
  4                                                          ; tiles
  (masque "b:000bannn" (n :glyph-count :u :default 0)
          (a :type-1 :u :default 0) (b :type-2 :u :default 0))
  (masque "b:t0000kqw" (w :wide :u :default 0) (q :right-half :u :default 0)
          (k :blink :u :default 0) (t :top-above :u :default 0))
  (masque "b:ppoo00xx" (x :transparency :u :default 0)
          (o :bottom-offset :u :default 0) (p :top-offset :u :default 0))
  (:swatch :u16 :default 0)
  (:palette :u8 :default 0)
  (masque "b:0hcceegg" (g :background :u :default 0) (e :cycle-ab :u :default 0)
          (c :cycle-cd :u :default 0) (h :grouping :u :default 0))
  (manifest :g0 glyph-layer)
  (:g1 (:case :type-1 (0 (manifest glyph-layer)) (1 (manifest glyph-mask))))
  (:g2 (:case :type-2 (0 (manifest glyph-layer)) (1 (manifest glyph-mask))))
  (manifest :bottom tile-ref)
  (manifest :top tile-ref)
  (:swatch-a :u16 :default 0) (:swatch-b :u16 :default 0)
  (:swatch-c :u16 :default 0) (:swatch-d :u16 :default 0))

;; A view of the first eight bytes, which every format reads alike but for its parameters, for
;; reading, and later amending, the fields a writer changes without asking a cell's format.
(defmanifest cell-common (:unit 8 :endian :little :record t)
  (:format :u8 :default :plain :enumerate-by cell-format)
  (:structure :u8 :default 0)
  (masque "b:fffffkqw" (w :wide :u :default 0) (q :right-half :u :default 0)
          (k :blink :u :default 0) (f :format-flags :u :default 0))
  (:parameters :u8 :default 0)
  (:swatch :u16 :default 0)
  (:palette :u8 :default 0)
  (masque "b:ccccccgg" (g :background :u :default 0) (c :colour-parameters :u :default 0)))

;; Entries whose data is placed after them, with offsets from the record's start.
(defmanifest indexed-entry (:unit 8 :endian :big)
  (:offset :u8 :slot (:offset-of :data))
  (:size   :u8 :slot (:length-of :data))
  (:data   (:u 1 :vec) :place :blobs))

(defmanifest indexed-archive (:unit 8 :endian :big)
  (:n :u8 :slot (:count-of :entries))
  (manifest :entries indexed-entry :count :n)
  (place :blobs))

(defun read-cell (record)
  "RECORD, a 32-byte cell, read through the manifest of the format its first byte names."
  (ecase (aref record 0)
    (0 (unmarshal plain-cell record))
    (2 (unmarshal inline-cell record))
    (4 (unmarshal tile-cell record))))

(defun cell-octets (&rest bytes)
  "A 32-byte record holding BYTES, then zeros."
  (let ((v (ub8 32)))
    (replace v bytes)
    v))

;;; ============================================================
;;; Plain cells: the worked records
;;; ============================================================

;; In these records swatch #x1234 has the background in slot 0 and the foreground in slot 1,
;; and slot 2 red where an underline is coloured; the palette slot is 7, and g is at atlas
;; position 103. The line glyphs are at 8 + 8 × style + flags.

(def-test a-record-of-zeros-is-a-blank-plain-cell ()
  "A record of zeros reads as a plain cell with no glyphs, and a plain cell written with
every field defaulted is all zeros."
  (let ((zeros (ub8 32)))
    (is (equalp '(0 :plain) (list (unmarshal plain-cell zeros nil :glyph-count)
                                  (unmarshal cell-common zeros nil :format))))
    (let ((buf (make-array 32 :element-type '(unsigned-byte 8) :initial-element #xEE)))
      (is (eql 32 (marshal plain-cell buf)))
      (is (equalp zeros buf)))))

(def-test the-letter-g-underlined-in-the-text-s-colour ()
  "Two glyphs, the type bit of glyph 1 marking it a mask: g as a layer in ink slot 1, and the
underline glyph 9 cutting it by union."
  (let ((buf (ub8 32))
        (record (cell-octets 0 #x0A 0 0  #x34 #x12 7 0  103 0 0 #x02  9 0 0 0)))
    (marshal plain-cell buf :glyph-count 2 :type-1 1 :swatch #x1234 :palette 7
                            :g0 (:glyph 103 :ink 1) :g1 (:glyph 9 :operation :union))
    (is (equalp record buf))
    (is (equalp '(2 1 (:glyph 103 :selector 0 :ink 1)
                  (:glyph 9 :selector 0 :operation :union :reach 0))
                (multiple-value-list (unmarshal plain-cell record nil
                                                :glyph-count :type-1 :g0 :g1))))))

(def-test the-letter-g-in-reverse-video ()
  "Reverse video is the background slot set to 1 and the text's ink slot to 0, in the same
swatch."
  (let ((buf (ub8 32)))
    (marshal plain-cell buf :glyph-count 1 :swatch #x1234 :palette 7 :background 1
                            :g0 (:glyph 103 :ink 0))
    (is (equalp (cell-octets 0 1 0 0  #x34 #x12 7 1  103 0 0 0) buf))))

(def-test a-red-underline-beneath-g-and-a-strikethrough-in-its-colour ()
  "Three glyphs: the underline as a layer in slot 2, g as a layer over it, and the
strikethrough, glyph 10, as a mask by union; only glyph 2's type bit is set."
  (let ((buf (ub8 32))
        (record (cell-octets 0 #x13 0 0  #x34 #x12 7 0  9 0 0 #x04  103 0 0 #x02  10 0 0 0)))
    (marshal plain-cell buf :glyph-count 3 :type-2 1 :swatch #x1234 :palette 7
                            :g0 (:glyph 9 :ink 2) :g1 (:glyph 103 :ink 1)
                            :g2 (:glyph 10 :operation :union))
    (is (equalp record buf))
    (is (equalp '((:glyph 103 :selector 0 :ink 1)
                  (:glyph 10 :selector 0 :operation :union :reach 0))
                (multiple-value-list (unmarshal plain-cell record nil :g1 :g2))))))

(def-test a-scroll-bar-thumb-as-a-band-inside-one-cell ()
  "Two masks cut the bar by intersection, the second with its uninked pixels, for a band from
two-eighths to six-eighths down; the bar and the blocks are at atlas positions 200 to 202."
  (let ((buf (ub8 32))
        (record (cell-octets 0 #x1B 0 0  0 0 0 0  200 0 0 #x02  201 0 0 #x02  202 0 0 #x03)))
    (marshal plain-cell buf :glyph-count 3 :type-1 1 :type-2 1
                            :g0 (:glyph 200 :ink 1)
                            :g1 (:glyph 201 :operation :intersection)
                            :g2 (:glyph 202 :selector 1 :operation :intersection))
    (is (equalp record buf))
    (is (equalp '(:glyph 202 :selector 1 :operation :intersection :reach 0)
                (unmarshal plain-cell record nil :g2)))))

(def-test a-mask-reaching-two-layers ()
  "A mask with a reach field of 1 cuts the two layers before it: the left half, at atlas
position 203, reveals g and its coloured underline to the middle of the cell."
  (let ((buf (ub8 32)))
    (marshal plain-cell buf :glyph-count 3 :type-2 1 :swatch #x1234 :palette 7
                            :g0 (:glyph 9 :ink 2) :g1 (:glyph 103 :ink 1)
                            :g2 (:glyph 203 :operation :intersection :reach 1))
    (is (equalp (cell-octets 0 #x13 0 0  #x34 #x12 7 0  9 0 0 #x04  103 0 0 #x02  203 0 0 #x0A)
                buf))))

(def-test the-letter-g-blinking-over-a-red-underline ()
  "The blink flag is bit 18, bit 2 of byte 2, and both glyphs are layers."
  (let ((buf (ub8 32))
        (record (cell-octets 0 2 4 0  #x34 #x12 7 0  9 0 0 #x04  103 0 0 #x02)))
    (marshal plain-cell buf :glyph-count 2 :blink 1 :swatch #x1234 :palette 7
                            :g0 (:glyph 9 :ink 2) :g1 (:glyph 103 :ink 1))
    (is (equalp record buf))
    (is (eql 1 (unmarshal plain-cell record nil :blink)))))

(def-test glyph-words-are-read-by-their-type-bits-when-the-code-runs ()
  "When the type bits are known only when the code runs, each glyph word's form is chosen
then, on write as on read."
  (let ((buf (ub8 32))
        (types '(1 0)))
    (marshal plain-cell buf :glyph-count 3 :type-1 (first types) :type-2 (second types)
                            :g0 (:glyph 103 :ink 1) :g1 (:glyph 9 :operation :union)
                            :g2 (:glyph 104 :ink 1))
    (is (equalp (cell-octets 0 #x0B 0 0  0 0 0 0  103 0 0 2  9 0 0 0  104 0 0 2) buf))
    (is (equalp '((:glyph 9 :selector 0 :operation :union :reach 0)
                  (:glyph 104 :selector 0 :ink 1))
                (multiple-value-list (unmarshal plain-cell buf nil :g1 :g2))))))

;;; ============================================================
;;; Inline colours and tiles
;;; ============================================================

(def-test a-cell-with-inline-colours ()
  "Format 2 holds up to three glyph words and the colours of slots 0-3, three bytes each,
in bytes 20-31."
  (let ((buf (ub8 32))
        (record (cell-octets 2 1 0 0  0 0 0 0  103 0 0 2  0 0 0 0  0 0 0 0
                             0 0 0  #xFF #xFF #xFF  #x12 #x34 #x56  0 #x80 #xFF)))
    (marshal inline-cell buf :glyph-count 1 :g0 (:glyph 103 :ink 1)
                             :colours #(0 0 0  #xFF #xFF #xFF  #x12 #x34 #x56  0 #x80 #xFF))
    (is (equalp record buf))
    (is (equalp #(0 0 0  #xFF #xFF #xFF  #x12 #x34 #x56  0 #x80 #xFF)
                (unmarshal inline-cell record nil :colours)))))

(def-test a-chess-piece-on-a-board-square-with-a-label ()
  "A tile cell with one glyph: a transparency field of 2, offsets of 2 and 0, the square's
tile beneath the piece's, and swatches A-D in words 6 and 7."
  (let ((buf (ub8 32))
        (record (cell-octets 4 1 0 #x22  #x34 #x12 9 0  140 0 0 2  0 0 0 0  0 0 0 0
                             10 0 11 0  1 1 2 1  3 1 4 1)))
    (marshal tile-cell buf :glyph-count 1 :transparency 2 :bottom-offset 2 :top-offset 0
                           :swatch #x1234 :palette 9 :g0 (:glyph 140 :ink 1)
                           :bottom (:index 10) :top (:index 11)
                           :swatch-a #x101 :swatch-b #x102 :swatch-c #x103 :swatch-d #x104)
    (is (equalp record buf))
    (is (equalp '(2 2 0 (:index 10 :horizontal-flip 0 :vertical-flip 0))
                (multiple-value-list (unmarshal tile-cell record nil
                                                :transparency :bottom-offset :top-offset
                                                :bottom))))))

(def-test shore-water-mirrored-and-cycling-beneath-a-boat ()
  "The top tile above the glyphs, bit 23; the water's tile flipped horizontally, bit 14 of
its field; and the cycle of C-D's first four colours, b = 2, in the header's last byte."
  (let ((buf (ub8 32))
        (record (cell-octets 4 1 #x80 #x22  #x34 #x12 9 #x20  140 0 0 2  0 0 0 0  0 0 0 0
                             20 #x40 21 0  1 1 2 1  3 1 4 1)))
    (marshal tile-cell buf :glyph-count 1 :top-above 1 :transparency 2 :bottom-offset 2
                           :swatch #x1234 :palette 9 :cycle-cd 2 :g0 (:glyph 140 :ink 1)
                           :bottom (:index 20 :horizontal-flip 1) :top (:index 21)
                           :swatch-a #x101 :swatch-b #x102 :swatch-c #x103 :swatch-d #x104)
    (is (equalp record buf))
    (is (equalp '((:index 20 :horizontal-flip 1 :vertical-flip 0) 2 1)
                (multiple-value-list (unmarshal tile-cell record nil
                                                :bottom :cycle-cd :top-above))))))

;;; ============================================================
;;; Reading any cell
;;; ============================================================

(def-test a-reader-dispatches-on-the-format-byte ()
  "A reader that dispatches on byte 0 reads each format through its own manifest, and every
format's shared fields read alike through the common view."
  (let ((plain (cell-octets 0 2 4 0  #x34 #x12 7 0  9 0 0 #x04  103 0 0 #x02))
        (inline (cell-octets 2 1 0 0  #x34 #x12 7 1  103 0 0 2))
        (tiles (cell-octets 4 1 #x80 #x22  #x34 #x12 9 #x20  140 0 0 2)))
    (is (equalp '(2 1 1) (mapcar (lambda (r) (getf (read-cell r) :glyph-count))
                                 (list plain inline tiles))))
    (is (equalp '((:plain 1 7 0) (:inline-colours 0 7 1) (:tiles 0 9 0))
                (mapcar (lambda (r) (multiple-value-list
                                     (unmarshal cell-common r nil :format :blink :palette
                                                :background)))
                        (list plain inline tiles))))
    (is (eql #b10000 (unmarshal cell-common tiles nil :format-flags)))))   ; bit 23, top above

(def-test a-cell-read-through-another-format-s-manifest-fails-as-a-constant ()
  "The format byte is a constant in each format's manifest, so a tile cell read as a plain
cell signals manifest-check-failed of kind :constant at unit 0."
  (let ((c (handler-case (unmarshal plain-cell (cell-octets 4 1 0 #x22))
             (manifest-check-failed (c) c))))
    (is (eq :constant (check-kind c)))
    (is (eql 0 (check-position c)))))

(def-test a-cell-written-as-read-is-reproduced ()
  "Each worked record, read through its format's manifest and written back from the plist,
reproduces its bytes."
  (dolist (record (list (cell-octets 0 #x13 0 0  #x34 #x12 7 0  9 0 0 #x04  103 0 0 #x02
                                     #xCB 0 0 #x0A)
                        (cell-octets 4 1 #x80 #x22  #x34 #x12 9 #x20  140 0 0 2  0 0 0 0
                                     0 0 0 0  20 #x40 21 0  1 1 2 1  3 1 4 1)))
    (let ((buf (ub8 32))
          (plist (read-cell record)))
      (ecase (aref record 0)
        (0 (marshal plain-cell buf (:from plist)))
        (4 (marshal tile-cell buf (:from plist))))
      (is (equalp record buf)))))

;;; ============================================================
;;; Records at an index
;;; ============================================================

(def-test a-cell-written-at-an-index-of-a-grid ()
  "(:at index) writes a record at that index of a vector, leaving the rest of the vector as
it was, and returns the position after it, so records can be written one after another."
  (let ((grid (make-array 96 :element-type '(unsigned-byte 8) :initial-element #xEE))
        (cell (cell-octets 0 1 0 0  #x34 #x12 7 1  103 0 0 0)))
    (is (eql 64 (marshal plain-cell grid (:at 32) :glyph-count 1 :swatch #x1234 :palette 7
                                                  :background 1 :g0 (:glyph 103 :ink 0))))
    (is (equalp cell (subseq grid 32 64)))
    (is-true (every (lambda (u) (= u #xEE)) (subseq grid 0 32)))
    (is-true (every (lambda (u) (= u #xEE)) (subseq grid 64 96)))
    (let ((i 0))
      (dotimes (k 3)
        (setf i (marshal plain-cell grid (:at i) :glyph-count 1 :g0 (:glyph (+ 100 k) :ink 1))))
      (is (eql 96 i))
      (is (equalp '(100 101 102)
                  (loop :for k :below 3
                        :collect (getf (unmarshal plain-cell grid (:at (* 32 k)) :g0) :glyph)))))))

(def-test a-cell-read-at-an-index-in-full-and-selectively ()
  "(:at index) reads a record from that index of a vector, in full or one field."
  (let ((grid (ub8 96)))
    (replace grid (cell-octets 0 2 4 0  #x34 #x12 7 0  9 0 0 #x04  103 0 0 #x02) :start1 64)
    (is (eql 2 (getf (unmarshal plain-cell grid (:at 64)) :glyph-count)))
    (is (eql 1 (unmarshal plain-cell grid (:at 64) :blink)))
    (is (equalp '(:plain 7) (multiple-value-list
                             (unmarshal cell-common grid (:at 64) :format :palette))))
    (is (eql 0 (unmarshal cell-common grid (:at 0) :blink)))))

(def-test a-png-at-an-index-has-its-crcs-over-the-units-where-they-lie ()
  "A record with checksum slots written at an index is checksummed over its units where they
lie, reads back with no failed check, and a failure is reported at its position in the
vector."
  (let* ((png (make-1x1-png))
         (v (ub8 (+ 5 (length png)))))
    (is (eql (+ 5 (length png)) (marshal png-file v (:at 5 :from (unmarshal png-file png)))))
    (is (equalp png (subseq v 5)))
    (is (null (failed-checks (lambda () (unmarshal png-file v (:at 5))))))
    (setf (aref v (+ 5 57)) (logxor (aref v (+ 5 57)) 1))     ; IDAT's CRC, at units 56 to 59
    (is (eql (+ 5 56)
             (handler-case (progn (unmarshal png-file v (:at 5)) nil)
               (manifest-check-failed (c) (check-position c)))))))

(def-test offsets-in-a-record-at-an-index-count-from-its-start ()
  "An :offset-of slot holds an offset from the record's start, written and read, so a record
written at an index holds the same units as at 0, and its placed fields are read there."
  (let ((at-zero (ub8 8))
        (at-three (ub8 11)))
    (marshal indexed-archive at-zero :entries (list (list :data #(1 2)) (list :data #(3))))
    (marshal indexed-archive at-three (:at 3) :entries (list (list :data #(1 2)) (list :data #(3))))
    (is (equalp '(2  5 2  7 1  1 2 3) (coerce at-zero 'list)))
    (is (equalp at-zero (subseq at-three 3)))
    (is (equalp '(:n 2 :entries ((:offset 5 :size 2 :data #(1 2)) (:offset 7 :size 1 :data #(3))))
                (unmarshal indexed-archive at-three (:at 3)))))
  (let ((v (ub8 7)))
    (marshal checked-offset-probe v (:at 3) :x 9 :body #x0102)
    (is (equalp '(0 0 0  2 9 1 2) (coerce v 'list)))
    (is (null (failed-checks (lambda () (unmarshal checked-offset-probe v (:at 3))))))))

(def-test a-to-pad-counts-from-the-record-and-an-align-pad-from-the-vector ()
  "A :to pad counts from the record's start and an :align pad from the vector's, so a record
at index 1 is aligned to the vector's boundaries and padded to its own length."
  (let ((v (make-array 14 :element-type '(unsigned-byte 8) :initial-element #xEE)))
    (is (eql 14 (marshal pad-probe v (:at 1) :len 1 :data #(7) :x #x11 :y #x22)))
    (is (equalp '(#xEE  1 7 #xAA  #x11  0 0 0 0 0 0 0 0  #x22) (coerce v 'list)))
    (is (equalp '(:len 1 :data #(7) :x #x11 :y #x22) (unmarshal pad-probe v (:at 1))))))

(def-test a-record-at-an-index-past-the-end-of-its-vector-overruns ()
  "A record that would not fit after its index signals manifest-overrun, whose position is
the unit in the vector that would not fit."
  (let ((c (handler-case (marshal plain-cell (ub8 96) (:at 80))
             (manifest-overrun (c) c))))
    (is (eql 96 (overrun-size c)))
    (is (eql 96 (overrun-position c)))))

(def-test a-record-with-slots-at-an-index-of-a-vector-with-a-fill-pointer ()
  "A record with slots written at an index of a vector that is not simple goes through a
buffer, and lands at that index."
  (let* ((png (make-1x1-png))
         (v (make-array (+ 4 (length png)) :element-type '(unsigned-byte 8) :fill-pointer 0
                                           :initial-element 0)))
    (marshal png-file v (:at 4 :from (unmarshal png-file png)))
    (is (equalp (coerce png 'list)
                (loop :for u :from 4 :below (array-dimension v 0) :collect (aref v u))))
    (is (eql 0 (fill-pointer v)))))

(def-test at-an-index-needs-a-vector-and-an-index ()
  "(:at index) with a destination or a source that is not a vector, or an index that is not
a non-negative integer, is an error: at expansion where the form shows it, and when the code
runs otherwise."
  (signals error (macroexpand-1 '(marshal plain-cell (lambda (u) u) (:at 0))))
  (signals error (macroexpand-1 '(unmarshal plain-cell 0 (:at 0))))
  (signals error (macroexpand-1 '(unmarshal plain-cell '(32 . 0) (:at 0))))
  (let ((f (lambda (u) u)))
    (signals error (marshal plain-cell f (:at 0))))
  (let ((n 0))
    (signals error (unmarshal plain-cell n (:at 0))))
  (let ((i -1))
    (signals error (marshal plain-cell (ub8 32) (:at i)))
    (signals error (unmarshal plain-cell (ub8 32) (:at i)))))

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

;; Records of fixed size with fields a patch may not write: a slot, and a region a checksum
;; covers.
(defmanifest patch-slot-probe (:unit 8 :endian :big)
  (:size :u8 :slot (:length-of :data))
  (:data (:u 1 :vec) :count 2 :default #(0 0))
  (:tag  :u8 :default 0))

(defmanifest patch-sum-probe (:unit 8 :endian :big)
  (:sum  :u8 :slot (:checksum-of :data :by #'byte-sum))
  (:data :u16 :default 0)
  (:tag  :u8 :default 0))

;; A :case field whose selector has an enumeration, with a branch that is a sub-manifest and
;; one that is not, for the errors of paths through it.
(defenum path-branch-kind nil :item 0 :word 1)

(defmanifest path-branch-probe (:unit 8 :endian :big)
  (:kind :u8 :default :item :enumerate-by path-branch-kind)
  (:body (:case :kind (:item (manifest item-probe)) (:word :u16))))

;; Two plain cells side by side, as the halves of a double-width glyph lie, for paths of
;; three names.
(defmanifest cell-pair (:unit 8 :endian :little)
  (manifest :left plain-cell)
  (manifest :right plain-cell))

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

(defun error-message (thunk)
  "The report of the error THUNK signals, or the empty string if it signals none."
  (handler-case (progn (funcall thunk) "")
    (error (e) (princ-to-string e))))

(defun write-sample-cell (buffer index glyph palette)
  "Write at INDEX of BUFFER the plain cell of g underlined in the text's colour, with GLYPH and
PALETTE known only when the code runs, as a terminal writes a character."
  (marshal plain-cell buffer (:at index) :glyph-count 2 :type-1 1 :swatch #x1234
                                         :palette palette :g0 (:glyph glyph :ink 1)
                                         :g1 (:glyph 9 :operation :union)))

(defun patch-sample-cell (buffer index palette)
  "Patch the cell at INDEX of BUFFER: its blink bit, its palette slot, and a mask's reach."
  (marshal plain-cell buffer (:at index :patch t) :blink 1 :palette palette (:g1 :reach) 2))

(defun not-simple (vector)
  "A vector holding VECTOR's units that is not a simple array, so a write to it takes the
general path."
  (make-array (length vector) :element-type (array-element-type vector) :adjustable t
                              :initial-contents vector))

(defun patched-grid ()
  "A 64-byte vector holding at 32 a plain cell of two glyphs, the second a mask: glyph 103 in
ink 1 under glyph 9 as a union, the cell wide, in swatch #x1234 and palette slot 7."
  (let ((v (ub8 64)))
    (replace v (cell-octets 0 #x0A 1 0  #x34 #x12 7 0  103 0 0 #x02  9 0 0 0) :start1 32)
    v))

(defun with-units (vector &rest positions-and-units)
  "A copy of VECTOR with each unit given at its position in POSITIONS-AND-UNITS."
  (let ((v (copy-seq vector)))
    (loop :for (i u) :on positions-and-units :by #'cddr :do (setf (aref v i) u))
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

;;; ============================================================
;;; Paths
;;; ============================================================

(def-test paths-read-fields-inside-sub-manifests-and-case-branches ()
  "A key that is a list of field names reads the field it names inside a sub-manifest or a
:case branch, among keywords, the values in the order asked."
  (let ((record (cell-octets 0 #x0A 0 0  #x34 #x12 7 0  103 0 0 #x02  9 0 0 0)))
    (is (equal '(103 :union 0 1)
               (multiple-value-list
                (unmarshal plain-cell record nil (:g0 :glyph) (:g1 :operation) :blink (:g0 :ink)))))
    (is (eql 0 (unmarshal plain-cell record nil (:g2 :ink))))))

(def-test a-path-through-a-branch-that-lacks-its-field-signals-an-error ()
  "A path whose field the branch found when the code runs lacks signals an error naming the
path, the branch found, and the selector that chose it, with its value."
  (let* ((record (cell-octets 0 #x0A 0 0  #x34 #x12 7 0  103 0 0 #x02  9 0 0 0))
         (message (error-message (lambda () (unmarshal plain-cell record nil (:g1 :ink))))))
    (is-true (search "(:G1 :INK)" message))
    (is-true (search "manifest GLYPH-MASK" message))
    (is-true (search ":TYPE-1 = 1" message))))

(def-test a-branch-error-names-the-branch-at-any-depth-and-through-an-enumeration ()
  "The error of a path through a branch that lacks its field names the branch where it was
found two manifests deep, and names a branch that is not a sub-manifest as such, with the
selector's value as its enumeration gives it."
  (let* ((v (ub8 72))
         (deep (progn (replace v (cell-octets 0 #x0A 0 0  #x34 #x12 7 0  104 0 0 2  9 0 0 #x08)
                               :start1 40)
                      (error-message (lambda () (unmarshal cell-pair v (:at 8) (:right :g1 :ink))))))
         (scalar (error-message (lambda () (unmarshal path-branch-probe (cell-octets 1 5 6) nil
                                                      (:body :tag))))))
    (is-true (search "(:RIGHT :G1 :INK)" deep))
    (is-true (search "manifest GLYPH-MASK" deep))
    (is-true (search ":TYPE-1 = 1" deep))
    (is-true (search "a branch that is not a sub-manifest" scalar))
    (is-true (search ":KIND = :WORD" scalar))
    (is (eql 6 (unmarshal path-branch-probe (cell-octets 0 5 0 6) nil (:body :value))))))

(def-test a-path-of-three-names-at-an-index ()
  "A path may name a field two manifests deep, through a sub-manifest and a :case branch, in
a record read at an index."
  (let ((v (ub8 72)))
    (replace v (cell-octets 0 2 0 0  #x34 #x12 7 0  103 0 0 2  9 0 0 #x04) :start1 8)
    (replace v (cell-octets 0 #x0A 0 0  #x34 #x12 7 0  104 0 0 2  9 0 0 #x08) :start1 40)
    (is (equal '(2 :union 1 104)
               (multiple-value-list
                (unmarshal cell-pair v (:at 8) (:left :g1 :ink) (:right :g1 :operation)
                           (:right :g1 :reach) (:right :g0 :glyph)))))))

(def-test a-path-that-names-no-field-is-an-expansion-error ()
  "A path whose names are not fields along it, or that passes through a scalar or into a
repeat, is an error when the code is expanded."
  (dolist (form '((unmarshal plain-cell v nil (:g1 :nothing))
                  (unmarshal plain-cell v nil (:blink :x))
                  (unmarshal counted-items-probe v nil (:items :tag))
                  (unmarshal plain-cell v nil (:g1 "ink"))))
    (signals error (macroexpand-1 form))))

;;; ============================================================
;;; Patches
;;; ============================================================

(def-test a-patch-writes-only-the-fields-it-is-given ()
  "(:patch t) writes the fields given into a record already in a vector, leaving every other
unit and every other group of a masque as it was, and returns the position after the record."
  (let ((v (patched-grid)))
    (is (eql 64 (marshal plain-cell v (:at 32 :patch t) :blink 1 :palette 9)))
    (is (equalp (with-units (patched-grid) 34 5 38 9) v)))
  (let ((v (patched-grid)))
    (is (eql 40 (marshal cell-common v (:at 32 :patch t) :swatch #x4321)))
    (is (equalp (with-units (patched-grid) 36 #x21 37 #x43) v))))

(def-test a-patch-writes-fields-inside-sub-manifests-by-paths-and-nested-pairs ()
  "A path or a sub-manifest's nested pairs in a patch write only the fields they give within
it, and a path and pairs may give fields of the same sub-manifest together."
  (let ((v (patched-grid)))
    (marshal plain-cell v (:at 32 :patch t) (:g0 :ink) 3)
    (is (equalp (with-units (patched-grid) 43 6) v)))
  (let ((v (patched-grid)))
    (marshal plain-cell v (:at 32 :patch t) :g0 (:glyph 200))
    (is (equalp (with-units (patched-grid) 40 200) v)))
  (let ((v (patched-grid)))
    (marshal plain-cell v (:at 32 :patch t) :g0 (:glyph 200) (:g0 :ink) 0)
    (is (equalp (with-units (patched-grid) 40 200 43 0) v))))

(def-test a-patch-writes-a-branch-chosen-by-the-selector-given-or-in-the-vector ()
  "A field through a :case branch is written in the branch its selector chooses: the
selector's value given in the patch when there is one, and otherwise the selector as it
stands in the vector."
  (let ((v (patched-grid)))
    (marshal plain-cell v (:at 32 :patch t) (:g1 :reach) 2)
    (is (equalp (with-units (patched-grid) 47 #x10) v))
    (is (eql 2 (unmarshal plain-cell v (:at 32) (:g1 :reach)))))
  (let ((v (patched-grid)))
    (marshal plain-cell v (:at 32 :patch t) :type-1 0 (:g1 :ink) 2)
    (is (equalp (with-units (patched-grid) 33 2 47 4) v))
    (is (eql 2 (unmarshal plain-cell v (:at 32) (:g1 :ink))))))

(def-test a-patch-through-a-branch-that-lacks-its-field-signals-an-error ()
  "A patch giving a field the branch chosen when the code runs lacks signals an error naming
the field, the branch and the selector's value; one no branch has is an expansion error."
  (let* ((v (patched-grid))
         (message (error-message (lambda () (marshal plain-cell v (:at 32 :patch t) (:g1 :ink) 2)))))
    (is-true (search ":INK" message))
    (is-true (search "manifest GLYPH-MASK" message))
    (is-true (search ":TYPE-1 = 1" message))
    (is (equalp (patched-grid) v)))
  (signals error (macroexpand-1 '(marshal plain-cell v (:patch t) (:g1 :nothing) 2))))

(def-test a-patch-of-a-wider-masque-keeps-its-other-groups ()
  "A patch of groups in a 16-bit masque, a tile's flips and index, writes those groups and
keeps the rest of both units."
  (let* ((tiles (cell-octets 4 1 #x80 #x22  #x34 #x12 9 #x20  140 0 0 2  0 0 0 0  0 0 0 0
                             20 #x40 21 0))
         (v (copy-seq tiles)))
    (is (eql 32 (marshal tile-cell v (:patch t) (:bottom :vertical-flip) 1 (:top :index) 300)))
    (is (equalp (with-units tiles 21 #xC0 22 #x2C 23 #x01) v))
    (is (equalp '(20 1 1 300 0)
                (multiple-value-list
                 (unmarshal tile-cell v nil (:bottom :index) (:bottom :horizontal-flip)
                            (:bottom :vertical-flip) (:top :index) (:top :horizontal-flip)))))))

(def-test patches-chain-by-the-positions-they-return ()
  "Each patch returns its record's end, so patches of records one after another can be
chained."
  (let ((v (ub8 96))
        (i 0))
    (dotimes (k 3)
      (setf i (marshal plain-cell v (:at i :patch t) :palette (+ 5 k))))
    (is (eql 96 i))
    (is (equal '(5 6 7) (loop :for k :below 3 :collect (aref v (+ 6 (* 32 k))))))))

(def-test a-patch-past-the-end-of-its-vector-writes-nothing ()
  "A patch whose record would not fit within its vector signals manifest-overrun, at the
record's end, before it writes any unit."
  (let* ((v (patched-grid))
         (c (handler-case (marshal plain-cell v (:at 48 :patch t) :blink 1 :palette 9)
              (manifest-overrun (c) c))))
    (is (eql 64 (overrun-size c)))
    (is (eql 80 (overrun-position c)))
    (is (equalp (patched-grid) v))))

(def-test a-patch-of-a-record-with-slots-writes-its-other-fields ()
  "A patch of a record with slots writes the fields given and leaves the slots as they are."
  (let ((v (octets 2 1 2 0  0 4 5 6)))
    (marshal patch-slot-probe v (:patch t) :tag 9)
    (is (equalp (octets 2 1 2 9  0 4 5 6) v))
    (marshal patch-sum-probe v (:at 4 :patch t) :tag 7)
    (is (equalp (octets 2 1 2 9  0 4 5 7) v))
    (marshal fixed-items-probe v (:at 1 :patch t) :after 3)
    (is (equalp (octets 2 1 2 9  0 4 5 3) v))))

(def-test what-a-patch-may-not-write-is-an-expansion-error ()
  "A patch with (:from record), of a manifest of varying size, or giving a slot, a field a
checksum covers, a repeat, a field the manifest lacks or a field twice, is an error when the
code is expanded; so is a path in a full write."
  (dolist (form '((marshal plain-cell v (:patch t :from r))
                  (marshal png-file v (:patch t) :signature #(1))
                  (marshal patch-slot-probe v (:patch t) :size 2)
                  (marshal patch-sum-probe v (:patch t) :data 2)
                  (marshal fixed-items-probe v (:patch t) :items nil)
                  (marshal plain-cell v (:patch t) :nothing 1)
                  (marshal plain-cell v (:patch t) (:g0 :ink) 1 :g0 (:ink 2))
                  (marshal plain-cell v (:patch t) :blink 1 :blink 0)
                  (marshal plain-cell v (:g0 :ink) 1)
                  (marshal plain-cell v nil (:g0 :ink) 1)))
    (signals error (macroexpand-1 form))))

(def-test a-patch-needs-a-vector ()
  "A patch into a destination that is not a vector is an error: at expansion where the form
shows it, and when the code runs otherwise."
  (signals error (macroexpand-1 '(marshal plain-cell (lambda (u) u) (:patch t) :blink 1)))
  (let ((f (lambda (u) u)))
    (signals error (marshal plain-cell f (:patch t) :blink 1))))

;;; ============================================================
;;; The fast path
;;; ============================================================

#+sbcl
(def-test a-cell-written-or-patched-in-a-simple-vector-allocates-nothing ()
  "A full write, a write at an index and a patch of a cell in a simple vector of octets, the
fast path, allocate nothing."
  (let ((grid (ub8 64)))
    (write-sample-cell grid 0 103 7)
    (write-sample-cell grid 32 104 8)
    (patch-sample-cell grid 32 9)
    (flet ((consed (thunk)
             (let ((before (sb-ext:get-bytes-consed)))
               (dotimes (n 1000) (funcall thunk))
               (- (sb-ext:get-bytes-consed) before))))
      (is (eql 0 (consed (lambda () (write-sample-cell grid 0 103 7)))))
      (is (eql 0 (consed (lambda () (write-sample-cell grid 32 104 8)))))
      (is (eql 0 (consed (lambda () (patch-sample-cell grid 32 9))))))))

(def-test the-fast-and-the-general-path-write-the-same-units ()
  "A record of fixed size writes the same units to a simple vector, by the fast path, as to a
vector that is not simple or to a function, by the general path: in full, at an index, and
patched."
  (let ((records (list (cell-octets 0 #x0A 4 0  #x34 #x12 7 0  103 0 0 #x02  9 0 0 0)
                       (cell-octets 0 #x0A 0 0  #x34 #x12 7 1  #x67 0 0 #x02  #x67 0 0 #x0D))))
    (dolist (record records)
      (let* ((cell (unmarshal plain-cell record))
             (fast (ub8 40))
             (general (not-simple (ub8 40)))
             (units nil))
        (marshal plain-cell fast (:at 8 :from cell))
        (marshal plain-cell general (:at 8 :from cell))
        (marshal plain-cell (lambda (u) (push u units)) (:from cell))
        (is (equalp record (subseq fast 8)))
        (is (equalp fast general))
        (is (equalp record (coerce (reverse units) '(vector (unsigned-byte 8)))))
        (patch-sample-cell fast 8 3)
        (patch-sample-cell general 8 3)
        (is (equalp fast general)))))
  (let* ((tiles (cell-octets 4 1 #x80 #x22  #x34 #x12 9 #x20  140 0 0 2  0 0 0 0
                             0 0 0 0  20 #x40 21 0))
         (cell (unmarshal tile-cell tiles))
         (fast (ub8 32))
         (general (not-simple (ub8 32))))
    (marshal tile-cell fast (:from cell))
    (marshal tile-cell general (:from cell))
    (is (equalp tiles fast))
    (is (equalp fast general))))

(def-test an-overrun-on-the-fast-path-writes-nothing-and-reports-as-the-general-path-does ()
  "A record that would not fit signals manifest-overrun with the same size and position on
the fast path as on the general path, the first unit that would not fit, but the fast path
writes none of the record."
  (dolist (at '(20 50))
    (let* ((fast (make-array 40 :element-type '(unsigned-byte 8) :initial-element #xEE))
           (general (not-simple fast))
           (f (handler-case (write-sample-cell fast at 103 7) (manifest-overrun (c) c)))
           (g (handler-case (write-sample-cell general at 103 7) (manifest-overrun (c) c))))
      (is (eql 40 (overrun-size f)))
      (is (eql (overrun-size g) (overrun-size f)))
      (is (eql (max at 40) (overrun-position f)))
      (is (eql (overrun-position g) (overrun-position f)))
      (is-true (every (lambda (u) (= u #xEE)) fast)))))

(def-test a-patch-checks-every-branch-before-it-writes ()
  "A patch whose key the branch chosen lacks signals an error before it writes any unit, even
when it gives fields before that branch, and at any depth of sub-manifests."
  (let ((v (patched-grid)))
    (signals error (marshal plain-cell v (:at 32 :patch t) :blink 1 :palette 9 (:g1 :ink) 2))
    (is (equalp (patched-grid) v)))
  (let ((v (ub8 72)))
    (replace v (cell-octets 0 2 0 0  #x34 #x12 7 0  103 0 0 2  9 0 0 #x04) :start1 8)
    (replace v (cell-octets 0 #x0A 0 0  #x34 #x12 7 0  104 0 0 2  9 0 0 #x08) :start1 40)
    (let ((before (copy-seq v)))
      (signals error (marshal cell-pair v (:at 8 :patch t) (:left :blink) 1 (:right :g1 :ink) 2))
      (is (equalp before v))
      (marshal cell-pair v (:at 8 :patch t) (:left :blink) 1 (:right :g1 :reach) 3)
      (is (equalp '(1 3) (multiple-value-list
                          (unmarshal cell-pair v (:at 8) (:left :blink) (:right :g1 :reach))))))))

(def-test a-selector-a-patch-gives-as-a-form-is-evaluated-once ()
  "A selector's value that a patch gives as a form, which chooses the branch its other keys
are checked against, is evaluated once."
  (let ((v (patched-grid))
        (evaluations 0))
    (marshal plain-cell v (:at 32 :patch t) :type-1 (progn (incf evaluations) 0) (:g1 :ink) 2)
    (is (eql 1 evaluations))
    (is (eql 2 (unmarshal plain-cell v (:at 32) (:g1 :ink))))))

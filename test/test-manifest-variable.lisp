;;;; test/test-manifest-variable.lisp — Sub-manifests chosen by :case and repeated by count, to
;;;; a length or until a condition. Pure: no files.

(in-package #:specops-tests)

(in-suite manifest-variable)

;;; ============================================================
;;; Probe manifests and helpers
;;; ============================================================

;; :until :length with no length slot over the field.
(defmanifest unsized-items-probe (:unit 8 :endian :big)
  (manifest :items item-probe :until :length))

(defmanifest count-of-vector-probe (:unit 8 :endian :big)
  (:n    :u16 :slot (:count-of :data))
  (:data (:u 2 :vec) :count :n))

;; A :count-of slot over a field that has no elements.
(defmanifest count-of-scalar-probe (:unit 8 :endian :big)
  (:n :u8 :slot (:count-of :x))
  (:x :u8 :default 0))

;; Repeats a field plan rejects.
(defmanifest repeat-both-probe (:unit 8 :endian :big)
  (manifest :items item-probe :count 2 :until (:tag 0)))

(defmanifest repeat-unknown-probe (:unit 8 :endian :big)
  (manifest :items item-probe :times 2))

(defmanifest repeat-no-field-probe (:unit 8 :endian :big)
  (manifest :items item-probe :until (:kind 0)))

;;; ============================================================
;;; Tests
;;; ============================================================

;; A :case field whose branch is a sub-manifest.
(def-test png-chunk-an-ihdr-chunk-written-with-a-constant-selector ()
  "png-chunk, an IHDR chunk written with a constant selector and nested values."
  (let* ((png (make-1x1-png))
         (ihdr (subseq png 8 33)))
    (is (equalp ihdr
                (let ((buf (ub8 25)))
                  (marshal png-chunk buf :type :ihdr :data (:width 1 :height 1 :color-type :rgb))
                  buf)))))

(def-test png-chunk-an-idat-chunk-takes-the-raw-branch ()
  "png-chunk, an IDAT chunk takes the raw branch."
  (let* ((png (make-1x1-png))
         (idat (subseq png 33 60)))
    (is (equalp idat
                (let ((buf (ub8 27)))
                  (marshal png-chunk buf :type :idat :data (subseq idat 8 23))
                  buf)))))

(def-test png-chunk-the-ihdr-chunk-read-back-its-data-read-by-the-sub ()
  "png-chunk, the IHDR chunk read back: its data read by the sub-manifest."
  (let* ((png (make-1x1-png))
         (ihdr (subseq png 8 33)))
    (is (equalp (list :length 13 :type :ihdr
                      :data '(:width 1 :height 1 :bit-depth 8 :color-type :rgb
                              :compression 0 :filter 0 :interlace :none)
                      :crc (rd-u32-be ihdr 21))
                (unmarshal png-chunk ihdr)))))

(def-test png-chunk-the-idat-and-iend-chunks-read-back-as-raw-data ()
  "png-chunk, the IDAT and IEND chunks read back as raw data."
  (let* ((png (make-1x1-png))
         (idat (subseq png 33 60))
         (iend (subseq png 60 72)))
    (is (equalp (subseq idat 8 23)
                (getf (unmarshal png-chunk idat) :data)))
    (is (equalp (list :length 0 :type :iend :data #() :crc (rd-u32-be iend 8))
                (unmarshal png-chunk iend)))))

(def-test png-chunk-a-selector-known-only-at-run-time-chooses-the-sub ()
  "png-chunk, a selector known only at run time chooses the sub-manifest."
  (let* ((png (make-1x1-png))
         (ihdr (subseq png 8 33)))
    (is (equalp ihdr
                (let ((buf (ub8 25)) (type :ihdr)
                      (data (list :width 1 :height 1 :color-type :rgb)))
                  (marshal png-chunk buf :type type :data data)
                  buf)))))

(def-test png-chunk-read-as-a-record-holding-a-png-ihdr-record-and-written ()
  "png-chunk, read as a record holding a png-ihdr record, and written back."
  (let* ((png (make-1x1-png))
         (ihdr (subseq png 8 33)))
    (is (equalp '(png-ihdr 1 t)
                (let* ((r (unmarshal png-chunk ihdr (:as :record))) (buf (ub8 25)))
                  (marshal png-chunk buf (:from r))
                  (list (class-name (class-of (png-chunk-data r)))
                        (png-ihdr-width (png-chunk-data r))
                        (equalp buf ihdr)))))))

;; A chunk type the enum lacks: raw data, and its code read and written back.
(def-test png-chunk-a-text-chunk-reads-with-its-type-s-code-and-raw-data ()
  "png-chunk, a tEXt chunk reads with its type's code and raw data."
  (let ((text (ub8 21)))
    (marshal png-chunk text :type #x74455874
                            :data (->bytes (map 'vector #'char-code
                                                (concatenate 'string "Title" (string (code-char 0))
                                                             "png"))))
    (let ((plist (unmarshal png-chunk text)))
      (is (equalp 9
                  (getf plist :length)))
      (is (equalp #x74455874
                  (getf plist :type)))
      (is (equalp 9
                  (length (getf plist :data))))
      (is-true (= (getf plist :crc) (crc text 4 17))))))

(def-test png-chunk-the-text-chunk-read-and-written-back ()
  "png-chunk, the tEXt chunk read and written back."
  (let ((text (ub8 21)))
    (marshal png-chunk text :type #x74455874
                            :data (->bytes (map 'vector #'char-code
                                                (concatenate 'string "Title" (string (code-char 0))
                                                             "png"))))
    (let ((plist (unmarshal png-chunk text)))
      (is (equalp text
                  (let ((buf (ub8 21)))
                    (marshal png-chunk buf (:from plist))
                    buf))))))

;; Nested values for a branch that is not a sub-manifest.
(def-test png-chunk-nested-values-with-a-constant-selector-choosing-raw ()
  "png-chunk, nested values with a constant selector choosing raw data: an expansion error."
  (signals error
    (macroexpand-1 '(marshal png-chunk d :type :idat
                     :data (:width 1)))))

;; Whole PNGs: chunks repeated until IEND.
(def-test png-any-the-1x1-png-read-as-a-signature-and-a-list-of-three ()
  "png-any, the 1x1 PNG read as a signature and a list of three chunks."
  (let ((png (make-1x1-png)))
    (is (equalp (list +png-signature+ '(:ihdr :idat :iend) 1)
                (let ((plist (unmarshal png-any png)))
                  (list (getf plist :signature) (mapcar (lambda (c) (getf c :type)) (getf plist :chunks))
                        (getf (getf (first (getf plist :chunks)) :data) :width)))))))

(def-test png-any-the-1x1-png-read-and-written-back ()
  "png-any, the 1x1 PNG read and written back."
  (let ((png (make-1x1-png)))
    (is (equalp png
                (let ((buf (ub8 72)))
                  (marshal png-any buf (:from (unmarshal png-any png)))
                  buf)))))

(def-test png-any-the-1x1-png-read-as-records-and-written-back ()
  "png-any, the 1x1 PNG read as records and written back."
  (let ((png (make-1x1-png)))
    (is (equalp '((png-chunk png-chunk png-chunk) t)
                (let ((buf (ub8 72)) (r (unmarshal png-any png (:as :record))))
                  (marshal png-any buf (:from r))
                  (list (mapcar (lambda (c) (class-name (class-of c))) (png-any-chunks r))
                        (equalp buf png)))))))

(def-test png-any-a-png-with-a-text-chunk-and-two-idats-passes ()
  "png-any, a PNG with a tEXt chunk and two IDATs passes the verifier."
  (let ((png (png-with-ancillary-chunks)))
    (is-true (let ((*standard-output* (make-broadcast-stream))) (verify-png png)))))

(def-test png-any-that-png-read-back-as-five-chunks ()
  "png-any, that PNG read back as five chunks."
  (let ((png (png-with-ancillary-chunks)))
    (is (equalp (list :ihdr #x74455874 :idat :idat :iend)
                (mapcar (lambda (c) (getf c :type)) (getf (unmarshal png-any png) :chunks))))))

(def-test png-any-that-png-read-and-written-back ()
  "png-any, that PNG read and written back."
  (let ((png (png-with-ancillary-chunks)))
    (is (equalp png
                (let ((buf (ub8 105)))
                  (marshal png-any buf (:from (unmarshal png-any png)))
                  buf)))))

(def-test png-any-a-source-that-ends-before-iend-signals-an-error ()
  "png-any, a source that ends before IEND signals an error."
  (let ((png (png-with-ancillary-chunks)))
    (signals error
      (unmarshal png-any (subseq png 0 93)))))

(def-test png-any-chunks-without-iend-last-with-iend-early-or-none-at-all ()
  "png-any, chunks without IEND last, with IEND early, or none at all signal errors."
  (flet ((write-chunks (chunks)
           (marshal png-any (ub8 200) :signature +png-signature+ :chunks chunks)))
    (signals error
      (write-chunks (list (list :type :ihdr :data (list :width 1 :height 1)))))
    (signals error
      (write-chunks (list (list :type :iend :data #())
                          (list :type :ihdr :data (list :width 1 :height 1)))))
    (signals error
      (write-chunks nil))))

;; Counts, and the end of the source.
(def-test counted-items-probe-two-instances-counted-by-a-field-read ()
  "counted-items-probe, two instances counted by a field, read and written."
  (is (equalp '((:n 2 :items ((:tag 1 :value 5) (:tag 2 :value 6))) t)
              (let ((bytes (->bytes #(2 1 0 5 2 0 6))))
                (list (unmarshal counted-items-probe bytes)
                      (equalp bytes (->bytes (let ((acc nil))
                                               (marshal counted-items-probe (lambda (b) (push b acc))
                                                        (:from (unmarshal counted-items-probe bytes)))
                                               (reverse acc)))))))))

(def-test fixed-items-probe-a-fixed-count-of-two-with-the-field-after ()
  "fixed-items-probe, a fixed count of two, with the field after lined up."
  (is (equalp '(:items ((:tag 1 :value 5) (:tag 2 :value 6)) :after 9)
              (unmarshal fixed-items-probe (->bytes #(1 0 5 2 0 6 9))))))

(def-test rest-items-probe-instances-to-the-end-of-the-source-read ()
  "rest-items-probe, instances to the end of the source, read and written."
  (is (equalp '(3 t)
              (let ((bytes (->bytes #(9 1 0 5 2 0 6 3 0 7))))
                (list (length (getf (unmarshal rest-items-probe bytes) :items))
                      (equalp bytes (->bytes (let ((acc nil))
                                               (marshal rest-items-probe (lambda (b) (push b acc))
                                                        (:from (unmarshal rest-items-probe bytes)))
                                               (reverse acc)))))))))

(def-test rest-items-probe-items-given-as-records-and-a-vector ()
  "rest-items-probe, items given as records and a vector."
  (is (equalp '(9 1 0 5 2 0 6)
              (let ((acc nil))
                (marshal rest-items-probe (lambda (b) (push b acc))
                         :head 9 :items (vector (make-item-probe :tag 1 :value 5)
                                                (make-item-probe :tag 2 :value 6)))
                (reverse acc)))))

;; Instances read until a length is used.
(def-test sized-items-probe-written-the-length-slot-holds-the-bytes ()
  "sized-items-probe, written: the length slot holds the bytes of the instances."
  (is (equalp '(6 1 0 5 2 0 6 9)
              (let ((buf (ub8 8)))
                (marshal sized-items-probe buf
                         :items (list (list :tag 1 :value 5) (list :tag 2 :value 6)) :after 9)
                (coerce buf 'list)))))

(def-test sized-items-probe-read-back-to-the-end-of-the-length ()
  "sized-items-probe, read back to the end of the length, with the field after lined up."
  (is (equalp '(:size 6 :items ((:tag 1 :value 5) (:tag 2 :value 6)) :after 9)
              (unmarshal sized-items-probe (->bytes #(6 1 0 5 2 0 6 9))))))

(def-test sized-items-probe-a-length-of-0-reads-no-instances ()
  "sized-items-probe, a length of 0 reads no instances."
  (is (equalp '(:size 0 :items nil :after 9)
              (unmarshal sized-items-probe (->bytes #(0 9))))))

(def-test sized-items-probe-an-instance-crossing-the-end-of-the-length ()
  "sized-items-probe, an instance crossing the end of the length signals an error."
  (signals error
    (unmarshal sized-items-probe (->bytes #(4 1 0 5 2 0 6 9)))))

(def-test unsized-items-probe-until-length-with-no-length-slot ()
  "unsized-items-probe, :until :length with no length slot: an expansion error on read."
  (signals error
    (macroexpand-1 '(unmarshal unsized-items-probe s))))

;; Count fields computed on write.
(def-test count-of-items-probe-the-count-field-holds-the-number ()
  "count-of-items-probe, the count field holds the number of instances written."
  (is (equalp '(2 1 0 5 2 0 6 9)
              (let ((buf (ub8 8)))
                (marshal count-of-items-probe buf
                         :items (list (list :tag 1 :value 5) (list :tag 2 :value 6)) :after 9)
                (coerce buf 'list)))))

(def-test count-of-items-probe-read-back-the-count-field-counts-the-repeat ()
  "count-of-items-probe, read back: the count field counts the repeat."
  (is (equalp '(:n 2 :items ((:tag 1 :value 5) (:tag 2 :value 6)) :after 9)
              (unmarshal count-of-items-probe (->bytes #(2 1 0 5 2 0 6 9))))))

(def-test count-of-items-probe-a-record-s-stale-count-is-recomputed-and-no ()
  "count-of-items-probe, a record's stale count is recomputed, and no instances count 0."
  (is (equalp '(1 3 0 4 9)
              (let ((buf (ub8 5)))
                (marshal count-of-items-probe buf
                         (:from (make-count-of-items-probe
                                 :n 7 :items (list (make-item-probe :tag 3 :value 4)) :after 9)))
                (coerce buf 'list))))
  (is (equalp '(0 9)
              (let ((buf (ub8 2)))
                (marshal count-of-items-probe buf :items nil :after 9)
                (coerce buf 'list)))))

(def-test count-of-vector-probe-a-count-of-two-byte-elements-written ()
  "count-of-vector-probe, a count of two-byte elements, written and read back."
  (is (equalp '((0 3 0 1 0 2 0 3) (:n 3 :data #(1 2 3)))
              (let ((buf (ub8 8)))
                (marshal count-of-vector-probe buf :data #(1 2 3))
                (list (coerce buf 'list) (unmarshal count-of-vector-probe buf))))))

(def-test count-of-scalar-probe-a-count-of-slot-over-a-scalar-an-expansion ()
  "count-of-scalar-probe, a :count-of slot over a scalar: an expansion error."
  (signals error
    (macroexpand-1 '(marshal count-of-scalar-probe d :x 1))))

;; Repeats that are expansion errors.
(def-test a-repeat-with-count-and-until-an-unknown-option-or-until ()
  "A repeat with both :count and :until, an unknown option, or :until on a missing field is an
expansion error."
  (signals error (macroexpand-1 '(unmarshal repeat-both-probe s)))
  (signals error (macroexpand-1 '(unmarshal repeat-unknown-probe s)))
  (signals error (macroexpand-1 '(unmarshal repeat-no-field-probe s))))

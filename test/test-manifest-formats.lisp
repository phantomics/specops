;;;; test/test-manifest-formats.lisp — Whole formats written and read with manifests: PNG, WOFF
;;;; and sfnt, and placed fields. Pure: no files.

(in-package #:specops-tests)

(in-suite manifest-formats)

;;; ============================================================
;;; Probe manifests and helpers
;;; ============================================================

;; Entries whose bodies, sub-manifests, are placed after all the entries, 2-aligned.
(defmanifest placed-entry (:unit 8 :endian :big)
  (:at :u8 :slot (:offset-of :body))
  (manifest :body item-probe :place :bodies))

(defmanifest placed-probe (:unit 8 :endian :big)
  (:n :u8 :slot (:count-of :entries))
  (manifest :entries placed-entry :count :n)
  (place :bodies :align 2))

;; A placed vector with its offset, length and checksum, unaligned.
(defmanifest placed-sum-entry (:unit 8 :endian :big)
  (:at   :u8 :slot (:offset-of :data))
  (:len  :u8 :slot (:length-of :data))
  (:sum  :u8 :slot (:checksum-of :data :by #'byte-sum))
  (:data (:u 1 :vec) :place :blobs))

(defmanifest placed-sum-probe (:unit 8 :endian :big)
  (manifest :entries placed-sum-entry :count 2)
  (place :blobs))

;; Placed fields that are expansion errors.
(defmanifest placed-no-offset-probe (:unit 8 :endian :big)
  (:len  :u8 :slot (:length-of :data))
  (:data (:u 1 :vec) :place :x)
  (place :x))

(defmanifest placed-no-directive-probe (:unit 8 :endian :big)
  (:at   :u8 :slot (:offset-of :data))
  (:data (:u 1 :vec) :count 1 :place :nowhere))

(defmanifest placed-slot-after-probe (:unit 8 :endian :big)
  (:data (:u 1 :vec) :count 1 :place :x)
  (:at   :u8 :slot (:offset-of :data))
  (place :x))

(defmanifest placed-in-span-probe (:unit 8 :endian :big)
  (span :s
    (:at   :u8 :slot (:offset-of :data))
    (:data (:u 1 :vec) :count 1 :place :x))
  (place :x))

;; WOFF 1.0: a header, a directory of entries sorted by tag, and the tables, each 4-aligned
;; and padded. The fields describing the uncompressed font are given; the layout is derived.
(defmanifest woff-entry (:unit 8 :endian :big :record t)
  (:tag           (:u 4))
  (:offset        (:u 4) :slot (:offset-of :data))
  (:comp-length   (:u 4) :slot (:length-of :data))
  (:orig-length   (:u 4))
  (:orig-checksum (:u 4))
  (:data          (:u 1 :vec) :place :tables))

(defmanifest woff-file (:unit 8 :endian :big :record t)
  (span :file
    (str "wOFF")
    (:flavor           (:u 4))
    (:length           (:u 4) :slot (:length-of :file))
    (:num-tables       :u16   :slot (:count-of :entries))
    (_                 :u16   :default 0)
    (:total-sfnt-size  (:u 4))
    (:major-version    :u16   :default 0)
    (:minor-version    :u16   :default 0)
    (:meta-offset      (:u 4) :default 0)
    (:meta-length      (:u 4) :default 0)
    (:meta-orig-length (:u 4) :default 0)
    (:priv-offset      (:u 4) :default 0)
    (:priv-length      (:u 4) :default 0)
    (manifest :entries woff-entry :count :num-tables)
    (place :tables :align 4)))

;; An sfnt font, TrueType or OpenType: the source a WOFF packs. Table checksums are given,
;; since head's is computed with its checkSumAdjustment taken as zero.
(defmanifest sfnt-entry (:unit 8 :endian :big :record t)
  (:tag      (:u 4))
  (:checksum (:u 4))
  (:offset   (:u 4) :slot (:offset-of :data))
  (:length   (:u 4) :slot (:length-of :data))
  (:data     (:u 1 :vec) :place :tables))

(defmanifest sfnt-file (:unit 8 :endian :big :record t)
  (:version        (:u 4))
  (:num-tables     :u16 :slot (:count-of :entries))
  (:search-range   :u16)
  (:entry-selector :u16)
  (:range-shift    :u16)
  (manifest :entries sfnt-entry :count :num-tables)
  (place :tables :align 4))

(defun rd-u16-be (v i)
  (logior (ash (aref v i) 8) (aref v (+ i 1))))

(defun tag-code (tag)
  "The code of a 4-character table tag, or TAG itself when it is already a code."
  (if (stringp tag) (reduce (lambda (a c) (+ (ash a 8) (char-code c))) tag :initial-value 0) tag))

(defun table-checksum (tag data)
  "The checksum of the sfnt table DATA with tag TAG, as a font's directory gives it: head's is
computed with its checkSumAdjustment, at 8, taken as zero."
  (ldb (byte 32 0) (- (sfnt-checksum data 0 (length data))
                      (if (and (= (tag-code tag) (tag-code "head")) (>= (length data) 12))
                          (rd-u32-be data 8)
                          0))))

(defun woff-record (tables &key (flavor #x00010000))
  "A plist for woff-file from TABLES, a list of (tag data &optional orig-length orig-checksum),
with the directory sorted by tag, and the original length and checksum of each table those
of its data when not given, as for a table stored uncompressed."
  (let ((entries (sort (mapcar (lambda (table)
                                 (destructuring-bind (tag data &optional orig-length orig-checksum) table
                                   (list :tag (tag-code tag)
                                         :orig-length (or orig-length (length data))
                                         :orig-checksum (or orig-checksum (table-checksum tag data))
                                         :data data)))
                               tables)
                       #'< :key (lambda (e) (getf e :tag)))))
    (list :flavor flavor :entries entries
          :total-sfnt-size (+ 12 (* 16 (length entries))
                              (loop :for e :in entries :sum (* 4 (ceiling (getf e :orig-length) 4)))))))

(defun read-woff (source)
  (unmarshal woff-file source))

(defun write-woff (record)
  "WOFF bytes written by woff-file from RECORD, a plist or a woff-file record."
  (collect-units (lambda (d) (marshal woff-file d (:from record)))))

(defun make-woff (tables &rest args)
  "WOFF bytes written by woff-file from TABLES, as woff-record takes them."
  (write-woff (apply #'woff-record tables args)))

(defun hand-woff (tables order)
  "WOFF bytes built by hand, not through manifests, from TABLES as woff-record takes them,
with the directory sorted by tag and the tables stored in ORDER, a list of their tags."
  (let* ((record (woff-record tables))
         (entries (getf record :entries))
         (n (length entries))
         (out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
         (stored (mapcar (lambda (tag) (find (tag-code tag) entries :key (lambda (e) (getf e :tag)))) order))
         (offsets (let ((pos (+ 44 (* 20 n))))
                    (mapcar (lambda (e) (prog1 (cons e pos) (incf pos (* 4 (ceiling (length (getf e :data)) 4)))))
                            stored)))
         (end (+ 44 (* 20 n) (loop :for e :in stored :sum (* 4 (ceiling (length (getf e :data)) 4))))))
    (flet ((u32 (v) (loop :for s :from 24 :downto 0 :by 8 :do (vector-push-extend (ldb (byte 8 s) v) out)))
           (u16 (v) (vector-push-extend (ldb (byte 8 8) v) out) (vector-push-extend (ldb (byte 8 0) v) out)))
      (u32 #x774F4646) (u32 (getf record :flavor)) (u32 end) (u16 n) (u16 0)
      (u32 (getf record :total-sfnt-size)) (u16 0) (u16 0)
      (dotimes (i 5) (u32 0))
      (dolist (e entries)
        (u32 (getf e :tag)) (u32 (rest (assoc e offsets))) (u32 (length (getf e :data)))
        (u32 (getf e :orig-length)) (u32 (getf e :orig-checksum)))
      (dolist (e stored)
        (let ((d (getf e :data)))
          (loop :for b :across d :do (vector-push-extend b out))
          (dotimes (i (- (* 4 (ceiling (length d) 4)) (length d))) (vector-push-extend 0 out))))
      (->bytes out))))

(defun verify-woff (v)
  "Check the WOFF bytes V by hand, as a user agent must: the signature, length and reserved
field, the metadata and private blocks absent, the directory sorted by tag, totalSfntSize,
the tables 4-aligned, following the directory with only zero padding between them and ending
the file, compressed lengths no greater than original ones, and the checksums of tables
stored uncompressed. Returns a list of the errors found, NIL when there are none."
  (let ((errors nil))
    (flet ((err (fmt &rest args) (push (apply #'format nil fmt args) errors)))
      (if (< (length v) 44)
          (err "shorter than a header")
          (let* ((n (rd-u16-be v 12))
                 (entries (loop :for i :below n :for at := (+ 44 (* 20 i))
                                :collect (loop :for k :below 5 :collect (rd-u32-be v (+ at (* 4 k)))))))
            (unless (= (rd-u32-be v 0) #x774F4646) (err "signature"))
            (unless (= (rd-u32-be v 8) (length v)) (err "length ~a, file ~a" (rd-u32-be v 8) (length v)))
            (unless (zerop (rd-u16-be v 14)) (err "reserved field"))
            (unless (loop :for at :from 24 :below 44 :by 4 :always (zerop (rd-u32-be v at)))
              (err "metadata or private block"))
            (unless (or (< n 2) (apply #'< (mapcar #'first entries))) (err "directory not sorted by tag"))
            (unless (= (rd-u32-be v 16) (+ 12 (* 16 n) (loop :for e :in entries :sum (* 4 (ceiling (fourth e) 4)))))
              (err "totalSfntSize"))
            (let ((pos (+ 44 (* 20 n))))
              (dolist (e (sort (copy-list entries) #'< :key #'second))
                (destructuring-bind (tag offset comp orig sum) e
                  (unless (= offset pos) (err "table ~x at ~a, expected at ~a" tag offset pos))
                  (unless (zerop (mod offset 4)) (err "table ~x not 4-aligned" tag))
                  (when (> comp orig) (err "table ~x compressed past its length" tag))
                  (when (> (+ offset comp) (length v)) (err "table ~x past the end" tag))
                  ;; head's checksum is computed with its checkSumAdjustment, at 8, taken as zero
                  (when (and (= comp orig) (<= (+ offset comp) (length v))
                             (/= sum (ldb (byte 32 0)
                                          (- (sfnt-checksum v offset (+ offset comp))
                                             (if (and (= tag (tag-code "head")) (>= comp 12))
                                                 (rd-u32-be v (+ offset 8))
                                                 0)))))
                    (err "table ~x checksum" tag))
                  (setf pos (+ offset (* 4 (ceiling comp 4))))
                  (unless (loop :for i :from (+ offset comp) :below (min pos (length v)) :always (zerop (aref v i)))
                    (err "table ~x padding" tag))))
              (unless (= pos (length v)) (err "data after the last table"))))))
    (nreverse errors)))

(defun sfnt-record (tables &key (version #x00010000))
  "A plist for sfnt-file from TABLES, a list of (tag data), with the directory sorted by tag,
each table's checksum computed, and the binary-search fields derived from the table count."
  (let* ((entries (sort (mapcar (lambda (table)
                                  (destructuring-bind (tag data) table
                                    (list :tag (tag-code tag) :checksum (table-checksum tag data)
                                          :data data)))
                                tables)
                        #'< :key (lambda (e) (getf e :tag))))
         (n (length entries))
         (selector (if (plusp n) (floor (log n 2)) 0)))
    (list :version version :entries entries :search-range (* 16 (expt 2 selector))
          :entry-selector selector :range-shift (- (* 16 n) (* 16 (expt 2 selector))))))

(defun sfnt->woff-tables (sfnt)
  "The tables of SFNT, a plist read by sfnt-file, as woff-record takes them, keeping their
checksums."
  (mapcar (lambda (e) (list (getf e :tag) (getf e :data) (length (getf e :data)) (getf e :checksum)))
          (getf sfnt :entries)))

(defparameter *woff-tables*
  (list (list "head" (->bytes (loop :for i :below 54 :collect (mod (* 7 i) 256))))
        (list "cmap" (->bytes #(0 0 0 1 0 3 0 1 0 0 0 12 9)))
        (list "OS/2" (->bytes #(0 4 2 88 0 0 0 0)))
        (list "name" (->bytes #(1 2 3 4 5 6)))))

;;; ============================================================
;;; PNG
;;; ============================================================

(def-test png-demo-builds-a-valid-1x1-png ()
  "A 1x1 PNG built from manifests passes the independent structural verifier."
  (is-true (let ((*standard-output* (make-broadcast-stream)))
             (verify-png (make-1x1-png)))))

;;; ============================================================
;;; Placed fields, WOFF and sfnt
;;; ============================================================

;; Placed fields.
(def-test placed-probe-sub-manifest-bodies-placed-after-the-entries-2 ()
  "placed-probe: sub-manifest bodies placed after the entries, 2-aligned, offsets backpatched."
  (let ((entries (list (list :body (list :tag 1 :value 5)) (list :body (list :tag 2 :value 6))))
        (bytes '(2 4 8 0 1 0 5 0 2 0 6 0)))
    (is (equalp (list 12 bytes)
                (let ((v (ub8 12))) (list (marshal placed-probe v :entries entries) (coerce v 'list)))))
    (is (equalp bytes
                (coerce (collect-units (lambda (d) (marshal placed-probe d :entries entries))) 'list)))))

(def-test placed-probe-read-back-each-body-read-at-its-offset ()
  "placed-probe, read back: each body read at its offset."
  (let ((bytes '(2 4 8 0 1 0 5 0 2 0 6 0)))
    (is (equalp '(:n 2 :entries ((:at 4 :body (:tag 1 :value 5)) (:at 8 :body (:tag 2 :value 6))))
                (unmarshal placed-probe (->bytes bytes))))))

(def-test placed-sum-probe-offsets-lengths-and-checksums-of-placed-vectors ()
  "placed-sum-probe: offsets, lengths and checksums of placed vectors, and read back."
  (let ((bytes (->bytes #(6 3 6 9 2 9 1 2 3 4 5))))
    (is (equalp (coerce bytes 'list)
                (coerce (collect-units (lambda (d) (marshal placed-sum-probe d
                                                         :entries (list (list :data #(1 2 3))
                                                                        (list :data #(4 5))))))
                        'list)))
    (is (equalp (list nil '(:entries ((:at 6 :len 3 :sum 6 :data #(1 2 3)) (:at 9 :len 2 :sum 9 :data #(4 5)))))
                (multiple-value-list (failed-checks (lambda () (unmarshal placed-sum-probe bytes))))))))

(def-test placed-sum-probe-a-changed-byte-of-placed-data-fails ()
  "placed-sum-probe: a changed byte of placed data fails its checksum."
  (let ((bytes (->bytes #(6 3 6 9 2 9 1 2 3 4 5))))
    (let ((bad (copy-seq bytes)))
      (setf (aref bad 7) 3)
      (is (equalp '((:checksum :sum))
                  (mapcar (lambda (s) (list (first s) (second s))) (failed-checks (lambda () (unmarshal placed-sum-probe bad)))))))))

(def-test placed-fields-with-no-offset-slot-no-place-directive-a-slot ()
  "Placed fields with no offset slot, no place directive, a slot after them, or in a span:
expansion errors."
  (signals error (macroexpand-1 '(unmarshal placed-no-offset-probe s)))
  (signals error (macroexpand-1 '(marshal placed-no-directive-probe d :data #(1))))
  (signals error (macroexpand-1 '(marshal placed-slot-after-probe d :data #(1))))
  (signals error (macroexpand-1 '(marshal placed-in-span-probe d :data #(1)))))

;; WOFF files written.
(def-test woff-file-a-woff-of-four-tables-passes-the-hand-verifier ()
  "woff-file: a WOFF of four tables passes the hand verifier, and matches one built by hand."
  (let* ((woff (make-woff *woff-tables*)))
    (is-false (verify-woff woff))
    (is (equalp 212
                (length woff)))
    (is (equalp 212
                (rd-u32-be woff 8)))
    (is (equalp 4
                (rd-u16-be woff 12)))
    (is-true (equalp woff (hand-woff *woff-tables* '("OS/2" "cmap" "head" "name"))))))

(def-test woff-file-read-back-the-directory-sorted-by-tag-each-table-s ()
  "woff-file, read back: the directory sorted by tag, each table's data at its offset."
  (let* ((woff (make-woff *woff-tables*))
         (plist (read-woff woff)))
    (is (equalp (mapcar #'tag-code '("OS/2" "cmap" "head" "name"))
                (mapcar (lambda (e) (getf e :tag)) (getf plist :entries))))
    (is-true (every (lambda (e) (equalp (getf e :data)
                                        (second (find (getf e :tag) *woff-tables*
                                                      :key (lambda (tb) (tag-code (first tb)))))))
                    (getf plist :entries)))
    (is (equalp (+ 12 64 88)
                (getf plist :total-sfnt-size)))))

(def-test woff-file-read-and-written-back-through-a-plist-and-through ()
  "woff-file, read and written back, through a plist and through records."
  (let* ((woff (make-woff *woff-tables*))
         (plist (read-woff woff)))
    (is-true (equalp woff (write-woff plist)))
    (is-true (equalp woff (write-woff (unmarshal woff-file woff (:as :record)))))))

;; WOFF files whose tables are stored out of directory order, and compressed tables.
(def-test a-woff-stored-out-of-directory-order-read-with-no-failed-check ()
  "A WOFF stored out of directory order: read with no failed check, each table at its offset."
  (let* ((order '("name" "head" "OS/2" "cmap"))
         (woff (hand-woff *woff-tables* order))
         (plist (multiple-value-list (failed-checks (lambda () (read-woff woff))))))
    (is-false (verify-woff woff))
    (is-false (first plist))
    (is-false (equalp (getf (second plist) :entries) (getf (read-woff (make-woff *woff-tables*)) :entries)
                      ))))

(def-test that-woff-written-back-in-directory-order-valid-the-same-tables ()
  "That WOFF written back in directory order: valid, the same tables, a canonical round trip."
  (let* ((order '("name" "head" "OS/2" "cmap"))
         (woff (hand-woff *woff-tables* order))
         (plist (multiple-value-list (failed-checks (lambda () (read-woff woff)))))
         (rewritten (write-woff (second plist))))
    (is-false (verify-woff rewritten))
    (is-true (equalp rewritten (make-woff *woff-tables*)))
    (is-false (equalp rewritten woff))))

(def-test a-compressed-table-its-original-length-and-checksum-given-read ()
  "A compressed table, its original length and checksum given, read and written back unchanged."
  (let* ((tables (list (list "glyf" (->bytes #(120 156 1 2 3 4 5 6 7 8)) 40 #x12345678)
                       (list "head" (->bytes #(1 2 3 4)))))
         (woff (hand-woff tables '("head" "glyf"))))
    (is-false (verify-woff woff))
    (is (equalp '(10 40 #x12345678)
                (let ((glyf (first (getf (read-woff woff) :entries))))
                  (list (getf glyf :comp-length) (getf glyf :orig-length) (getf glyf :orig-checksum)))))
    (is-true (equalp (write-woff (read-woff woff))
                     (hand-woff tables '("glyf" "head"))))))

;; Streams and selective reads.
(def-test woffs-from-a-stream-offsets-read-backwards-and-forwards-one ()
  "WOFFs from a stream, offsets read backwards and forwards, one after another."
  (let* ((out-of-order (hand-woff *woff-tables* '("name" "head" "OS/2" "cmap")))
         (woff (make-woff *woff-tables*)))
    (is (equalp '(t 212 t 424)
                (with-source-file (concatenate '(vector (unsigned-byte 8)) out-of-order woff)
                  (lambda (in)
                    (list (equalp (read-woff in) (read-woff out-of-order))
                          (file-position in)
                          (equalp (read-woff in) (read-woff woff))
                          (file-position in))))))))

(def-test selective-reads-the-flavor-the-table-count-and-size ()
  "Selective reads: the flavor, the table count and size, and the entries, from vectors and a
stream."
  (let* ((out-of-order (hand-woff *woff-tables* '("name" "head" "OS/2" "cmap")))
         (woff (make-woff *woff-tables*)))
    (is (equalp #x00010000
                (unmarshal woff-file woff nil :flavor)))
    (is (equalp (list 4 164)
                (multiple-value-list (unmarshal woff-file out-of-order nil :num-tables :total-sfnt-size))))
    (is-true (equalp (unmarshal woff-file out-of-order nil :entries)
                     (getf (read-woff out-of-order) :entries)))
    (is (equalp (list #x00010000 212)
                (with-source-file out-of-order
                  (lambda (in) (list (unmarshal woff-file in nil :flavor) (file-position in))))))))

(def-test reading-flavor-reads-the-flavor-the-length-the-count-and-each ()
  "Reading :flavor reads the flavor, the length, the count and each entry's offset and length,
no table data."
  (is (equalp 5
              (reads-outside-guard (macroexpand-1 '(unmarshal woff-file s nil :flavor)))))
  (is (equalp 18
              (reads-outside-guard (macroexpand-1 '(unmarshal woff-file s))))))

(def-test a-wrong-file-length-fails-its-check-a-wrong-signature-fails ()
  "A wrong file length fails its check; a wrong signature fails as a constant."
  (let* ((woff (make-woff *woff-tables*)))
    (let ((bad (copy-seq woff)))
      (setf (aref bad 11) (1+ (aref bad 11)))
      (is (equalp '((:length :length 212 213))
                  (mapcar (lambda (s) (list (first s) (second s) (fourth s) (fifth s)))
                          (failed-checks (lambda () (read-woff bad))))))
      (is (equalp '(:constant)
                  (let ((sig (copy-seq woff)))
                    (setf (aref sig 3) (char-code #\3))
                    (mapcar #'first (failed-checks (lambda () (read-woff sig))))))))))

(def-test sfnt-file-an-sfnt-written-read-back-and-packed-as-a-woff ()
  "sfnt-file: an sfnt written, read back, and packed as a WOFF with the same tables."
  (let* ((sfnt (collect-units (lambda (d) (marshal sfnt-file d (:from (sfnt-record *woff-tables*))))))
         (read (unmarshal sfnt-file sfnt))
         (woff (make-woff (sfnt->woff-tables read) :flavor (getf read :version))))
    (is (equalp (+ 12 64 88)
                (length sfnt)))
    (is (equalp 64
                (getf read :search-range)))
    (is (equalp 2
                (getf read :entry-selector)))
    (is (equalp 0
                (getf read :range-shift)))
    (is-false (verify-woff woff))
    (is-true (equalp woff (make-woff *woff-tables*)))
    (is-true (equalp (mapcar (lambda (e) (getf e :data)) (getf read :entries))
                     (mapcar (lambda (e) (getf e :data)) (getf (read-woff woff) :entries))))))

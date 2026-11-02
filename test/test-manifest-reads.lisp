;;;; test/test-manifest-reads.lisp — Checked reads and their restarts, stream sources and
;;;; selective reads. Stream-source tests write and read temporary files, deleted afterwards.

(in-package #:specops-tests)

(in-suite manifest-reads)

;;; ============================================================
;;; Probe manifests and helpers
;;; ============================================================

;; A length slot over two-byte elements, which a length of an odd number of bytes fails.
(defmanifest checked-wide-probe (:unit 8 :endian :big)
  (:size :u8 :slot (:length-of :data))
  (:data (:u 2 :vec)))

;; A count slot over a repeat that is not counted by it, so the two can disagree.
(defmanifest checked-count-probe (:unit 8 :endian :big)
  (:n :u8 :slot (:count-of :items))
  (manifest :items item-probe :until :end))

;; A checksum read from an integer source.
(defmanifest checked-int-probe (:unit 8 :endian :big)
  (span :body (:a :u8 :default 0) (:b :u8 :default 0))
  (:sum :u8 :slot (:checksum-of :body :by #'byte-sum)))

;; An Adler-32 checksum, through the wrapper with the standard signature.
(defmanifest checked-adler-probe (:unit 8 :endian :big)
  (span :body (:data (:u 1 :vec) :count 4))
  (:sum (:u 4) :default 0 :slot (:checksum-of :body :by #'adler32)))

;; Slots a checked read rejects at expansion.
(defmanifest checked-dangling-probe (:unit 8 :endian :big)
  (:size :u8 :slot (:length-of :nothing)))

(defmanifest checked-no-by-probe (:unit 8 :endian :big)
  (span :body (:a :u8 :default 0))
  (:sum :u8 :slot (:checksum-of :body)))

;; A fixed buffer at the end of a record, whose units after its terminator are not read.
(defmanifest stream-buffer-probe (:unit 8 :endian :big)
  (:name (:u 1 :vec) :count 5 :end-by 0))

(defun tree-count (symbol tree)
  "The number of times SYMBOL appears in TREE."
  (cond ((eq symbol tree) 1)
        ((consp tree) (+ (tree-count symbol (car tree)) (tree-count symbol (cdr tree))))
        (t 0)))

;; Instances repeated until a condition, then a field: passing over the repeat reads only
;; the field its condition tests.
(defmanifest until-then-probe (:unit 8 :endian :big)
  (manifest :items item-probe :until (:tag 0))
  (:after :u8 :default 0))

(defmacro keys-read-apart (manifest source &rest keys)
  "The keys of KEYS for which reading MANIFEST from SOURCE with that key alone gives something
other than a full read gives for it; NIL when every selective read agrees."
  (let ((src (gensym "SRC")) (full (gensym "FULL")))
    `(let* ((,src ,source) (,full (unmarshal ,manifest ,src)))
       (remove nil (list ,@(mapcar (lambda (k) `(unless (equalp (unmarshal ,manifest ,src nil ,k)
                                                                (getf ,full ,k))
                                                    ,k))
                                   keys))))))

;;; ============================================================
;;; Checked reads: checksums, length, offset and count slots, constants
;;; ============================================================

;; Sound files read with no failed check.
(def-test png-file-and-png-any-read-the-1x1-png-and-png-any-the-png ()
  "png-file and png-any read the 1x1 PNG, and png-any the PNG with ancillary chunks, with no
failure."
  (is-false (failed-checks (lambda () (unmarshal png-file (make-1x1-png)))))
  (is-false (failed-checks (lambda () (unmarshal png-any (make-1x1-png)))))
  (is-false (failed-checks (lambda () (unmarshal png-any (png-with-ancillary-chunks))))))

;; A corrupt CRC: the condition, its report, and the two restarts.
(def-test png-file-a-corrupt-idat-crc-signals-manifest-check-failed ()
  "png-file, a corrupt IDAT CRC signals manifest-check-failed, a subclass of error."
  (let ((png (make-1x1-png)))
    (setf (aref png 57) (logxor (aref png 57) 1))  ; IDAT's CRC is at units 56 to 59
    (is (equalp '(t :checksum :crc png-idat-chunk 56)
                (handler-case (progn (unmarshal png-file png) :no-error)
                  (manifest-check-failed (c) (list (typep c 'error) (check-kind c) (check-field c)
                                                   (check-manifest c) (check-position c))))))))

(def-test png-file-the-condition-s-report-names-the-field-the-manifest ()
  "png-file, the condition's report names the field, the manifest, the unit and both values."
  (let ((png (make-1x1-png)))
    (setf (aref png 57) (logxor (aref png 57) 1))  ; IDAT's CRC is at units 56 to 59
    (let ((stored (rd-u32-be png 56))
          (computed (crc png 37 56)))
      (let ((report (handler-case (progn (unmarshal png-file png) "")
                      (manifest-check-failed (c) (princ-to-string c)))))
        (is-true (search "field :CRC" report))
        (is-true (search "manifest PNG-IDAT-CHUNK" report))
        (is-true (search "at unit 56" report))
        (is-true (search (format nil "#x~x" stored) report))
        (is-true (search (format nil ":TYPE+DATA gives #x~x" computed) report))))))

(def-test png-file-continue-keeps-the-stored-crc-and-the-failure-offers ()
  "png-file, CONTINUE keeps the stored CRC, and the failure offers USE-VALUE."
  (let ((png (make-1x1-png)))
    (setf (aref png 57) (logxor (aref png 57) 1))  ; IDAT's CRC is at units 56 to 59
    (let ((stored (rd-u32-be png 56))
          (computed (crc png 37 56)))
      (is (equalp (list (list (list :checksum :crc 'png-idat-chunk computed stored 56 t)) stored)
                  (multiple-value-bind (seen plist) (failed-checks (lambda () (unmarshal png-file png)))
                    (list seen (getf (getf plist :idat) :crc))))))))

(def-test png-file-use-value-with-the-expected-value-gives-the-computed ()
  "png-file, USE-VALUE with the expected value gives the computed CRC, in a record too."
  (let ((png (make-1x1-png)))
    (setf (aref png 57) (logxor (aref png 57) 1))  ; IDAT's CRC is at units 56 to 59
    (let ((computed (crc png 37 56)))
      (is (equalp computed
                  (getf (getf (nth-value 1 (failed-checks (lambda () (unmarshal png-file png))
                                                          'use-value))
                              :idat)
                        :crc)))
      (is (equalp computed
                  (png-idat-chunk-crc
                   (png-file-idat (nth-value 1 (failed-checks
                                                (lambda () (unmarshal png-file png (:as :record)))
                                                'use-value)))))))))

(def-test png-file-check-nil-reads-the-stored-crc-with-no-failure ()
  "png-file, (:check nil) reads the stored CRC with no failure, and leaves the checks out."
  (let ((png (make-1x1-png)))
    (setf (aref png 57) (logxor (aref png 57) 1))  ; IDAT's CRC is at units 56 to 59
    (let ((stored (rd-u32-be png 56)))
      (is (equalp stored
                  (getf (getf (unmarshal png-file png (:check nil)) :idat) :crc)))
      (is-false (tree-mentions-p 'check-failed (macroexpand-1 '(unmarshal png-file s (:check nil)))))
      (is-true (tree-mentions-p 'check-failed (macroexpand-1 '(unmarshal png-file s)))))))

;; A corrupt CRC inside a repeat: only the instance at fault fails.
(def-test png-any-a-byte-changed-in-the-second-of-two-idats-fails ()
  "png-any, a byte changed in the second of two IDATs fails that chunk's CRC alone."
  (let ((png (png-with-ancillary-chunks)))
    (setf (aref png 85) (logxor (aref png 85) #xFF))  ; the second IDAT's data, units 81 to 88
    (is (equalp (list '((:checksum :crc png-chunk 89)) t (list :ihdr #x74455874 :idat :idat :iend))
                (multiple-value-bind (seen plist) (failed-checks (lambda () (unmarshal png-any png)))
                  (list (mapcar (lambda (s) (list (first s) (second s) (third s) (sixth s))) seen)
                        (= (fourth (first seen)) (crc png 77 89))
                        (mapcar (lambda (c) (getf c :type)) (getf plist :chunks))))))))

;; Length slots.
(def-test png-file-an-ihdr-length-of-14-over-a-13-unit-body-fails-and-use ()
  "png-file, an IHDR length of 14 over a 13-unit body fails, and USE-VALUE gives 13."
  (let ((png (make-1x1-png)))
    (setf (aref png 11) 14)  ; IHDR's length, 13, at units 8 to 11
    (is (equalp '(((:length :length png-ihdr-chunk 13 14 8 t)) 13)
                (multiple-value-bind (seen plist) (failed-checks (lambda () (unmarshal png-file png))
                                                                 'use-value)
                  (list seen (getf (getf plist :ihdr) :length)))))))

(def-test checked-wide-probe-a-length-of-3-over-two-unit-elements-reads ()
  "checked-wide-probe, a length of 3 over two-unit elements reads one, and fails."
  (is (equalp '(((:length :size checked-wide-probe 2 3 0 t)) (:size 3 :data #(1)))
              (multiple-value-list (failed-checks (lambda () (unmarshal checked-wide-probe
                                                                        (->bytes #(3 0 1 0 2)))))))))

(def-test checked-wide-probe-a-length-of-4-reads-two-elements-with-no ()
  "checked-wide-probe, a length of 4 reads two elements with no failure."
  (is (equalp '(nil (:size 4 :data #(1 2)))
              (multiple-value-list (failed-checks (lambda () (unmarshal checked-wide-probe
                                                                        (->bytes #(4 0 1 0 2)))))))))

;; Offset slots, the first test of :offset-of in either direction.
(def-test checked-offset-probe-written-the-offset-slot-holds-the-unit ()
  "checked-offset-probe, written: the offset slot holds the unit at which :body begins."
  (let ((buf (ub8 4)))
    (marshal checked-offset-probe buf :x 7 :body 5)
    (is (equalp '(2 7 0 5)
                (coerce buf 'list)))))

(def-test checked-offset-probe-read-back-with-no-failure-and-with-a-wrong ()
  "checked-offset-probe, read back with no failure, and with a wrong offset."
  (let ((buf (ub8 4)))
    (marshal checked-offset-probe buf :x 7 :body 5)
    (is (equalp '(nil (:at 2 :x 7 :body 5))
                (multiple-value-list (failed-checks (lambda () (unmarshal checked-offset-probe buf))))))
    (is (equalp '((:offset :at checked-offset-probe 2 3 0 t))
                (failed-checks (lambda () (unmarshal checked-offset-probe (->bytes #(3 7 0 5)))))))))

;; Count slots.
(def-test checked-count-probe-a-count-that-agrees-with-the-instances-read ()
  "checked-count-probe, a count that agrees with the instances read, and one that does not."
  (is-false (failed-checks (lambda () (unmarshal checked-count-probe (->bytes #(2 1 0 5 2 0 6))))))
  (is (equalp '((:count :n checked-count-probe 2 3 0 t))
              (failed-checks (lambda () (unmarshal checked-count-probe (->bytes #(3 1 0 5 2 0 6))))))))

(def-test count-of-items-probe-a-repeat-counted-by-its-count-of-field ()
  "count-of-items-probe, a repeat counted by its :count-of field needs no check."
  (is-false (tree-mentions-p 'check-failed (macroexpand-1 '(unmarshal count-of-items-probe s)))))

;; A checksum read from an integer source, through a buffer of its units.
(def-test checked-int-probe-integer-sources-sound-corrupt-and-as-a-units ()
  "checked-int-probe, integer sources: sound, corrupt, and as a (units . integer) pair."
  (is (equalp '(nil (:a 1 :b 2 :sum 3))
              (multiple-value-list (failed-checks (lambda () (unmarshal checked-int-probe #x010203))))))
  (is (equalp '((:checksum :sum checked-int-probe 3 4 2 t))
              (failed-checks (lambda () (unmarshal checked-int-probe #x010204)))))
  (is (equalp '(nil (:a 0 :b 2 :sum 2))
              (multiple-value-list (failed-checks (lambda () (unmarshal checked-int-probe
                                                                        (cons 3 #x000202))))))))

;; Adler-32 through the wrapper.
(def-test checked-adler-probe-adler32-written-as-adler32-checksum-gives ()
  "checked-adler-probe writes the Adler-32 that adler32-checksum gives, and reads it back with
no failed check."
  (let ((buf (ub8 8)))
    (marshal checked-adler-probe buf :data #(1 2 3 4))
    (is-true (= (rd-u32-be buf 4) (adler32-checksum (->bytes #(1 2 3 4))) (adler32 buf 0 4)))
    (is (equalp (list nil (list :data #(1 2 3 4) :sum (rd-u32-be buf 4)))
                (multiple-value-list (failed-checks (lambda () (unmarshal checked-adler-probe buf))))))))

(def-test checked-adler-probe-a-changed-data-byte-fails-the-adler-32 ()
  "checked-adler-probe, a changed data byte fails the Adler-32."
  (let ((buf (ub8 8)))
    (marshal checked-adler-probe buf :data #(1 2 3 4))
    (setf (aref buf 2) 9)
    (is (equalp '(:checksum)
                (mapcar #'first (failed-checks (lambda () (unmarshal checked-adler-probe buf))))))))

;; Constants: restartable with CONTINUE only.
(def-test png-file-a-changed-chunk-type-fails-as-a-constant-then-the-crc ()
  "png-file, a changed chunk type fails as a constant, then the CRC over it fails."
  (let ((png (make-1x1-png)))
    (setf (aref png 15) (char-code #\X))  ; "IHDR" becomes "IHDX"
    (is (equalp '((:constant nil 15 nil) (:checksum :crc 29 t))
                (mapcar (lambda (s) (list (first s) (second s) (sixth s) (seventh s)))
                        (failed-checks (lambda () (unmarshal png-file png)) 'use-value))))))

(def-test terminated-probe-a-missing-terminator-fails-as-a-constant ()
  "terminated-probe, a missing terminator fails as a constant, and CONTINUE reads on."
  (is (equalp '(((:constant nil terminated-probe 0 1 2 nil)) (:n 7))
              (multiple-value-list (failed-checks (lambda () (unmarshal terminated-probe (->bytes #(65 66 1 7)))))))))

(def-test masque-probe-a-wrong-static-group-fails-as-a-constant ()
  "masque-probe, a wrong static group fails as a constant, and CONTINUE reads on."
  (is (equalp '(((:constant nil masque-probe 3 4 1 nil)) (:lead 1 :kind 5 :tail 9))
              (multiple-value-list (failed-checks (lambda () (unmarshal masque-probe (->bytes #(1 4 5 9)))))))))

(def-test terminated-probe-and-masque-probe-check-nil-reads-them-with-no ()
  "terminated-probe and masque-probe, (:check nil) reads them with no failure."
  (is (equalp '(nil (:n 7))
              (multiple-value-list (failed-checks (lambda () (unmarshal terminated-probe
                                                                        (->bytes #(65 66 1 7))
                                                                        (:check nil)))))))
  (is (equalp '(nil (:lead 1 :kind 5 :tail 9))
              (multiple-value-list (failed-checks (lambda () (unmarshal masque-probe
                                                                        (->bytes #(1 4 5 9))
                                                                        (:check nil))))))))

;; Expansion errors.
(def-test a-slot-over-a-region-the-manifest-lacks-a-checksum-with-no ()
  "A slot over a region the manifest lacks, a checksum with no :by, and an unknown option are
expansion errors."
  (signals error (macroexpand-1 '(unmarshal checked-dangling-probe s)))
  (signals error (macroexpand-1 '(unmarshal checked-no-by-probe s)))
  (signals error (macroexpand-1 '(unmarshal png-file s (:checks nil)))))

;;; ============================================================
;;; Stream sources: units read as needed, records read in sequence
;;; ============================================================

(def-test png-file-from-a-stream-reads-as-from-a-vector-and-leaves ()
  "png-file from a stream reads as from a vector, and leaves the stream at its end."
  (let* ((png (make-1x1-png)))
    (is (equalp '(t 72)
                (with-source-file png (lambda (in)
                                        (list (equalp (unmarshal png-file in) (unmarshal png-file png))
                                              (file-position in))))))))

(def-test png-any-after-a-3-byte-header-positions-count-from-where-reading ()
  "png-any after a 3-byte header: positions count from where reading began."
  (let* ((ancillary (png-with-ancillary-chunks))
         (header (->bytes #(1 2 3))))
    (is (equalp '(t 108)
                (with-source-file (concatenate '(vector (unsigned-byte 8)) header ancillary)
                  (lambda (in)
                    (read-sequence (make-array 3 :element-type '(unsigned-byte 8)) in)
                    (list (equalp (unmarshal png-any in) (unmarshal png-any ancillary))
                          (file-position in))))))))

(def-test png-any-from-a-stream-as-records-written-back-to-the-same-bytes ()
  "png-any from a stream as records, written back to the same bytes."
  (let* ((ancillary (png-with-ancillary-chunks)))
    (is (equalp ancillary
                (with-source-file ancillary
                  (lambda (in)
                    (collect-units (lambda (d) (marshal png-any d (:from (unmarshal png-any in (:as :record))))))))))))

(def-test item-probe-twice-then-png-file-from-one-stream ()
  "Successive reads from one stream each start where the last ended: two item-probes, then a
png-file."
  (let* ((png (make-1x1-png)))
    (is (equalp '((:tag 1 :value 5) (:tag 2 :value 6) 1 78)
                (with-source-file (concatenate '(vector (unsigned-byte 8)) #(1 0 5 2 0 6) png)
                  (lambda (in)
                    (list (unmarshal item-probe in) (unmarshal item-probe in)
                          (getf (getf (getf (unmarshal png-file in) :ihdr) :png-ihdr) :width)
                          (file-position in))))))))

(def-test goff-end-probe-twice-each-read-takes-its-80-units-trailing-pad ()
  "goff-end-probe twice: each read takes its 80 units, trailing pad included."
  (let ((end (ub8 80)))
    (marshal goff-end-probe end :t-tag 3 :record-count 7)
    (is (equalp '(7 80 7 160 9)
                (with-source-file (concatenate '(vector (unsigned-byte 8)) end end #(9))
                  (lambda (in)
                    (list (getf (unmarshal goff-end-probe in) :record-count) (file-position in)
                          (getf (unmarshal goff-end-probe in) :record-count) (file-position in)
                          (read-byte in))))))))

(def-test pad-length-and-stream-buffer-probe-a-record-ending-in-a-pad ()
  "pad-length and stream-buffer-probe: a record ending in a pad or a buffer is read to its end."
  (is (equalp '((:len 2 :data #(7 8)) 16 (:name #(65)) 42)
              (with-source-file (->bytes #(2 7 8 0 0 0 0 0 0 0 0 0 0 0 0 0  65 0 9 9 9  42))
                (lambda (in)
                  (list (unmarshal pad-length in) (file-position in)
                        (unmarshal stream-buffer-probe in) (read-byte in)))))))

(def-test rest-items-probe-instances-to-the-end-of-the-stream ()
  "rest-items-probe, instances to the end of the stream."
  (is (equalp 3
              (with-source-file (->bytes #(9 1 0 5 2 0 6 3 0 7))
                (lambda (in) (length (getf (unmarshal rest-items-probe in) :items)))))))

(def-test png-any-from-a-stream-that-ends-before-iend-signals-an-error ()
  "png-any from a stream that ends before IEND signals an error."
  (let* ((ancillary (png-with-ancillary-chunks)))
    (with-source-file (subseq ancillary 0 93)
      (lambda (in) (signals error (unmarshal png-any in))))))

(def-test a-terminated-leb-vector-read-element-by-element-from-a-stream ()
  "A terminated LEB vector read element by element from a stream."
  (is (equalp '((:v #(129 5) :after 9) 4)
              (with-source-file (->bytes #(#x81 #x01 5 0 9 4))
                (lambda (in) (list (unmarshal leb-vec-terminated-probe in) (read-byte in)))))))

(def-test png-file-from-a-stream-a-corrupt-idat-crc-fails-its-check ()
  "png-file from a stream: a corrupt IDAT CRC fails its check, at the same unit."
  (let* ((png (make-1x1-png)))
    (let ((bad (copy-seq png)))
      (setf (aref bad 57) (logxor (aref bad 57) 1))
      (is (equalp '((:checksum :crc 56))
                  (with-source-file bad
                    (lambda (in) (mapcar (lambda (s) (list (first s) (second s) (sixth s)))
                                         (failed-checks (lambda () (unmarshal png-file in)))))))))))

(def-test a-manifest-buffer-keeps-its-array-across-reads-and-across ()
  "A manifest-buffer keeps its array across reads, and across a write."
  (let* ((png (make-1x1-png))
         (ancillary (png-with-ancillary-chunks)))
    (let ((holder (make-manifest-buffer)))
      (is (equalp '(t t)
                  (with-source-file (concatenate '(vector (unsigned-byte 8)) png png)
                    (lambda (in)
                      (unmarshal png-file in (:buffer holder))
                      (let ((array (manifest-buffer-array holder)))
                        (collect-units (lambda (d) (marshal png-any d (:from (unmarshal png-any ancillary)
                                                                       :buffer holder))))
                        (list (equalp (unmarshal png-file in (:buffer holder)) (unmarshal png-file png))
                              (eq array (manifest-buffer-array holder)))))))))))

(def-test a-manifest-buffer-grows-to-a-record-longer-than-its-first-256 ()
  "A manifest-buffer grows to a record longer than its first 256 units."
  (let ((holder (make-manifest-buffer)))
    (is (equalp '(300 t)
                (let ((idat (ub8 312)))
                  (marshal png-idat-chunk idat :data (->bytes (make-array 300 :initial-element 7)))
                  (with-source-file idat
                    (lambda (in)
                      (list (length (getf (unmarshal png-idat-chunk in (:buffer holder)) :data))
                            (>= (length (manifest-buffer-array holder)) 312)))))))))

(def-test only-the-top-level-expansion-wraps-a-stream-however-deep ()
  "Only the top-level expansion wraps a stream, however deep the nesting."
  (is (equalp 1
              (tree-count 'make-stream-source (macroexpand-1 '(unmarshal png-any s)))))
  (is (equalp 1
              (tree-count 'fill-stream-source (macroexpand-1 '(unmarshal png-any s))))))

;;; ============================================================
;;; Selective reads: only the fields asked for, and what locates them
;;; ============================================================

(def-test pngs-png-file-png-any-and-png-chunk-over-an-ihdr-and-an-idat ()
  "Reading one key of png-file, png-any or png-chunk gives what a full read gives for that key."
  (let* ((png (make-1x1-png))
         (ancillary (png-with-ancillary-chunks))
         (ihdr (subseq png 8 33)))
    (is (null (keys-read-apart png-file png :signature :ihdr :idat :iend)))
    (is (null (keys-read-apart png-any ancillary :signature :chunks)))
    (is (null (keys-read-apart png-chunk ihdr :length :type :data :crc)))
    (is (null (keys-read-apart png-chunk (subseq png 33 60) :length :type :data :crc)))))

(def-test terminated-multi-element-user-rule-and-fixed-buffer-fields ()
  "Reading one key gives what a full read gives, past terminated, multi-element, user-rule and
fixed-buffer fields."
  (is (null (keys-read-apart cstring-probe (->bytes #(72 105 0 9)) :s :after)))
  (is (null (keys-read-apart crlf-probe (->bytes #(65 66 13 10 9)) :line :after)))
  (is (null (keys-read-apart quic-probe (->bytes #(#x25 #x7B #xBD 9)) :a :b :after)))
  (is (null (keys-read-apart bounded-probe (->bytes #(65 0 9 9 9 7)) :name :after)))
  (is (null (keys-read-apart stop-at-probe
                             (collect-units (lambda (d) (marshal stop-at-probe d
                                                                :v #(1 2) :after 9)))
                             :v :after))))

(def-test leb-leb-vector-case-and-masque-fields-passed-over ()
  "Reading one key gives what a full read gives, past LEB, LEB vector, :case and masque fields."
  (is (null (keys-read-apart leb-probe (->bytes #(#x81 #x01 9)) :n :after)))
  (is (null (keys-read-apart leb-vec-counted-probe (->bytes #(2 #x81 #x01 5 9))
                             :n :v :after)))
  (is (null (keys-read-apart leb-vec-terminated-probe (->bytes #(#x81 #x01 5 0 9))
                             :v :after)))
  (is (null (keys-read-apart case-probe (->bytes #(1 0 5 9)) :kind :v :after)))
  (is (null (keys-read-apart case-probe (->bytes #(4 #x81 #x01 9)) :kind :v :after)))
  (is (null (keys-read-apart case-enum-probe (->bytes #(1 0 5 9)) :kind :v :after)))
  (is (null (keys-read-apart masque-probe (->bytes #(1 3 5 9)) :lead :kind :tail))))

(def-test repeats-sub-manifests-pads-and-slots-passed-over ()
  "Reading one key gives what a full read gives, past repeats, sub-manifests, pads and slots."
  (is (null (keys-read-apart counted-items-probe (->bytes #(2 1 0 5 2 0 6)) :n :items)))
  (is (null (keys-read-apart fixed-items-probe (->bytes #(1 0 5 2 0 6 9)) :items :after)))
  (is (null (keys-read-apart sized-items-probe (->bytes #(6 1 0 5 2 0 6 9))
                             :size :items :after)))
  (is (null (keys-read-apart rest-items-probe (->bytes #(9 1 0 5 2 0 6 3 0 7)) :head :items)))
  (is (null (keys-read-apart until-then-probe (->bytes #(1 0 5 0 0 6 9)) :items :after)))
  (is (null (keys-read-apart checked-count-probe (->bytes #(2 1 0 5 2 0 6)) :n :items)))
  (is (null (keys-read-apart nest-probe
                             (collect-units (lambda (d) (marshal nest-probe d :marker 1
                                                                :ihdr (:width 1 :height 1)
                                                                :trailer 9)))
                             :marker :ihdr :trailer)))
  (is (null (keys-read-apart pad-probe
                             (collect-units (lambda (d) (marshal pad-probe d :len 2
                                                                :data #(1 2) :x 3 :y 4)))
                             :len :data :x :y)))
  (is (null (keys-read-apart goff-end-probe
                             (let ((v (ub8 80)))
                               (marshal goff-end-probe v :t-tag 3 :record-count 7)
                               v)
                             :t-tag :entry-flag :record-count :entry-offset))))

(def-test several-keys-come-back-in-the-order-asked-and-a-sub-manifest-key ()
  "Several keys come back in the order asked, and a sub-manifest key gives its whole record."
  (let* ((png (make-1x1-png))
         (ihdr (subseq png 8 33)))
    (is (equalp (list (rd-u32-be ihdr 21) :ihdr)
                (multiple-value-list (unmarshal png-chunk ihdr nil :crc :type))))
    (is-true (equalp (unmarshal png-file png nil :ihdr) (getf (unmarshal png-file png) :ihdr)))))

(def-test deserializer-calls-in-selective-expansions-outside-the-stream-s ()
  "A selective read calls deserializers only for the fields it must read to reach those asked
for: fewer than a full read; what follows them is passed over only when reading a stream."
  (is (equalp '(1 2 1 9 1 2)
              (mapcar (lambda (form) (reads-outside-guard (macroexpand-1 form)))
                      '((unmarshal png-file s nil :signature)    ; the signature's loop
                        (unmarshal png-chunk s nil :type)        ; :length, :type; :data by its length
                        (unmarshal nest-probe s nil :trailer)    ; the IHDR body by its fixed widths
                        (unmarshal nest-probe s)                 ; a full read, for comparison
                        (unmarshal masque-probe s nil :tail)
                        (unmarshal cstring-probe s nil :after))))))

(def-test a-vector-cut-short-after-the-fields-asked-for-still-reads-them ()
  "A vector cut short after the fields asked for still reads them."
  (let* ((png (make-1x1-png))
         (ihdr (subseq png 8 33)))
    (is (equalp +png-signature+
                (unmarshal png-file (subseq png 0 8) nil :signature)))
    (is (equalp :ihdr
                (unmarshal png-chunk (subseq ihdr 0 8) nil :type)))))

(def-test a-corrupt-crc-unchecked-reading-type-which-precedes-it ()
  "A corrupt CRC: unchecked reading :type, which precedes it, and caught reading :crc."
  (let* ((png (make-1x1-png))
         (ihdr (subseq png 8 33)))
    (let ((bad (copy-seq ihdr)))
      (setf (aref bad 21) (logxor (aref bad 21) 1))   ; the CRC
      (is-false (failed-checks (lambda () (unmarshal png-chunk bad nil :type))))
      (is (equalp '(:checksum)
                  (mapcar #'first (failed-checks (lambda () (unmarshal png-chunk bad nil :crc)))))))))

(def-test a-changed-chunk-type-passed-over-is-not-checked-as-a-constant ()
  "A changed chunk type passed over is not checked as a constant; the CRC over it is."
  (let* ((png (make-1x1-png))
         (ihdr (subseq png 8 33)))
    (let ((bad (copy-seq ihdr)))
      (setf (aref bad 7) (char-code #\X))             ; "IHDR" becomes "IHDX"
      (is-false (failed-checks (lambda () (unmarshal png-ihdr-chunk bad nil :length))))
      (is (equalp '(:checksum)
                  (mapcar #'first (failed-checks (lambda () (unmarshal png-ihdr-chunk bad nil :crc)))))))))

(def-test a-key-the-manifest-lacks-is-an-expansion-error ()
  "A key the manifest lacks is an expansion error."
  (signals error
    (macroexpand-1 '(unmarshal png-file s nil :nope))))

(def-test png-file-signature-from-a-stream-the-record-taken-its-crcs ()
  "png-file :signature from a stream: the record taken, its CRCs unchecked, the next read
whole."
  (let* ((png (make-1x1-png)))
    (let ((bad (copy-seq png)))
      (setf (aref bad 57) (logxor (aref bad 57) 1))   ; the IDAT CRC
      (is (equalp '(nil 72 t)
                  (with-source-file (concatenate '(vector (unsigned-byte 8)) bad png)
                    (lambda (in)
                      (list (failed-checks (lambda () (unmarshal png-file in nil :signature)))
                            (file-position in)
                            (equalp (unmarshal png-file in) (unmarshal png-file png))))))))))

(def-test png-any-signature-and-until-then-probe-after-from-streams-taking ()
  "png-any :signature and until-then-probe :after from streams, taking each record."
  (let* ((ancillary (png-with-ancillary-chunks)))
    (is (equalp (list +png-signature+ 105 9 42)
                (with-source-file (concatenate '(vector (unsigned-byte 8)) ancillary #(1 0 5 0 0 6 9 42))
                  (lambda (in)
                    (list (unmarshal png-any in nil :signature) (file-position in)
                          (unmarshal until-then-probe in nil :after) (read-byte in))))))))

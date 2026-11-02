;;;; test/test-manifest-fields.lisp — Variable-length and typed fields: terminators, LEB128,
;;;; :case, vectors, fixed buffers, user stop rules and floats. Pure: no files.

(in-package #:specops-tests)

(in-suite manifest-fields)

;;; ============================================================
;;; Probe manifests and helpers
;;; ============================================================

;; A fixed-length text field encoded in EBCDIC, with a string default.
(defmanifest codec-probe (:unit 8 :endian :big)
  (:num  :u16 :default 0)
  (:text (:u 1 :vec) :count 5 :default "     "
         :codec #'specops.map.ebcdic:ebcdic-code-cp037))

;; A numeric default is already a code, so it is not passed through the codec.
(defmanifest codec-default-probe (:unit 8 :endian :big)
  (:fill (:u 1 :vec) :count 3 :default 64
         :codec #'specops.map.ebcdic:ebcdic-code-cp037))

;; A constant string with a character the codec does not map (a Tab).
(defmanifest codec-str-probe (:unit 8 :endian :big)
  (str #.(coerce (list #\A #\Tab) 'string)
       :codec #'specops.map.ebcdic:ebcdic-code-cp037))

;; A character terminator is encoded with the field's codec ($ is #x5B in CP037).
(defmanifest ebcdic-terminated-probe (:unit 8 :endian :big)
  (:s     (:u 1 :vec) :end-by #\$ :codec #'specops.map.ebcdic:ebcdic-code-cp037)
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

(defmanifest leb-enum-default-probe (:unit 8 :endian :big)
  (:op (:u 1 :leb) :default :jump :enumerate-by leb-opcode))

;; An unnamed LEB is still read through, so the field after it lines up.
(defmanifest leb-unnamed-probe (:unit 8 :endian :big)
  (_ (:u 1 :leb) :default 300)
  (:k :u8 :default 0))

(defmanifest case-otherwise-probe (:unit 8 :endian :big)
  (:kind  :u8 :default 0)
  (:v     (:case :kind (0 :u8) (otherwise :u16)) :default 0)
  (:after :u8 :default 0))

;; The selector sits outside the span; the length slot counts the chosen width.
(defmanifest case-span-probe (:unit 8 :endian :big)
  (:kind :u8 :default 0)
  (:len  :u8 :slot (:length-of :body))
  (span :body
    (:v (:case :kind (0 :u8) (1 :u32)) :default 0)
    (:k :u8 :default 0)))

;; The :case field's own value is enumerated.
(defmanifest case-value-enum-probe (:unit 8 :endian :big)
  (:kind :u8 :default 0)
  (:op   (:case :kind (0 :u8) (1 :u16)) :default 0 :enumerate-by case-size))

(defmanifest case-value-default-probe (:unit 8 :endian :big)
  (:kind :u8 :default 0)
  (:op   (:case :kind (0 :u8) (1 :u16)) :default :long :enumerate-by case-size))

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

;; A :case branch whose sub-manifest uses a byte order its big-endian parent lacks.
(defmanifest case-le-branch-probe (:unit 8 :endian :big)
  (:kind :u8 :default 0)
  (:body (:case :kind (0 (manifest le-child-probe)) (otherwise :u8))))

;; Two plain fields, for checking each value form is evaluated once.
(defmanifest eval-once-probe (:unit 8 :endian :big)
  (:a :u8 :default 0)
  (:b :u8 :default 0))

;; A terminated field followed by a scalar, for counting element reads.
(defmanifest vec-scan-probe (:unit 8 :endian :big)
  (:s     (:u 1 :vec) :end-by 0)
  (:after :u8 :default 0))

;; Characters go through the codec; integers are raw codes.
(defmanifest vec-codec-mixed-probe (:unit 8 :endian :big)
  (:s (:u 1 :vec) :count 3 :codec #'specops.map.ebcdic:ebcdic-code-cp037))

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

;; An EBCDIC buffer whose character terminator goes through the codec.
(defmanifest bounded-ebcdic-probe (:unit 8 :endian :big)
  (:name (:u 1 :vec) :count 4 :end-by #\$
         :codec #'specops.map.ebcdic:ebcdic-code-cp037))

;; Two-byte little-endian elements: the buffer is three elements, six bytes.
(defmanifest bounded-wide-probe (:unit 8 :endian :big)
  (:s (:u 2 :vec) :count 3 :end-by 0 :endian :little))

;; A length slot over a span counts the whole buffer.
(defmanifest bounded-span-probe (:unit 8 :endian :big)
  (:len :u8 :slot (:length-of :body))
  (span :body
    (:name (:u 1 :vec) :count 4 :end-by 0)
    (:k    :u8 :default 0)))

;; The buffer's size comes from an earlier field.
(defmanifest bounded-counted-probe (:unit 8 :endian :big)
  (:n     :u8 :default 0)
  (:name  (:u 1 :vec) :count :n :end-by 0)
  (:after :u8 :default 0))

;; The same terminator given as a string.
(defmanifest crlf-string-probe (:unit 8 :endian :big)
  (:line  (:u 1 :vec) :end-by #.(coerce (list #\Return #\Newline) 'string))
  (:after :u8 :default 0))

;; A pattern that overlaps itself, for matches that straddle data and terminator.
(defmanifest aa-probe (:unit 8 :endian :big)
  (:s     (:u 1 :vec) :end-by "AA")
  (:after :u8 :default 0))

;; Characters of the terminator go through the codec ($ is #x5B in CP037).
(defmanifest ebcdic-multi-probe (:unit 8 :endian :big)
  (:name (:u 1 :vec) :end-by "$$" :codec #'specops.map.ebcdic:ebcdic-code-cp037))

;; A six-byte buffer ended by CR LF.
(defmanifest bounded-crlf-probe (:unit 8 :endian :big)
  (:line  (:u 1 :vec) :count 6 :end-by #(13 10))
  (:after :u8 :default 0))

(defmanifest empty-terminator-probe (:unit 8 :endian :big)
  (:s (:u 1 :vec) :end-by #()))

(defmanifest sleb-vec-probe (:unit 8 :endian :big)
  (:v (:s 1 :vec :leb) :count 3))

;; Enumerated elements: leb-opcode is :nop 0, :call 200, :jump 300.
(defmanifest leb-vec-enum-probe (:unit 8 :endian :big)
  (:ops (:u 1 :vec :leb) :end-by 0 :enumerate-by leb-opcode))

;; A LEB vector as one branch of a :case field.
(defmanifest leb-vec-case-probe (:unit 8 :endian :big)
  (:kind  :u8 :default 0)
  (:v     (:case :kind (0 :u8) (1 :u 1 :vec :leb)) :end-by 0 :default 0)
  (:after :u8 :default 0))

;; A length slot counts bytes, which cannot give an element count on read.
(defmanifest leb-vec-length-probe (:unit 8 :endian :big)
  (:len :u8 :slot (:length-of :v))
  (:v   (:u 1 :vec :leb)))

(defmanifest leb-vec-bounded-probe (:unit 8 :endian :big)
  (:v (:u 1 :vec :leb) :count 3 :end-by 0))

(defmanifest leb-vec-wide-probe (:unit 8 :endian :big)
  (:v (:u 2 :vec :leb) :count 3))

(defmanifest high-bit-probe (:unit 8 :endian :big)
  (:v     (:u 1 :vec) :stop-by high-bit-run)
  (:after :u8 :default 0))

(defmanifest double-zero-probe (:unit 8 :endian :big)
  (:v     (:u 1 :vec) :stop-by double-zero)
  (:after :u8 :default 0))

(defmanifest stop-at-inline-probe (:unit 8 :endian :big)
  (:v     (:u 1 :vec) :stop-by (stop-at-inline :code 255))
  (:after :u8 :default 0))

(defmanifest foreign-rule-probe (:unit 8 :endian :big)
  (:v     (:u 1 :vec) :stop-by (specops.test.rules:until-code :code 254))
  (:after :u8 :default 0))

(defmanifest stop-by-unknown-probe (:unit 8 :endian :big)
  (:v (:u 1 :vec) :stop-by no-such-rule))

(defmanifest stop-by-with-end-by-probe (:unit 8 :endian :big)
  (:v (:u 1 :vec) :stop-by double-zero :end-by 0))

(defmanifest float-probe (:unit 8 :endian :big :record t)
  (:half   (:f 2) :default 0)
  (:single :f32   :default 0)
  (:double (:f 8) :default 0)
  (:quad   :f128  :default 0))

(defmanifest float-le-probe (:unit 8 :endian :little)
  (:single (:f 4) :default 0))

(defmanifest float-vec-probe (:unit 8 :endian :big)
  (:n     :u8 :slot (:count-of :v))
  (:v     (:f 4 :vec) :count :n)
  (:after :u8 :default 0))

(defmanifest float-case-probe (:unit 8 :endian :big)
  (:kind  :u8 :default 0)
  (:v     (:case :kind (0 :u 4) (1 :f 4) (2 :f 8)) :default 0)
  (:after :u8 :default 0))

;; Two 16-bit units hold a single float: widths count bytes, whatever the unit.
(defmanifest float-word-probe (:unit 16 :endian :big)
  (:x (:f 4) :default 0))

(defmanifest float-bad-width-probe (:unit 8 :endian :big)
  (:x (:f 3) :default 0))

(defun hex-units (v &optional (digits 2))
  "The units of V as one hexadecimal string."
  (format nil "~{~v,'0x~}" (loop :for u :across v :append (list digits u))))

(defun has-case-p (tree)
               (or (eq tree 'case)
                   (and (consp tree) (or (has-case-p (car tree)) (has-case-p (cdr tree))))))

(defun count-reads (tree)
               ;; element reads are calls of a deserializer: (funcall (aref ds …) …)
               (if (consp tree)
                   (+ (if (and (eq (car tree) 'funcall) (consp (second tree))
                               (eq (car (second tree)) 'aref))
                          1 0)
                      (count-reads (car tree)) (count-reads (cdr tree)))
                   0))

(defun mentions-p (tree symbols)
               (if (consp tree)
                   (or (mentions-p (car tree) symbols) (mentions-p (cdr tree) symbols))
                   (and (member tree symbols) t)))

;;; ============================================================
;;; Codecs on :vec fields, and terminated fields (:end-by)
;;; ============================================================

;; Codecs on :vec fields.
(def-test codec-probe-hello-encoded-in-cp037 ()
  "codec-probe, \"Hello\" encoded in CP037."
  (is (equalp '(0 50 #xC8 #x85 #x93 #x93 #x96)
              (to-list (lambda (d) (marshal codec-probe d :num 50 :text "Hello"))))))

(def-test codec-probe-string-default-encoded-ebcdic-spaces ()
  "codec-probe, string default encoded (EBCDIC spaces)."
  (is (equalp '(0 50 #x40 #x40 #x40 #x40 #x40)
              (to-list (lambda (d) (marshal codec-probe d :num 50))))))

(def-test codec-probe-read-back-and-decoded-to-a-string ()
  "codec-probe, read back and decoded to a string."
  (is (equalp '(:num 50 :text "Hello")
              (unmarshal codec-probe (->bytes #(0 50 #xC8 #x85 #x93 #x93 #x96))))))

(def-test codec-default-probe-numeric-default-written-as-is-count-times ()
  "codec-default-probe, numeric default written as is, :count times."
  (is (equalp '(64 64 64)
              (to-list (lambda (d) (marshal codec-default-probe d))))))

(def-test codec-probe-a-character-the-codec-doesn-t-map-signals-an-error ()
  "codec-probe, a character the codec doesn't map signals an error on write."
  (signals error
    (to-list (lambda (d)
               (marshal codec-probe d :num 1
                        :text (coerce (list #\A #\Tab #\B #\C #\D)
                                      'string))))))

(def-test codec-probe-a-code-the-codec-doesn-t-assign-signals-an-error ()
  "codec-probe, a code the codec doesn't assign signals an error on read."
  (signals error
    (unmarshal codec-probe (->bytes #(0 1 #x30 #xC1 #xC1 #xC1 #xC1)))))

(def-test codec-str-probe-unmapped-character-in-a-constant-string-signals ()
  "codec-str-probe, unmapped character in a constant string signals an error."
  (signals error
    (to-list (lambda (d) (marshal codec-str-probe d)))))

;; NUL-terminated strings.
(def-test cstring-probe-abc-then-the-terminator ()
  "cstring-probe, \"abc\" then the terminator."
  (is (equalp '(97 98 99 0 9)
              (to-list (lambda (d) (marshal cstring-probe d :s "abc" :after 9))))))

(def-test cstring-probe-read-back-field-after-the-terminator-lines-up ()
  "cstring-probe reads the data before the NUL terminator, and the field after it lines up."
  (is (equalp '(:s #(97 98 99) :after 9)
              (unmarshal cstring-probe (->bytes #(97 98 99 0 9))))))

(def-test cstring-probe-no-value-writes-the-terminator-alone ()
  "cstring-probe, no value writes the terminator alone."
  (is (equalp '(0 9)
              (to-list (lambda (d) (marshal cstring-probe d :after 9))))))

(def-test cstring-probe-empty-field-read-back ()
  "cstring-probe reads a field holding only its terminator as an empty vector."
  (is (equalp '(:s #() :after 9)
              (unmarshal cstring-probe (->bytes #(0 9))))))

(def-test cstring-probe-data-containing-the-terminator-signals-an-error ()
  "cstring-probe, data containing the terminator signals an error."
  (signals error
    (to-list (lambda (d)
               (marshal cstring-probe d
                        :s (coerce (list #\a (code-char 0) #\b) 'string)
                        :after 9)))))

(def-test cstring-probe-a-source-with-no-terminator-signals-an-error ()
  "cstring-probe, a source with no terminator signals an error."
  (signals error
    (unmarshal cstring-probe (->bytes #(97 98 99)))))

;; Terminator encoded through the codec.
(def-test ebcdic-terminated-probe-written-as-x5b ()
  "ebcdic-terminated-probe encodes its character terminator #\\$ with the field's codec, as
#x5B."
  (is (equalp '(#xC8 #xC9 #x5B 9)
              (to-list (lambda (d) (marshal ebcdic-terminated-probe d :s "HI" :after 9))))))

(def-test ebcdic-terminated-probe-read-back-as-hi ()
  "ebcdic-terminated-probe, read back as \"HI\"."
  (is (equalp '(:s "HI" :after 9)
              (unmarshal ebcdic-terminated-probe (->bytes #(#xC8 #xC9 #x5B 9))))))

;; Two-byte elements and terminator.
(def-test wide-terminated-probe-little-endian-elements-and-a-2-byte ()
  "wide-terminated-probe, little-endian elements and a 2-byte terminator."
  (is (equalp '(#x42 #x41 #x43 0 0 0 9)
              (to-list (lambda (d) (marshal wide-terminated-probe d :s #(#x4142 #x43) :after 9))))))

(def-test wide-terminated-probe-read-back ()
  "wide-terminated-probe reads two-byte little-endian elements up to its two-byte terminator."
  (is (equalp '(:s #(#x4142 #x43) :after 9)
              (unmarshal wide-terminated-probe (->bytes #(#x42 #x41 #x43 0 0 0 9))))))

;; A length slot over a span holding a terminated field.
(def-test terminated-span-probe-length-counts-the-terminator ()
  "terminated-span-probe, length counts the terminator."
  (let ((buf (ub8 5)))
    (marshal terminated-span-probe buf :s "ab" :k 7)
    (is (equalp '(4 97 98 0 7)
                (coerce buf 'list)))))

(def-test terminated-span-probe-read-back ()
  "terminated-span-probe reads back a length that counts the terminator, and the field after
it."
  (let ((buf (ub8 5)))
    (marshal terminated-span-probe buf :s "ab" :k 7)
    (is (equalp '(:len 4 :s #(97 98) :k 7)
                (unmarshal terminated-span-probe buf)))))

;;; ============================================================
;;; LEB128 fields
;;; ============================================================

;; Unsigned.
(def-test leb-probe-624485-then-the-next-field ()
  "leb-probe writes 624485 as the three-byte LEB128 E5 8E 26, followed by the next field."
  (is (equalp '(#xE5 #x8E #x26 9)
              (to-list (lambda (d) (marshal leb-probe d :n 624485 :after 9))))))

(def-test leb-probe-read-back-field-after-the-leb-lines-up ()
  "leb-probe reads a three-byte LEB128, and the field after it lines up."
  (is (equalp '(:n 624485 :after 9)
              (unmarshal leb-probe (->bytes #(#xE5 #x8E #x26 9))))))

(def-test leb-probe-0-127-and-128-at-the-one-byte-boundary ()
  "leb-probe writes 0 and 127 in one byte and 128 in two, at the one-byte boundary."
  (is (equalp '((0 9) (#x7F 9) (#x80 #x01 9))
              (mapcar (lambda (x) (to-list (lambda (d) (marshal leb-probe d :n x :after 9))))
                      '(0 127 128)))))

(def-test leb-probe-no-value-writes-the-default-0 ()
  "leb-probe, no value writes the default 0."
  (is (equalp '(0 0)
              (to-list (lambda (d) (marshal leb-probe d))))))

(def-test leb-probe-a-negative-value-signals-an-error ()
  "leb-probe, a negative value signals an error."
  (signals error
    (to-list (lambda (d) (marshal leb-probe d :n -1)))))

(def-test leb-probe-an-unfinished-leb-at-the-end-of-the-source-signals ()
  "leb-probe, an unfinished LEB at the end of the source signals an error."
  (signals error
    (unmarshal leb-probe (->bytes #(#x80 #x80)))))

;; Signed.
(def-test sleb-probe-123456 ()
  "sleb-probe writes -123456 as the three-byte signed LEB128 C0 BB 78."
  (is (equalp '(#xC0 #xBB #x78 9)
              (to-list (lambda (d) (marshal sleb-probe d :n -123456 :after 9))))))

(def-test sleb-probe-read-back ()
  "sleb-probe reads the signed LEB128 C0 BB 78 as -123456."
  (is (equalp '(:n -123456 :after 9)
              (unmarshal sleb-probe (->bytes #(#xC0 #xBB #x78 9))))))

(def-test sleb-probe-63-64-64-and-65-at-the-sign-bit-boundary ()
  "sleb-probe writes 63 and -64 in one byte and 64 and -65 in two, at the sign-bit boundary."
  (is (equalp '((#x3F 9) (#xC0 #x00 9) (#x40 9) (#xBF #x7F 9))
              (mapcar (lambda (x) (to-list (lambda (d) (marshal sleb-probe d :n x :after 9))))
                      '(63 64 -64 -65)))))

;; Round trips, bignums included.
(def-test leb-probe-unsigned-values-round-trip ()
  "leb-probe, unsigned values round-trip."
  (is (equalp (list 0 1 127 128 300 16383 16384 624485 (expt 2 70))
              (loop :for x :in (list 0 1 127 128 300 16383 16384 624485 (expt 2 70))
                    :collect (getf (unmarshal leb-probe
                                              (->bytes (to-list (lambda (d)
                                                                  (marshal leb-probe d :n x)))))
                                   :n)))))

(def-test sleb-probe-signed-values-round-trip ()
  "sleb-probe, signed values round-trip."
  (is (equalp (list 0 1 -1 63 64 -64 -65 -128 -123456 (expt 2 70) (- (expt 2 70)))
              (loop :for x :in (list 0 1 -1 63 64 -64 -65 -128 -123456 (expt 2 70) (- (expt 2 70)))
                    :collect (getf (unmarshal sleb-probe
                                              (->bytes (to-list (lambda (d)
                                                                  (marshal sleb-probe d :n x)))))
                                   :n)))))

;; A length slot over a span holding a LEB.
(def-test leb-span-probe-length-counts-the-leb-s-two-bytes ()
  "leb-span-probe, length counts the LEB's two bytes."
  (let ((buf (ub8 4)))
    (marshal leb-span-probe buf :n 300 :k 7)
    (is (equalp '(3 #xAC #x02 7)
                (coerce buf 'list)))))

(def-test leb-span-probe-read-back ()
  "leb-span-probe reads back a length that counts the LEB's bytes, and the fields after it."
  (let ((buf (ub8 4)))
    (marshal leb-span-probe buf :n 300 :k 7)
    (is (equalp '(:len 3 :n 300 :k 7)
                (unmarshal leb-span-probe buf)))))

;; Enumerated and unnamed LEBs.
(def-test leb-enum-probe-call-written-as-leb-200 ()
  "leb-enum-probe, :call written as LEB 200."
  (is (equalp '(#xC8 #x01)
              (to-list (lambda (d) (marshal leb-enum-probe d :op :call))))))

(def-test leb-enum-probe-read-back-as-call ()
  "leb-enum-probe, read back as :call."
  (is (equalp '(:op :call)
              (unmarshal leb-enum-probe (->bytes #(#xC8 #x01))))))

(def-test leb-enum-default-probe-the-default-jump-written-as-leb-300 ()
  "leb-enum-default-probe, the default :jump written as LEB 300, and read back."
  (is (equalp '((#xAC #x02) (:op :jump))
              (let ((bytes (to-list (lambda (d) (marshal leb-enum-default-probe d)))))
                (list bytes (unmarshal leb-enum-default-probe (->bytes bytes)))))))

(def-test leb-unnamed-probe-default-300-written ()
  "leb-unnamed-probe writes an unnamed LEB field's default, 300, in two bytes."
  (is (equalp '(#xAC #x02 7)
              (to-list (lambda (d) (marshal leb-unnamed-probe d :k 7))))))

(def-test leb-unnamed-probe-read-skips-the-leb ()
  "leb-unnamed-probe, read skips the LEB."
  (is (equalp '(:k 7)
              (unmarshal leb-unnamed-probe (->bytes #(#xAC #x02 7))))))

;;; ============================================================
;;; :case widths selected by an earlier field
;;; ============================================================

;; Each branch, with a constant selector.
(def-test case-probe-kind-0-writes-one-byte ()
  "case-probe, kind 0 writes one byte."
  (is (equalp '(0 5 9)
              (to-list (lambda (d) (marshal case-probe d :kind 0 :v 5 :after 9))))))

(def-test case-probe-kind-1-writes-two-bytes ()
  "case-probe, kind 1 writes two bytes."
  (is (equalp '(1 1 2 9)
              (to-list (lambda (d) (marshal case-probe d :kind 1 :v #x102 :after 9))))))

(def-test case-probe-kind-3-shares-the-1-3-branch ()
  "case-probe, kind 3 shares the (1 3) branch."
  (is (equalp '(3 1 2 9)
              (to-list (lambda (d) (marshal case-probe d :kind 3 :v #x102 :after 9))))))

(def-test case-probe-kind-2-writes-a-signed-four-byte-value ()
  "case-probe, kind 2 writes a signed four-byte value."
  (is (equalp '(2 #xFF #xFF #xFF #xFE 9)
              (to-list (lambda (d) (marshal case-probe d :kind 2 :v -2 :after 9))))))

(def-test case-probe-kind-4-writes-a-leb ()
  "case-probe, kind 4 writes a LEB."
  (is (equalp '(4 #xAC #x02 9)
              (to-list (lambda (d) (marshal case-probe d :kind 4 :v 300 :after 9))))))

;; A selector known only at run time.
(def-test case-probe-a-run-time-selector-picks-each-branch ()
  "case-probe, a run-time selector picks each branch."
  (is (equalp '((0 5 9) (1 1 2 9) (2 #xFF #xFF #xFF #xFE 9) (3 1 2 9) (4 #xAC #x02 9))
              (loop :for (k v) :in '((0 5) (1 #x102) (2 -2) (3 #x102) (4 300))
                    :collect (to-list (lambda (d) (marshal case-probe d :kind k :v v :after 9)))))))

(def-test case-probe-a-constant-selector-is-resolved-at-expansion-a-run ()
  "case-probe, a constant selector is resolved at expansion, a run-time one is not."
  (is-false (has-case-p (macroexpand-1 '(marshal case-probe d :kind 1 :v 5))))
  (is-true (has-case-p (macroexpand-1 '(marshal case-probe d :kind k :v 5)))))

(def-test case-probe-an-unmatched-run-time-selector-signals-an-error ()
  "case-probe, an unmatched run-time selector signals an error."
  (signals error
    (let ((k 7)) (to-list (lambda (d) (marshal case-probe d :kind k :v 1))))))

(def-test case-probe-an-unmatched-constant-selector-is-an-expansion-error ()
  "case-probe, an unmatched constant selector is an expansion error."
  (signals error
    (macroexpand-1 '(marshal case-probe d :kind 7 :v 1))))

;; Enumerated selectors.
(def-test case-enum-probe-defaulted-selector-word-picks-the-two-byte ()
  "case-enum-probe, defaulted selector (:word) picks the two-byte branch."
  (is (equalp '(1 1 2 9)
              (to-list (lambda (d) (marshal case-enum-probe d :v #x102 :after 9))))))

(def-test case-enum-probe-a-keyword-selector-picks-the-signed-branch ()
  "case-enum-probe, a keyword selector picks the signed branch."
  (is (equalp '(2 #xFF #xFF #xFF #xFE 9)
              (to-list (lambda (d) (marshal case-enum-probe d :kind :long :v -2 :after 9))))))

(def-test case-enum-probe-a-run-time-keyword-selector-picks-the-leb-branch ()
  "case-enum-probe, a run-time keyword selector picks the LEB branch."
  (is (equalp '(3 #xAC #x02 9)
              (let ((k :varint))
                (to-list (lambda (d) (marshal case-enum-probe d :kind k :v 300 :after 9)))))))

(def-test case-enum-probe-read-back-with-the-selector-decoded ()
  "case-enum-probe, read back with the selector decoded."
  (is (equalp '(:kind :word :v #x102 :after 9)
              (unmarshal case-enum-probe (->bytes #(1 1 2 9)))))
  (is (equalp '(:kind :long :v -2 :after 9)
              (unmarshal case-enum-probe (->bytes #(2 #xFF #xFF #xFF #xFE 9)))))
  (is (equalp '(:kind :varint :v 300 :after 9)
              (unmarshal case-enum-probe (->bytes #(3 #xAC #x02 9))))))

(def-test case-badkey-probe-a-key-outside-the-enum-is-an-expansion-error ()
  "case-badkey-probe, a key outside the enum is an expansion error on both sides."
  (signals error
    (macroexpand-1 '(marshal case-badkey-probe d :v 1)))
  (signals error
    (macroexpand-1 '(unmarshal case-badkey-probe s))))

;; Default branch, order, and reading.
(def-test case-otherwise-probe-an-unlisted-selector-takes-the-otherwise ()
  "case-otherwise-probe, an unlisted selector takes the otherwise branch."
  (is (equalp '(9 1 2 9)
              (to-list (lambda (d) (marshal case-otherwise-probe d :kind 9 :v #x102 :after 9))))))

(def-test case-otherwise-probe-read-back ()
  "case-otherwise-probe reads an unlisted selector's value through the otherwise branch."
  (is (equalp '(:kind 9 :v #x102 :after 9)
              (unmarshal case-otherwise-probe (->bytes #(9 1 2 9))))))

(def-test case-late-probe-a-selector-after-its-field-is-an-expansion-error ()
  "case-late-probe, a selector after its field is an expansion error on both sides, naming
both."
  (is (equalp '(t t)
              (flet ((names-both-p (form)
                       ;; the message names the field (:V) and the selector it needs (:KIND)
                       (handler-case (progn (macroexpand-1 form) nil)
                         (error (e) (let ((message (princ-to-string e)))
                                      (and (search ":V " message) (search ":KIND" message) t))))))
                (list (names-both-p '(marshal case-late-probe d :kind 0))
                      (names-both-p '(unmarshal case-late-probe s)))))))

(def-test case-probe-every-branch-round-trips ()
  "case-probe, every branch round-trips."
  (is (equalp '((:kind 0 :v 5 :after 9) (:kind 1 :v #x102 :after 9) (:kind 2 :v -2 :after 9)
                (:kind 3 :v #x102 :after 9) (:kind 4 :v 300 :after 9))
              (loop :for (k v) :in '((0 5) (1 #x102) (2 -2) (3 #x102) (4 300))
                    :collect (unmarshal case-probe
                                        (->bytes (to-list (lambda (d)
                                                            (marshal case-probe d
                                                                     :kind k :v v :after 9)))))))))

(def-test case-probe-an-unmatched-selector-on-read-signals-an-error ()
  "case-probe, an unmatched selector on read signals an error."
  (signals error
    (unmarshal case-probe (->bytes #(7 0 0)))))

;; Spans, unnamed and enumerated :case fields.
(def-test case-span-probe-length-counts-the-chosen-four-byte-width ()
  "case-span-probe, length counts the chosen four-byte width."
  (let ((buf (ub8 7)))
    (marshal case-span-probe buf :kind 1 :v 7 :k 9)
    (is (equalp '(1 5 0 0 0 7 9)
                (coerce buf 'list)))))

(def-test case-span-probe-read-back ()
  "case-span-probe reads back a length that counts the chosen branch's width."
  (let ((buf (ub8 7)))
    (marshal case-span-probe buf :kind 1 :v 7 :k 9)
    (is (equalp '(:kind 1 :len 5 :v 7 :k 9)
                (unmarshal case-span-probe buf)))))

(def-test case-unnamed-probe-the-default-is-written-at-the-chosen-width ()
  "case-unnamed-probe, the default is written at the chosen width."
  (is (equalp '(1 0 0 0 0 9)
              (to-list (lambda (d) (marshal case-unnamed-probe d :kind 1 :after 9))))))

(def-test case-unnamed-probe-read-skips-the-chosen-width ()
  "case-unnamed-probe, read skips the chosen width."
  (is (equalp '(:kind 1 :after 9)
              (unmarshal case-unnamed-probe (->bytes #(1 0 0 0 0 9))))))

(def-test case-value-enum-probe-an-enumerated-case-value-written-and-read ()
  "case-value-enum-probe, an enumerated :case value written and read back."
  (is (equalp '((1 0 2) (:kind 1 :op :long))
              (let ((bytes (to-list (lambda (d) (marshal case-value-enum-probe d :kind 1 :op :long)))))
                (list bytes (unmarshal case-value-enum-probe (->bytes bytes)))))))

(def-test case-value-default-probe-a-keyword-default-for-the-case-value ()
  "case-value-default-probe, a keyword default for the :case value."
  (is (equalp '((1 0 2) (:kind 1 :op :long))
              (let ((bytes (to-list (lambda (d) (marshal case-value-default-probe d :kind 1)))))
                (list bytes (unmarshal case-value-default-probe (->bytes bytes)))))))

(def-test case-nest-probe-a-run-time-selector-inside-a-sub-manifest ()
  "case-nest-probe, a run-time selector inside a sub-manifest."
  (is (equalp '(7 2 #xFF #xFF #xFF #xFE 9)
              (let ((k :long))
                (to-list (lambda (d) (marshal case-nest-probe d :head 7
                                              :inner (:kind k :v -2 :after 9))))))))

(def-test case-nest-probe-read-back ()
  "case-nest-probe reads a :case field inside a sub-manifest at the width its selector chooses."
  (is (equalp '(:head 7 :inner (:kind :long :v -2 :after 9))
              (unmarshal case-nest-probe (->bytes #(7 2 #xFF #xFF #xFF #xFE 9))))))

(def-test case-le-branch-probe-a-little-endian-sub-manifest-branch ()
  "case-le-branch-probe, a little-endian sub-manifest branch in a big-endian parent."
  (is (equalp '((0 2 1) (:kind 0 :body (:x #x0102)))
              (let* ((k 0)
                     (bytes (to-list (lambda (d) (marshal case-le-branch-probe d :kind k
                                                          :body (list :x #x0102))))))
                (list bytes (unmarshal case-le-branch-probe (->bytes bytes)))))))

;; Value forms are evaluated once.
(def-test eval-once-probe-each-value-form-is-evaluated-once ()
  "eval-once-probe, each value form is evaluated once."
  (is (equalp '((1 2) 2)
              (let ((n 0))
                (list (to-list (lambda (d) (marshal eval-once-probe d :a (incf n) :b (incf n))))
                      n)))))

;;; ============================================================
;;; :vec fields on the scan model
;;; ============================================================

;; One read per element.
(def-test vec-scan-probe-a-terminated-field-reads-each-element-once ()
  "vec-scan-probe, a terminated field reads each element once."
  (is (equalp 2
              (count-reads (macroexpand-1 '(unmarshal vec-scan-probe s))))))

;; Element encoding: characters through the codec, integers raw.
(def-test vec-codec-mixed-probe-integers-in-a-codec-field-are-raw-codes ()
  "vec-codec-mixed-probe, integers in a codec field are raw codes."
  (is (equalp '(#x40 #x40 #x40)
              (to-list (lambda (d) (marshal vec-codec-mixed-probe d :s #(64 64 64)))))))

(def-test vec-codec-mixed-probe-characters-and-integers-mixed ()
  "vec-codec-mixed-probe, characters and integers mixed."
  (is (equalp '(#xC1 #x40 #xC2)
              (to-list (lambda (d) (marshal vec-codec-mixed-probe d :s (vector #\A 64 #\B)))))))

;; Signed elements compare and write raw codes.
(def-test vec-signed-probe-1-128-written-with-the-xff-terminator ()
  "vec-signed-probe writes #(1 -128) in two's complement, then its #xFF terminator."
  (is (equalp '(1 #x80 #xFF 9)
              (to-list (lambda (d) (marshal vec-signed-probe d :s #(1 -128) :after 9))))))

(def-test vec-signed-probe-read-back ()
  "vec-signed-probe reads its elements back sign-extended, stopping at the #xFF terminator."
  (is (equalp '(:s #(1 -128) :after 9)
              (unmarshal vec-signed-probe (->bytes #(1 #x80 #xFF 9))))))

(def-test vec-signed-probe-1-is-the-terminator-s-code-and-signals-an-error ()
  "vec-signed-probe, -1 is the terminator's code and signals an error."
  (signals error
    (to-list (lambda (d) (marshal vec-signed-probe d :s #(1 -1))))))

(def-test vec-signed-probe-129-does-not-fit-and-signals-an-error ()
  "vec-signed-probe, -129 does not fit and signals an error."
  (signals error
    (to-list (lambda (d) (marshal vec-signed-probe d :s #(-129))))))

;; Enumerated elements.
(def-test vec-enum-probe-keyword-and-integer-elements-written-as-codes ()
  "vec-enum-probe, keyword and integer elements written as codes."
  (is (equalp '(2 1 3)
              (to-list (lambda (d) (marshal vec-enum-probe d :c #(:green :red 3)))))))

(def-test vec-enum-probe-read-back-as-keywords ()
  "vec-enum-probe, read back as keywords."
  (is (equalp '(:c #(:green :red :blue))
              (unmarshal vec-enum-probe (->bytes #(2 1 3))))))

(def-test vec-enum-probe-a-code-the-enum-lacks-reads-as-itself-and-writes ()
  "vec-enum-probe, a code the enum lacks reads as itself and writes back."
  (is (equalp '(#(:green 9 :blue) (2 9 3))
              (let ((value (getf (unmarshal vec-enum-probe (->bytes #(2 9 3))) :c)))
                (list value (to-list (lambda (d) (marshal vec-enum-probe d :c value))))))))

(def-test vec-enum-probe-a-keyword-outside-the-enum-signals-an-error ()
  "vec-enum-probe, a keyword outside the enum signals an error."
  (signals error
    (to-list (lambda (d) (marshal vec-enum-probe d :c #(:green :mauve 3))))))

(def-test vec-plain-probe-a-keyword-element-without-an-enum-signals ()
  "vec-plain-probe, a keyword element without an enum signals an error."
  (signals error
    (to-list (lambda (d) (marshal vec-plain-probe d :c #(:red))))))

;; Unnamed fields, :case branches and wide elements.
(def-test vec-unnamed-probe-an-unnamed-terminated-field-writes ()
  "vec-unnamed-probe, an unnamed terminated field writes its terminator."
  (is (equalp '(0 7)
              (to-list (lambda (d) (marshal vec-unnamed-probe d :k 7))))))

(def-test vec-unnamed-probe-read-skips-it ()
  "vec-unnamed-probe, read skips it."
  (is (equalp '(:k 7)
              (unmarshal vec-unnamed-probe (->bytes #(97 98 0 7))))))

(def-test vec-case-probe-a-scalar-branch-and-a-terminated-vector-branch ()
  "vec-case-probe writes a scalar branch and a terminated vector branch, each chosen by its
selector."
  (is (equalp '(0 5 9)
              (to-list (lambda (d) (marshal vec-case-probe d :kind 0 :v 5 :after 9)))))
  (is (equalp '(1 97 98 0 9)
              (to-list (lambda (d) (marshal vec-case-probe d :kind 1 :v "ab" :after 9))))))

(def-test vec-case-probe-both-branches-read-back ()
  "vec-case-probe reads both its scalar branch and its terminated vector branch back."
  (is (equalp '(:kind 0 :v 5 :after 9)
              (unmarshal vec-case-probe (->bytes #(0 5 9)))))
  (is (equalp '(:kind 1 :v #(97 98) :after 9)
              (unmarshal vec-case-probe (->bytes #(1 97 98 0 9))))))

(def-test vec-case-enum-probe-the-enum-decodes-a-scalar-branch ()
  "vec-case-enum-probe, the enum decodes a scalar branch and a vector branch."
  (is (equalp '(:kind 0 :v :blue)
              (unmarshal vec-case-enum-probe (->bytes #(0 3)))))
  (is (equalp '(:kind 1 :v #(:green :red))
              (unmarshal vec-case-enum-probe (->bytes #(1 2 1))))))

(def-test vec-wide-probe-two-byte-little-endian-elements-written ()
  "vec-wide-probe writes its two-byte elements little-endian."
  (is (equalp '(#x42 #x41 #x43 0)
              (to-list (lambda (d) (marshal vec-wide-probe d :s #(#x4142 #x43)))))))

(def-test vec-wide-probe-read-back ()
  "vec-wide-probe reads two-byte little-endian elements back."
  (is (equalp '(:s #(#x4142 #x43))
              (unmarshal vec-wide-probe (->bytes #(#x42 #x41 #x43 0))))))

(def-test vec-wide-probe-a-list-is-accepted-as-a-vector-s-value ()
  "vec-wide-probe, a list is accepted as a vector's value."
  (is (equalp '(#x42 #x41 #x43 0)
              (let ((value (list #x4142 #x43)))
                (to-list (lambda (d) (marshal vec-wide-probe d :s value)))))))

;;; ============================================================
;;; Fixed buffers holding a terminator (:count with :end-by)
;;; ============================================================

;; Writing: short, full and empty buffers.
(def-test bounded-probe-short-data-is-followed-by-terminators ()
  "bounded-probe, short data is followed by terminators to the buffer's end."
  (is (equalp '(97 98 0 0 0 9)
              (to-list (lambda (d) (marshal bounded-probe d :name "ab" :after 9))))))

(def-test bounded-probe-data-filling-the-buffer-has-no-terminator ()
  "bounded-probe, data filling the buffer has no terminator."
  (is (equalp '(97 98 99 100 101 9)
              (to-list (lambda (d) (marshal bounded-probe d :name "abcde" :after 9))))))

(def-test bounded-probe-no-value-fills-the-buffer-with-terminators ()
  "bounded-probe, no value fills the buffer with terminators."
  (is (equalp '(0 0 0 0 0 9)
              (to-list (lambda (d) (marshal bounded-probe d :after 9))))))

(def-test bounded-probe-data-longer-than-the-buffer-signals-an-error ()
  "bounded-probe, data longer than the buffer signals an error."
  (signals error
    (to-list (lambda (d) (marshal bounded-probe d :name "abcdef")))))

(def-test bounded-probe-data-containing-the-terminator-signals-an-error ()
  "bounded-probe, data containing the terminator signals an error."
  (signals error
    (to-list (lambda (d) (marshal bounded-probe d :name (vector 97 0 98))))))

;; Reading: stop at the terminator or the buffer's end, move past the buffer.
(def-test bounded-probe-a-short-buffer-read-back ()
  "bounded-probe reads a short value from its five-byte buffer, stopping at the terminator, and
the field after the buffer lines up."
  (is (equalp '(:name #(97 98) :after 9)
              (unmarshal bounded-probe (->bytes #(97 98 0 0 0 9))))))

(def-test bounded-probe-a-full-buffer-read-back-without-a-terminator ()
  "bounded-probe, a full buffer read back without a terminator."
  (is (equalp '(:name #(97 98 99 100 101) :after 9)
              (unmarshal bounded-probe (->bytes #(97 98 99 100 101 9))))))

(def-test bounded-probe-bytes-after-the-terminator-are-skipped-whatever ()
  "bounded-probe, bytes after the terminator are skipped, whatever they hold."
  (is (equalp '(:name #(97) :after 9)
              (unmarshal bounded-probe (->bytes #(97 0 122 122 122 9))))))

(def-test bounded-probe-an-empty-buffer-read-back ()
  "bounded-probe reads a buffer holding only terminators as an empty value."
  (is (equalp '(:name #() :after 9)
              (unmarshal bounded-probe (->bytes #(0 0 0 0 0 9))))))

(def-test bounded-probe-a-source-shorter-than-the-buffer-signals-an-error ()
  "bounded-probe, a source shorter than the buffer signals an error."
  (signals error
    (unmarshal bounded-probe (->bytes #(97 98 0 0)))))

;; Codecs, wide elements, spans and counts from other fields.
(def-test bounded-ebcdic-probe-hi-then-x5b-to-the-buffer-s-end ()
  "bounded-ebcdic-probe, \"HI\" then $ (#x5B) to the buffer's end."
  (is (equalp '(#xC8 #xC9 #x5B #x5B)
              (to-list (lambda (d) (marshal bounded-ebcdic-probe d :name "HI"))))))

(def-test bounded-ebcdic-probe-read-back-as-a-string ()
  "bounded-ebcdic-probe, read back as a string."
  (is (equalp '(:name "HI")
              (unmarshal bounded-ebcdic-probe (->bytes #(#xC8 #xC9 #x5B #x5B))))))

(def-test bounded-wide-probe-one-two-byte-element-then-two-terminator ()
  "bounded-wide-probe, one two-byte element then two terminator elements."
  (is (equalp '(#x42 #x41 0 0 0 0)
              (to-list (lambda (d) (marshal bounded-wide-probe d :s #(#x4142)))))))

(def-test bounded-wide-probe-read-back ()
  "bounded-wide-probe reads one two-byte element from a buffer filled out with terminators."
  (is (equalp '(:s #(#x4142))
              (unmarshal bounded-wide-probe (->bytes #(#x42 #x41 0 0 0 0))))))

(def-test bounded-span-probe-the-length-slot-counts-the-whole-buffer ()
  "bounded-span-probe, the length slot counts the whole buffer."
  (let ((buf (ub8 6)))
    (marshal bounded-span-probe buf :name "ab" :k 7)
    (is (equalp '(5 97 98 0 0 7)
                (coerce buf 'list)))))

(def-test bounded-span-probe-read-back ()
  "bounded-span-probe reads back a length that counts the whole buffer."
  (let ((buf (ub8 6)))
    (marshal bounded-span-probe buf :name "ab" :k 7)
    (is (equalp '(:len 5 :name #(97 98) :k 7)
                (unmarshal bounded-span-probe buf)))))

(def-test bounded-counted-probe-a-buffer-sized-by-an-earlier-field-read ()
  "bounded-counted-probe reads a buffer whose size an earlier field gives, skipping what
follows its terminator."
  (is (equalp '(:n 4 :name #(97) :after 9)
              (unmarshal bounded-counted-probe (->bytes #(4 97 0 122 122 9))))))

(def-test bounded-counted-probe-writing-it-needs-an-integer-count-for-now ()
  "bounded-counted-probe, writing it needs an integer :count for now."
  (signals error
    (macroexpand-1 '(marshal bounded-counted-probe d :n 4 :name "a"))))

;; Facts: the data count is unknown, the extent is the buffer.
(def-test bounded-probe-known-count-is-nil-and-known-extent-is-5-units ()
  "bounded-probe, known-count is NIL and known-extent is 5 units."
  (let ((plan (field-plan (second (gethash 'bounded-probe *manifests*))
                          :count-form (lambda (i) (getf i :count)))))
    (is-false (known-count (slot-value plan 'rule)))
    (is (equalp 5
                (known-extent plan)))))

;;; ============================================================
;;; Terminators of several elements
;;; ============================================================

;; CR LF, as a vector and as a string.
(def-test crlf-probe-ab-then-cr-lf ()
  "crlf-probe, \"ab\" then CR LF."
  (is (equalp '(97 98 13 10 9)
              (to-list (lambda (d) (marshal crlf-probe d :line "ab" :after 9))))))

(def-test crlf-probe-read-back-both-elements-of-the-terminator-dropped ()
  "crlf-probe reads the data before CR LF, dropping both elements of the terminator."
  (is (equalp '(:line #(97 98) :after 9)
              (unmarshal crlf-probe (->bytes #(97 98 13 10 9))))))

(def-test crlf-string-probe-a-string-terminator-writes-the-same-bytes ()
  "crlf-string-probe, a string terminator writes the same bytes."
  (is (equalp '(97 98 13 10 9)
              (to-list (lambda (d) (marshal crlf-string-probe d :line "ab" :after 9))))))

(def-test crlf-probe-no-value-writes-the-terminator-alone-and-reads-back ()
  "crlf-probe, no value writes the terminator alone, and reads back empty."
  (is (equalp '(13 10 9)
              (to-list (lambda (d) (marshal crlf-probe d :after 9)))))
  (is (equalp '(:line #() :after 9)
              (unmarshal crlf-probe (->bytes #(13 10 9))))))

;; Partial matches are data.
(def-test crlf-probe-a-cr-not-followed-by-lf-is-data-on-both-sides ()
  "crlf-probe, a CR not followed by LF is data, on both sides."
  (is (equalp '(97 13 98 13 10 9)
              (to-list (lambda (d) (marshal crlf-probe d :line #(97 13 98) :after 9)))))
  (is (equalp '(:line #(97 13 98) :after 9)
              (unmarshal crlf-probe (->bytes #(97 13 98 13 10 9))))))

(def-test crlf-probe-data-ending-in-cr-reads-back-whole ()
  "crlf-probe, data ending in CR reads back whole."
  (is (equalp '(97 13 13 10 9)
              (to-list (lambda (d) (marshal crlf-probe d :line #(97 13) :after 9)))))
  (is (equalp '(:line #(97 13) :after 9)
              (unmarshal crlf-probe (->bytes #(97 13 13 10 9))))))

;; Data that would end the field early.
(def-test crlf-probe-data-containing-cr-lf-signals-an-error ()
  "crlf-probe, data containing CR LF signals an error."
  (signals error
    (to-list (lambda (d) (marshal crlf-probe d :line #(97 13 10 98))))))

(def-test aa-probe-data-ending-in-a-would-form-aa-with-the-terminator ()
  "aa-probe, data ending in A would form AA with the terminator: an error."
  (signals error
    (to-list (lambda (d) (marshal aa-probe d :s "xA")))))

(def-test aa-probe-data-starting-with-a-is-fine-and-reads-back ()
  "aa-probe, data starting with A is fine, and reads back."
  (is (equalp '((65 120 65 65 9) (:s #(65 120) :after 9))
              (let ((bytes (to-list (lambda (d) (marshal aa-probe d :s "Ax" :after 9)))))
                (list bytes (unmarshal aa-probe (->bytes bytes)))))))

(def-test crlf-probe-a-source-with-no-terminator-signals-an-error ()
  "crlf-probe, a source with no terminator signals an error."
  (signals error
    (unmarshal crlf-probe (->bytes #(97 98 13)))))

;; A codec, and an empty terminator.
(def-test ebcdic-multi-probe-hi-then-encoded-as-x5b-x5b ()
  "ebcdic-multi-probe, \"HI\" then $$ encoded as #x5B #x5B."
  (is (equalp '(#xC8 #xC9 #x5B #x5B)
              (to-list (lambda (d) (marshal ebcdic-multi-probe d :name "HI"))))))

(def-test ebcdic-multi-probe-read-back-as-a-string ()
  "ebcdic-multi-probe, read back as a string."
  (is (equalp '(:name "HI")
              (unmarshal ebcdic-multi-probe (->bytes #(#xC8 #xC9 #x5B #x5B))))))

(def-test empty-terminator-probe-an-empty-end-by-is-an-expansion-error ()
  "empty-terminator-probe, an empty :end-by is an expansion error."
  (signals error
    (macroexpand-1 '(unmarshal empty-terminator-probe s))))

;; Fixed buffers ended by CR LF.
(def-test bounded-crlf-probe-ab-then-cr-lf-repeated-to-the-buffer-s-end ()
  "bounded-crlf-probe, \"ab\" then CR LF repeated to the buffer's end."
  (is (equalp '(97 98 13 10 13 10 9)
              (to-list (lambda (d) (marshal bounded-crlf-probe d :line "ab" :after 9))))))

(def-test bounded-crlf-probe-abcd-leaves-room-for-exactly-one-cr-lf ()
  "bounded-crlf-probe, \"abcd\" leaves room for exactly one CR LF."
  (is (equalp '(97 98 99 100 13 10 9)
              (to-list (lambda (d) (marshal bounded-crlf-probe d :line "abcd" :after 9))))))

(def-test bounded-crlf-probe-abcdef-fills-the-buffer-with-no-terminator ()
  "bounded-crlf-probe, \"abcdef\" fills the buffer with no terminator."
  (is (equalp '(97 98 99 100 101 102 9)
              (to-list (lambda (d) (marshal bounded-crlf-probe d :line "abcdef" :after 9))))))

(def-test bounded-crlf-probe-abcde-leaves-room-for-half-a-terminator ()
  "bounded-crlf-probe, \"abcde\" leaves room for half a terminator: an error."
  (signals error
    (to-list (lambda (d) (marshal bounded-crlf-probe d :line "abcde")))))

(def-test bounded-crlf-probe-read-back-skipping-what-follows ()
  "bounded-crlf-probe, read back, skipping what follows the terminator."
  (is (equalp '(:line #(97 98) :after 9)
              (unmarshal bounded-crlf-probe (->bytes #(97 98 13 10 13 10 9)))))
  (is (equalp '(:line #(97) :after 9)
              (unmarshal bounded-crlf-probe (->bytes #(97 13 10 122 122 122 9))))))

(def-test bounded-crlf-probe-a-full-buffer-ending-in-cr-keeps-the-cr ()
  "bounded-crlf-probe, a full buffer ending in CR keeps the CR as data."
  (is (equalp '(:line #(97 98 99 100 101 13) :after 9)
              (unmarshal bounded-crlf-probe (->bytes #(97 98 99 100 101 13 9))))))

;;; ============================================================
;;; Vectors of LEB128 elements
;;; ============================================================

;; Counted vectors.
(def-test leb-vec-probe-1-300-624485-as-lebs-of-one-two-and-three-bytes ()
  "leb-vec-probe writes #(1 300 624485) as LEB128s of one, two and three bytes."
  (is (equalp '(1 #xAC #x02 #xE5 #x8E #x26 9)
              (to-list (lambda (d) (marshal leb-vec-probe d :v #(1 300 624485) :after 9))))))

(def-test leb-vec-probe-read-back-with-the-field-after-lined-up ()
  "leb-vec-probe, read back, with the field after lined up."
  (is (equalp '(:v #(1 300 624485) :after 9)
              (unmarshal leb-vec-probe (->bytes #(1 #xAC #x02 #xE5 #x8E #x26 9))))))

(def-test leb-vec-probe-a-bignum-element-round-trips ()
  "leb-vec-probe, a bignum element round-trips."
  (is (equalp (vector 0 (expt 2 70) 5)
              (let ((v (vector 0 (expt 2 70) 5)))
                (getf (unmarshal leb-vec-probe
                                 (->bytes (to-list (lambda (d) (marshal leb-vec-probe d :v v)))))
                      :v)))))

(def-test sleb-vec-probe-signed-elements-1-64-123456 ()
  "sleb-vec-probe, signed elements #(-1 64 -123456)."
  (is (equalp '(#x7F #xC0 #x00 #xC0 #xBB #x78)
              (to-list (lambda (d) (marshal sleb-vec-probe d :v #(-1 64 -123456)))))))

(def-test sleb-vec-probe-read-back ()
  "sleb-vec-probe reads signed LEB128 elements back to #(-1 64 -123456)."
  (is (equalp '(:v #(-1 64 -123456))
              (unmarshal sleb-vec-probe (->bytes #(#x7F #xC0 #x00 #xC0 #xBB #x78))))))

(def-test leb-vec-counted-probe-a-count-from-an-earlier-field-written ()
  "leb-vec-counted-probe, a count from an earlier field, written and read back."
  (is (equalp '((2 5 #xAC #x02 9) (:n 2 :v #(5 300) :after 9))
              (let ((bytes (to-list (lambda (d) (marshal leb-vec-counted-probe d
                                                         :n 2 :v #(5 300) :after 9)))))
                (list bytes (unmarshal leb-vec-counted-probe (->bytes bytes)))))))

(def-test leb-vec-probe-a-leb-unfinished-at-the-end-of-the-source-signals ()
  "leb-vec-probe, a LEB unfinished at the end of the source signals an error."
  (signals error
    (unmarshal leb-vec-probe (->bytes #(1 #xAC)))))

;; Terminated vectors: the terminator is compared with element values.
(def-test leb-vec-terminated-probe-127-128-then-the-element-0 ()
  "leb-vec-terminated-probe writes #(127 128) as LEB128s, then the element 0 as its terminator."
  (is (equalp '(#x7F #x80 #x01 0 9)
              (to-list (lambda (d) (marshal leb-vec-terminated-probe d :v #(127 128) :after 9))))))

(def-test leb-vec-terminated-probe-read-back ()
  "leb-vec-terminated-probe reads LEB128 elements up to the element 0."
  (is (equalp '(:v #(127 128) :after 9)
              (unmarshal leb-vec-terminated-probe (->bytes #(#x7F #x80 #x01 0 9))))))

(def-test leb-vec-terminated-probe-an-element-with-the-value-0-signals ()
  "leb-vec-terminated-probe, an element with the value 0 signals an error."
  (signals error
    (to-list (lambda (d) (marshal leb-vec-terminated-probe d :v #(5 0 6))))))

(def-test leb-vec-enum-probe-keywords-written-as-leb-codes-read-back ()
  "leb-vec-enum-probe, keywords written as LEB codes, read back as keywords."
  (is (equalp '((#xC8 #x01 #xAC #x02 0) (:ops #(:call :jump)))
              (let ((bytes (to-list (lambda (d) (marshal leb-vec-enum-probe d :ops #(:call :jump))))))
                (list bytes (unmarshal leb-vec-enum-probe (->bytes bytes)))))))

(def-test leb-vec-case-probe-a-leb-vector-branch-inheriting-end-by-both ()
  "leb-vec-case-probe, a LEB vector branch inheriting :end-by, both ways."
  (is (equalp '((1 #xAC #x02 0 9) (:kind 1 :v #(300) :after 9))
              (let ((bytes (to-list (lambda (d) (marshal leb-vec-case-probe d :kind 1 :v #(300) :after 9)))))
                (list bytes (unmarshal leb-vec-case-probe (->bytes bytes)))))))

;; What a LEB vector cannot be, yet.
(def-test leb-vec-length-probe-the-length-slot-holds-the-vector-s-bytes ()
  "leb-vec-length-probe, the length slot holds the vector's bytes on write."
  (let ((buf (ub8 4)))
    (marshal leb-vec-length-probe buf :v #(1 300))
    (is (equalp '(3 1 #xAC #x02)
                (coerce buf 'list)))))

(def-test leb-vec-length-probe-read-to-the-end-of-the-3-bytes-its-length ()
  "leb-vec-length-probe, read to the end of the 3 bytes its length slot gives."
  (is (equalp '(:len 3 :v #(1 300))
              (unmarshal leb-vec-length-probe (->bytes #(3 1 #xAC #x02))))))

(def-test leb-vec-length-probe-an-element-crossing-the-end-of-the-length ()
  "leb-vec-length-probe, an element crossing the end of the length signals an error."
  (signals error
    (unmarshal leb-vec-length-probe (->bytes #(2 1 #xAC #x02)))))

(def-test leb-vec-bounded-probe-a-fixed-buffer-of-lebs-is-an-expansion ()
  "leb-vec-bounded-probe, a fixed buffer of LEBs is an expansion error on both sides."
  (signals error
    (macroexpand-1 '(unmarshal leb-vec-bounded-probe s)))
  (signals error
    (macroexpand-1 '(marshal leb-vec-bounded-probe d :v #(1)))))

(def-test leb-vec-wide-probe-leb-elements-wider-than-one-byte-an-expansion ()
  "leb-vec-wide-probe, LEB elements wider than one byte: an expansion error."
  (signals error
    (macroexpand-1 '(unmarshal leb-vec-wide-probe s))))

;;; ============================================================
;;; User-defined stop rules (:stop-by) and the runtime protocol
;;; ============================================================

;; done-p, and fresh state per field.
(def-test quic-probe-a-1-byte-and-a-2-byte-varint-each-field-with-its-own ()
  "quic-probe, a 1-byte and a 2-byte varint, each field with its own rule."
  (is (equalp '(:a #(#x25) :b #(#x7B #xBD) :after 9)
              (unmarshal quic-probe (->bytes #(#x25 #x7B #xBD 9))))))

(def-test quic-probe-written-unchanged ()
  "quic-probe writes the varints its user rule reads, byte for byte, adding no framing."
  (is (equalp '(#x25 #x7B #xBD 9)
              (to-list (lambda (d) (marshal quic-probe d :a #(#x25) :b #(#x7B #xBD) :after 9))))))

(def-test quic-probe-a-source-that-ends-before-the-varint-does-signals ()
  "quic-probe, a source that ends before the varint does signals an error."
  (signals error
    (unmarshal quic-probe (->bytes #(#x25 #x7B)))))

;; :keep and frame-element.
(def-test high-bit-probe-every-element-but-the-last-is-marked-on-write ()
  "high-bit-probe, every element but the last is marked on write."
  (is (equalp '(#x81 #x82 3 9)
              (to-list (lambda (d) (marshal high-bit-probe d :v #(1 2 3) :after 9))))))

(def-test high-bit-probe-read-back-the-unmarked-element-ends-the-field ()
  "high-bit-probe, read back: the unmarked element ends the field and is kept."
  (is (equalp '(:v #(#x81 #x82 3) :after 9)
              (unmarshal high-bit-probe (->bytes #(#x81 #x82 3 9))))))

;; :drop, trailer and frame-trailer.
(def-test double-zero-probe-data-then-two-zeros ()
  "double-zero-probe, data then two zeros."
  (is (equalp '(5 0 6 0 0 9)
              (to-list (lambda (d) (marshal double-zero-probe d :v #(5 0 6) :after 9))))))

(def-test double-zero-probe-read-back-a-single-zero-is-data-both-final ()
  "double-zero-probe, read back: a single zero is data, both final zeros dropped."
  (is (equalp '(:v #(5 0 6) :after 9)
              (unmarshal double-zero-probe (->bytes #(5 0 6 0 0 9))))))

;; Initargs, and the upgrade to inline code.
(def-test stop-at-probe-a-terminator-given-by-an-initarg-written-and-read ()
  "stop-at-probe, a terminator given by an initarg, written and read back."
  (is (equalp '((1 2 255 9) (:v #(1 2) :after 9))
              (let ((bytes (to-list (lambda (d) (marshal stop-at-probe d :v #(1 2) :after 9)))))
                (list bytes (unmarshal stop-at-probe (->bytes bytes)))))))

(def-test stop-at-inline-probe-the-upgraded-rule-gives-the-same-bytes ()
  "stop-at-inline-probe, the upgraded rule gives the same bytes and values."
  (is (equalp '((1 2 255 9) (:v #(1 2) :after 9))
              (let ((bytes (to-list (lambda (d) (marshal stop-at-inline-probe d :v #(1 2) :after 9)))))
                (list bytes (unmarshal stop-at-inline-probe (->bytes bytes)))))))

(def-test stop-at-probe-calls-the-runtime-protocol-stop-at-inline-probe ()
  "stop-at-probe calls the runtime protocol; stop-at-inline-probe does not."
  (is-true (mentions-p (macroexpand-1 '(unmarshal stop-at-probe s)) '(feed done-p)))
  (is-true (mentions-p (macroexpand-1 '(marshal stop-at-probe d :v #(1))) '(frame-trailer)))
  (is-false (mentions-p (macroexpand-1 '(unmarshal stop-at-inline-probe s))
                        '(feed done-p make-instance)))
  (is-false (mentions-p (macroexpand-1 '(marshal stop-at-inline-probe d :v #(1)))
                        '(frame-element frame-trailer make-instance))))

(def-test foreign-rule-probe-a-rule-from-another-package-written-and-read ()
  "foreign-rule-probe, a rule from another package, written and read back."
  (is (equalp '((1 2 254 9) (:v #(1 2) :after 9))
              (let ((bytes (to-list (lambda (d) (marshal foreign-rule-probe d :v #(1 2) :after 9)))))
                (list bytes (unmarshal foreign-rule-probe (->bytes bytes)))))))

;; Fields that cannot use :stop-by.
(def-test stop-by-unknown-probe-a-class-that-does-not-exist ()
  "stop-by-unknown-probe, a class that does not exist is an expansion error."
  (signals error
    (macroexpand-1 '(unmarshal stop-by-unknown-probe s))))

(def-test stop-by-with-end-by-probe-stop-by-with-end-by-is-an-expansion ()
  "stop-by-with-end-by-probe, :stop-by with :end-by is an expansion error."
  (signals error
    (macroexpand-1 '(unmarshal stop-by-with-end-by-probe s))))

(def-test stop-by-on-a-scalar-field-is-an-error-when-the-manifest ()
  ":stop-by on a scalar field is an error when the manifest is defined."
  (signals error
    (macroexpand-1 '(defmanifest stop-by-scalar-probe
                     (:unit 8 :endian :big)
                     (:v :u8 :stop-by double-zero)))))

;;; ============================================================
;;; IEEE floats: half, single, double and quad precision
;;; ============================================================

;; The four formats, written and read back with their Lisp types.
(def-test float-probe-half-single-double-and-quad-precision-written-big ()
  "float-probe: half, single, double and quad precision written big-endian."
  (let ((buf (ub8 30)))
    (marshal float-probe buf :half 1.5 :single -2.5 :double 0.1d0 :quad -2)
    (is (equalp (concatenate 'string "3E00" "C0200000" "3FB999999999999A" "C0000000000000000000000000000000")
                (hex-units buf)))))

(def-test float-probe-read-back-single-floats-for-half-and-single-a-double ()
  "float-probe, read back: single floats for half and single, a double, and an exact rational
for quad."
  (let ((buf (ub8 30)))
    (marshal float-probe buf :half 1.5 :single -2.5 :double 0.1d0 :quad -2)
    (is (equalp '((:half 1.5 :single -2.5 :double 0.1d0 :quad -2) (single-float single-float double-float))
                (let ((p (unmarshal float-probe buf)))
                  (list p (mapcar (lambda (k) (type-of (getf p k))) '(:half :single :double))))))))

(def-test float-probe-as-a-record-written-back-and-double-read-alone ()
  "float-probe, read as a record and written back, reproduces its bytes; :double read alone is
the double written."
  (let ((buf (ub8 30)))
    (marshal float-probe buf :half 1.5 :single -2.5 :double 0.1d0 :quad -2)
    (is-true (equalp buf (let ((out (ub8 30))) (marshal float-probe out (:from (unmarshal float-probe buf (:as :record)))) out)))
    (is-true (eql 0.1d0 (unmarshal float-probe buf nil :double)))))

(def-test float-le-probe-and-float-word-probe-byte-order-per-field-and-two ()
  "float-le-probe and float-word-probe: byte order per field, and two 16-bit units."
  (is (equalp '(0 0 #x80 #x3F)
              (let ((buf (ub8 4))) (marshal float-le-probe buf :single 1.0) (coerce buf 'list))))
  (is (equalp '((#x3F80 0) (:x 1.0))
              (let ((buf (make-array 2 :element-type '(unsigned-byte 16))))
                (marshal float-word-probe buf :x 1.0)
                (list (coerce buf 'list) (unmarshal float-word-probe buf))))))

(def-test values-too-large-for-their-format-are-errors-so-is-a-float-of-24 ()
  "Values too large for their format are errors; so is a float of 24 bits."
  (signals error
    (encode-float-bits 65520 16))
  (signals error
    (encode-float-bits 1d300 32))
  (signals error
    (encode-float-bits (expt 10 400) 64))
  (signals error
    (let ((buf (ub8 2))) (marshal float-probe buf :half 70000)))
  (signals error
    (macroexpand-1 '(marshal float-bad-width-probe d :x 1.0))))

#+sbcl
(def-test sbcl-infinities-and-a-nan-with-its-payload-read-as-sbcl-floats ()
  "SBCL: infinities and a NaN with its payload read as SBCL floats and written back bit for
bit."
  (is-true (sb-ext:float-infinity-p (decode-float-bits #x7F800000 32)))
  (is-true (sb-ext:float-nan-p (decode-float-bits #x7FC00001 32)))
  (is (equalp #x7FC00001
              (encode-float-bits (decode-float-bits #x7FC00001 32) 32)))
  (is (equalp #xFFF8000000000123
              (encode-float-bits (decode-float-bits #xFFF8000000000123 64) 64)))
  (is (equalp "0100C07F"
              (let ((buf (->bytes #(#x7F #xC0 0 1)))) (hex-units (let ((out (ub8 4))) (marshal float-le-probe out :single (unmarshal float-le-probe (reverse buf) nil :single)) out))))))

;; Vectors and :case branches.
(def-test float-vec-probe-a-vector-of-single-floats-counted-by-a-count ()
  "float-vec-probe: a vector of single floats counted by a :count-of slot, written and read
back."
  (let ((buf (ub8 14)))
    (marshal float-vec-probe buf :v #(1.0 -0.5 2.25) :after 9)
    (is (equalp "033F800000BF0000004010000009"
                (hex-units buf)))
    (is (equalp '(:n 3 :v #(1.0 -0.5 2.25) :after 9)
                (unmarshal float-vec-probe buf)))))

(def-test float-case-probe-a-float-branch-chosen-when-the-code-runs ()
  "float-case-probe: a float branch chosen when the code runs, written and read back."
  (is (equalp "023FE000000000000009"
              (let ((k 2) (buf (ub8 10))) (marshal float-case-probe buf :kind k :v 0.5d0 :after 9) (hex-units buf))))
  (is (equalp '(:kind 1 :v 1.5 :after 9)
              (unmarshal float-case-probe (->bytes #(1 #x3F #xC0 0 0 9)))))
  (is (equalp '(:kind 0 :v 7 :after 9)
              (unmarshal float-case-probe (->bytes #(0 0 0 0 7 9))))))

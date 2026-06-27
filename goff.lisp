;;;; goff.lisp — GOFF (Generalized Object File Format) emitter for SpecOps
;;;
;;; Writes GOFF object files from assembled System Z programs.
;;; GOFF is the standard object file format for z/OS, consisting
;;; of fixed-length 80-byte records with 3-byte PTV headers.
;;;
;;; Record types: HDR, ESD (symbols), TXT (code/data), RLD
;;; (relocations), END.
;;;
;;; Hierarchy: SD (Section Definition) → ED (Element Definition)
;;;            → LD (Label) / PR (Part Reference)

(in-package #:specops)

;; ===============================================================
;; GOFF record constants
;; ===============================================================

(defconstant +goff-record-length+ 80
  "Fixed length of a GOFF physical record.")

(defconstant +goff-ptv-prefix+ #x03
  "PTV byte 0: always #x03 for GOFF (distinguishes from OBJ #x02).")

;; Record type (high nibble of PTV byte 1)
(defconstant +goff-rt-esd+ #x00)
(defconstant +goff-rt-txt+ #x10)
(defconstant +goff-rt-rld+ #x20)
(defconstant +goff-rt-len+ #x30)
(defconstant +goff-rt-end+ #x40)
(defconstant +goff-rt-hdr+ #xF0)

;; Continuation flags (low 2 bits of PTV byte 1)
(defconstant +goff-cont-none+  #x00 "Not continued.")
(defconstant +goff-cont-more+  #x01 "Continued; more records follow.")
(defconstant +goff-cont-last+  #x02 "Continuation record, final.")
(defconstant +goff-cont-chain+ #x03 "Continuation record, more follow.")

;; ESD symbol types
(defconstant +goff-est-sd+ #x00 "Section Definition")
(defconstant +goff-est-ed+ #x01 "Element Definition")
(defconstant +goff-est-ld+ #x02 "Label Definition")
(defconstant +goff-est-pr+ #x03 "Part Reference")
(defconstant +goff-est-er+ #x04 "External Reference")

;; Namespace IDs
(defconstant +goff-ns-binder+ 0)
(defconstant +goff-ns-normal+ 1)
(defconstant +goff-ns-parts+  3)

;; ===============================================================
;; ASCII → EBCDIC conversion (CP-037)
;; ===============================================================

(defparameter *ascii-to-ebcdic*
  (let ((table (make-array 128 :element-type '(unsigned-byte 8) :initial-element #x40)))
    ;; Space and common punctuation
    (setf (aref table 32)  #x40   ; space
          (aref table 33)  #x5A   ; !
          (aref table 34)  #x7F   ; "
          (aref table 35)  #x7B   ; #
          (aref table 36)  #x5B   ; $
          (aref table 37)  #x6C   ; %
          (aref table 38)  #x50   ; &
          (aref table 39)  #x7D   ; '
          (aref table 40)  #x4D   ; (
          (aref table 41)  #x5D   ; )
          (aref table 42)  #x5C   ; *
          (aref table 43)  #x4E   ; +
          (aref table 44)  #x6B   ; ,
          (aref table 45)  #x60   ; -
          (aref table 46)  #x4B   ; .
          (aref table 47)  #x61   ; /
          ;; Digits 0-9
          (aref table 48)  #xF0
          (aref table 49)  #xF1
          (aref table 50)  #xF2
          (aref table 51)  #xF3
          (aref table 52)  #xF4
          (aref table 53)  #xF5
          (aref table 54)  #xF6
          (aref table 55)  #xF7
          (aref table 56)  #xF8
          (aref table 57)  #xF9
          (aref table 58)  #x7A   ; :
          (aref table 59)  #x5E   ; ;
          (aref table 60)  #x4C   ; <
          (aref table 61)  #x7E   ; =
          (aref table 62)  #x6E   ; >
          (aref table 63)  #x6F)  ; ?
    (setf (aref table 64)  #x7C)  ; @
    ;; Uppercase A-Z: #xC1-#xC9, #xD1-#xD9, #xE2-#xE9
    (loop :for i :from 0 :below 9  :do (setf (aref table (+ 65 i)) (+ #xC1 i)))  ; A-I
    (loop :for i :from 0 :below 9  :do (setf (aref table (+ 74 i)) (+ #xD1 i)))  ; J-R
    (loop :for i :from 0 :below 8  :do (setf (aref table (+ 83 i)) (+ #xE2 i)))  ; S-Z
    (setf (aref table 91)  #xBA   ; [
          (aref table 92)  #xE0   ; backslash
          (aref table 93)  #xBB   ; ]
          (aref table 94)  #xB0   ; ^
          (aref table 95)  #x6D)  ; _
    (setf (aref table 96) #x79)   ; `
    ;; Lowercase a-z: #x81-#x89, #x91-#x99, #xA2-#xA9
    (loop :for i :from 0 :below 9  :do (setf (aref table (+ 97  i)) (+ #x81 i)))  ; a-i
    (loop :for i :from 0 :below 9  :do (setf (aref table (+ 106 i)) (+ #x91 i)))  ; j-r
    (loop :for i :from 0 :below 8  :do (setf (aref table (+ 115 i)) (+ #xA2 i)))  ; s-z
    (setf (aref table 123) #xC0   ; {
          (aref table 124) #x4F   ; |
          (aref table 125) #xD0   ; }
          (aref table 126) #xA1)  ; ~
    table)
  "ASCII code point → EBCDIC CP-037 byte translation table.")

(defun %ascii-to-ebcdic (string)
  "Convert a string to an EBCDIC CP-037 byte vector."
  (map '(vector (unsigned-byte 8))
       (lambda (c)
         (let ((code (char-code c)))
           (if (< code 128)
               (aref *ascii-to-ebcdic* code)
               #x40)))  ; unmapped → space
       string))

;; ===============================================================
;; GOFF record buffer — builds 80-byte records with continuations
;; ===============================================================

(defun %goff-make-record ()
  "Create a fresh 80-byte zero-filled record buffer."
  (make-array +goff-record-length+ :element-type '(unsigned-byte 8)
                                    :initial-element 0))

(defun %goff-set-ptv (record rec-type &optional (cont +goff-cont-none+))
  "Set the 3-byte PTV header of RECORD."
  (setf (aref record 0) +goff-ptv-prefix+
        (aref record 1) (logior rec-type cont)
        (aref record 2) #x00))

(defun %goff-set-u8 (record offset value)
  (setf (aref record offset) (logand #xFF value)))

(defun %goff-set-u16 (record offset value)
  (setf (aref record offset)       (logand #xFF (ash value -8))
        (aref record (1+ offset))  (logand #xFF value)))

(defun %goff-set-u32 (record offset value)
  (setf (aref record offset)       (logand #xFF (ash value -24))
        (aref record (+ offset 1)) (logand #xFF (ash value -16))
        (aref record (+ offset 2)) (logand #xFF (ash value -8))
        (aref record (+ offset 3)) (logand #xFF value)))

(defun %goff-write-record (stream record)
  "Write a single 80-byte GOFF record to STREAM."
  (loop :for b :across record :do (write-byte b stream)))

(defun %goff-write-name-into-record (record start-offset name-bytes
                                     &key max-bytes)
  "Copy NAME-BYTES into RECORD starting at START-OFFSET.
Returns the number of bytes that didn't fit (for continuation)."
  (let* ((available (or max-bytes (- +goff-record-length+ start-offset)))
         (to-copy (min available (length name-bytes))))
    (loop :for i :below to-copy
          :do (setf (aref record (+ start-offset i))
                    (aref name-bytes i)))
    (- (length name-bytes) to-copy)))

;; ===============================================================
;; Record writers
;; ===============================================================

(defun %goff-write-hdr (stream &key (arch-level 1))
  "Write a GOFF HDR record."
  (let ((rec (%goff-make-record)))
    (%goff-set-ptv rec +goff-rt-hdr+)
    ;; Bytes 3-47: reserved (zeros)
    ;; Bytes 48-51: Architecture Level
    (%goff-set-u32 rec 48 arch-level)
    ;; Bytes 52-53: Module Properties Length = 0
    ;; Rest: zeros
    (%goff-write-record stream rec)))

(defun %goff-write-esd (stream &key sym-type esdid parent-esdid
                                    (offset 0) (length 0)
                                    (namespace-id +goff-ns-normal+)
                                    (amode 0) (rmode 0)
                                    (executable 0) (alignment 0)
                                    (binding-strength 0) (binding-scope 0)
                                    (linkage 0) (read-only nil)
                                    name-string)
  "Write a GOFF ESD record with optional continuation for long names."
  (let* ((name-ebcdic (when name-string (%ascii-to-ebcdic name-string)))
         (name-len (if name-ebcdic (length name-ebcdic) 0))
         (rec (%goff-make-record))
         ;; Name bytes available in first record: bytes 72-79 = 8 bytes
         (name-first-avail 8)
         (needs-cont (> name-len name-first-avail)))
    ;; PTV
    (%goff-set-ptv rec +goff-rt-esd+ (if needs-cont +goff-cont-more+ +goff-cont-none+))
    ;; Byte 3: Symbol Type
    (%goff-set-u8 rec 3 sym-type)
    ;; Bytes 4-7: ESDID
    (%goff-set-u32 rec 4 esdid)
    ;; Bytes 8-11: Parent ESDID
    (%goff-set-u32 rec 8 (or parent-esdid 0))
    ;; Bytes 16-19: Offset (for LD/ED)
    (%goff-set-u32 rec 16 offset)
    ;; Bytes 24-27: Length (for ED/PR; -1 if deferred)
    (%goff-set-u32 rec 24 length)
    ;; Byte 40: Namespace ID
    (%goff-set-u8 rec 40 namespace-id)
    ;; Bytes 60-69: Behavioral Attributes (10 bytes)
    (%goff-set-u8 rec 60 amode)                                     ; AMODE
    (%goff-set-u8 rec 61 rmode)                                     ; RMODE
    (%goff-set-u8 rec 63 (logior (if read-only #x08 0)              ; Read-only (bit 4)
                                  (ash executable 0)))               ; Executable (bits 5-7)
    (%goff-set-u8 rec 64 (ash binding-strength 0))                  ; Binding strength (bits 4-7)
    (%goff-set-u8 rec 65 (ash binding-scope 0))                     ; Binding scope (bits 4-7)
    (%goff-set-u8 rec 66 (logior (ash linkage 5) alignment))        ; Linkage + Alignment
    ;; Bytes 70-71: Name Length
    (%goff-set-u16 rec 70 name-len)
    ;; Bytes 72-79: First part of name
    (when name-ebcdic
      (%goff-write-name-into-record rec 72 name-ebcdic
                                    :max-bytes name-first-avail))
    (%goff-write-record stream rec)

    ;; Continuation records for long names
    (when needs-cont
      (let ((remaining (subseq name-ebcdic name-first-avail)))
        (loop :while (plusp (length remaining))
              :do (let ((cont-rec (%goff-make-record))
                        (avail (- +goff-record-length+ 3)))  ; 77 bytes after PTV
                    (let ((more (> (length remaining) avail)))
                      (%goff-set-ptv cont-rec +goff-rt-esd+
                                     (if more +goff-cont-chain+ +goff-cont-last+))
                      (let ((to-copy (min avail (length remaining))))
                        (%goff-write-name-into-record cont-rec 3 remaining
                                                      :max-bytes to-copy)
                        (setf remaining (if (> (length remaining) to-copy)
                                            (subseq remaining to-copy)
                                            #()))))
                    (%goff-write-record stream cont-rec)))))))

(defun %goff-write-txt (stream &key esdid (offset 0) data)
  "Write GOFF TXT record(s) for a chunk of data.
DATA is a (vector (unsigned-byte 8)). Splits into multiple records
if data exceeds the available space per record."
  (let ((data-len (length data))
        (pos 0))
    (loop :while (< pos data-len)
          :do (let* ((rec (%goff-make-record))
                     ;; First record: 24 bytes header, 56 bytes data max
                     ;; Continuation: 3 bytes PTV, 77 bytes data max
                     (first-p (= pos 0))
                     (avail (if first-p
                                (- +goff-record-length+ 24)  ; 56 bytes
                                (- +goff-record-length+ 3))) ; 77 bytes
                     (chunk-len (min avail (- data-len pos)))
                     (more (< (+ pos chunk-len) data-len)))
                (if first-p
                    (progn
                      (%goff-set-ptv rec +goff-rt-txt+
                                     (if more +goff-cont-more+ +goff-cont-none+))
                      ;; Byte 3: Style = byte-oriented (0)
                      ;; Bytes 4-7: Element ESDID
                      (%goff-set-u32 rec 4 esdid)
                      ;; Bytes 12-15: Offset within element
                      (%goff-set-u32 rec 12 offset)
                      ;; Bytes 22-23: Data Length
                      (%goff-set-u16 rec 22 data-len)
                      ;; Bytes 24+: data
                      (loop :for i :below chunk-len
                            :do (setf (aref rec (+ 24 i))
                                      (aref data (+ pos i)))))
                    (progn
                      (%goff-set-ptv rec +goff-rt-txt+
                                     (if more +goff-cont-chain+ +goff-cont-last+))
                      ;; Bytes 3+: continuation data
                      (loop :for i :below chunk-len
                            :do (setf (aref rec (+ 3 i))
                                      (aref data (+ pos i))))))
                (incf pos chunk-len)
                (%goff-write-record stream rec)))))

(defun %goff-write-rld (stream &key r-esdid p-esdid offset
                                    (ref-type 0) (referent-type 0)
                                    (target-length 4) (action 0))
  "Write a GOFF RLD record with a single relocation item."
  (let ((rec (%goff-make-record)))
    (%goff-set-ptv rec +goff-rt-rld+)
    ;; Bytes 4-5: Data Length (total bytes of RLD items)
    ;; A single full item is: 2 flags + 2 reserved + 4 target-length/reserved
    ;;                        + 4 R-pointer + 4 P-pointer + 4 offset = 20 bytes
    (%goff-set-u16 rec 4 20)
    ;; RLD item at byte 6:
    ;; Byte 0 of item (byte 6 of record): flags
    ;;   bit 0: same R_ID as prev (0=no)
    ;;   bit 1: same P_ID as prev (0=no)
    ;;   bit 2: same offset as prev (0=no)
    ;;   bit 6: offset length (0=4-byte)
    (%goff-set-u8 rec 6 0)
    ;; Byte 1 of item (byte 7): ref-type (bits 0-3), referent-type (bits 4-7)
    (%goff-set-u8 rec 7 (logior (ash ref-type 4) referent-type))
    ;; Byte 2 of item (byte 8): action (bit 0) + fetch/store (bit 7)
    (%goff-set-u8 rec 8 (ash action 7))
    ;; Byte 3 of item (byte 9): reserved
    ;; Byte 4 of item (byte 10): target length
    (%goff-set-u8 rec 10 target-length)
    ;; Byte 5 of item (byte 11): reserved
    ;; Bytes 6-7: reserved (bytes 12-13)
    ;; Bytes 8-11 of item: R_Pointer (bytes 14-17)
    (%goff-set-u32 rec 14 r-esdid)
    ;; Bytes 12-15 of item: P_Pointer (bytes 18-21)
    (%goff-set-u32 rec 18 p-esdid)
    ;; Bytes 16-19 of item: Offset (bytes 22-25)
    (%goff-set-u32 rec 22 offset)
    (%goff-write-record stream rec)))

(defun %goff-write-end-old (stream &key (entry-esdid nil) (entry-offset 0)
                                    (amode 0) (record-count 0))
  "Write a GOFF END record."
  (let ((rec (%goff-make-record)))
    (%goff-set-ptv rec +goff-rt-end+)
    ;; Byte 3: bits 6-7 = entry point flag
    ;;   0 = no entry, 1 = by ESDID, 2 = by name
    (%goff-set-u8 rec 3 (if entry-esdid 1 0))
    ;; Byte 4: AMODE of entry point
    (%goff-set-u8 rec 4 amode)
    ;; Bytes 8-11: Record Count
    (%goff-set-u32 rec 8 record-count)
    ;; Bytes 12-15: ESDID (if entry by ESDID)
    (when entry-esdid
      (%goff-set-u32 rec 12 entry-esdid))
    ;; Bytes 20-23: Offset
    (%goff-set-u32 rec 20 entry-offset)

    (print (list :rr rec))
    (%goff-write-record stream rec)))

(defmanifest goff-end (:unit 8 :endian :big :length 80)
  (masque "h:pptt00" (p +goff-ptv-prefix+)
          (t :t-tag :u8 :default 0))
  (:entry-flag   :u8)  ;; Byte 3: bits 6-7 = entry point flag
  (:amode        :u8)  ;; Byte 4: AMODE of entry point
  (pad 0 3)            ;; Bytes 5-7: zero
  (:record-count :u32) ;; Bytes 8-11: Record Count
  (:entry-esdid  :u32) ;; Bytes 12-15: ESDID (if entry by ESDID)
  (pad 0 4)            ;; Bytes 16-19: zero
  (:entry-offset :u32) ;; Bytes 20-23: Offset
  (pad 0 :to 80))      ;; Bytes 24-79: Complete the record with zero-padding

(defun %goff-write-end (stream &key (entry-esdid nil) (entry-offset 0)
                                    (amode 0) (record-count 0))
  "Write a GOFF END record."
  (marshal2 goff-end stream
    :t-tag (logior +goff-rt-end+ +goff-cont-none+)
    :entry-flag (if entry-esdid 1 0) :amode amode :record-count record-count
    :entry-esdid entry-esdid :entry-offset entry-offset))

;; (defun %goff-write-end (stream &key (entry-esdid nil) (entry-offset 0)
;;                                     (amode 0) (record-count 0))
;;   "Write a GOFF END record."
;;   (marshal stream (:unit 8 :endian :big :index index)
;;     (masque "h:pptt00"
;;             (p +goff-ptv-prefix+) (t (logior +goff-rt-end+ +goff-cont-none+)))
;;     (if entry-esdid 1 0) ;; Byte 3: bits 6-7 = entry point flag
;;     amode                ;; Byte 4: AMODE of entry point
;;     (3)                  ;; Bytes 5-7: zero
;;     (4 . record-count)   ;; Bytes 8-11: Record Count
;;     (4 . entry-esdid)    ;; Bytes 12-15: ESDID (if entry by ESDID)
;;     (4)                  ;; Bytes 16-19: zero
;;     (4 . entry-offset)   ;; Bytes 20-23: Offset
;;     (:pad 0 :to 80)))    ;; Bytes 24-79: Complete the record with zero-padding

;; ===============================================================
;; Alignment to GOFF alignment code
;; ===============================================================

(defun %goff-alignment-code (byte-alignment)
  "Convert a byte alignment value to a GOFF alignment code.
1→0(byte), 2→1(halfword), 4→2(fullword), 8→3(doubleword),
16→4(quadword), 32→5, 4096→12."
  (if (<= byte-alignment 1) 0
      (min 12 (floor (log byte-alignment 2)))))

;; ===============================================================
;; emit-program :goff — GOFF object file emitter
;; ===============================================================

(defmethod emit-program ((pgm program) (format (eql :goff)) stream &key)
  "Write a GOFF object file to STREAM.
The program must have been assembled with build-program first.

Produces:
  HDR record
  ESD records (SD for each unit, ED for each segment, LD for each symbol)
  TXT records (code/data bytes for each segment)
  RLD records (relocations)
  END record

Names are converted to EBCDIC CP-037."
  (let* ((assembler (pgm-assembler pgm))
         (record-count 0)
         (next-esdid 1)
         ;; Maps: segment → esdid, symbol → esdid
         (seg-esdid-map (make-hash-table :test #'eq))
         (sym-esdid-map (make-hash-table :test #'eq))
         ;; Collect all ESD, TXT, RLD info for ordered emission
         (esd-entries nil)
         (txt-entries nil)
         (rld-entries nil)
         ;; Entry point tracking
         (entry-esdid nil)
         (entry-offset 0)
         (entry-amode 0))

    (flet ((alloc-esdid ()
             (prog1 next-esdid (incf next-esdid))))

      ;; -- Pass 1: Build ESD entries --------------------------

      (dolist (unit (pgm-units pgm))
        ;; SD (Section Definition) for the unit
        (let ((sd-esdid (alloc-esdid)))
          (push (list :sd sd-esdid
                      :name (or (pun-name unit) "UNNAMED")
                      :amode (or (getf (pun-properties unit) :goff-amode) 0)
                      :rmode (or (getf (pun-properties unit) :goff-rmode) 0))
                esd-entries)

          ;; ED (Element Definition) for each segment
          (loop :for (seg-name . seg) :in (pun-segments unit)
                :when (member (seg-kind seg) '(:code :data :rodata))
                :do (let* ((ed-esdid (alloc-esdid))
                           (octets (%seg-bytes-as-octets seg assembler))
                           (seg-len (length octets)))
                      (setf (gethash seg seg-esdid-map) ed-esdid)
                      (push (list :ed ed-esdid
                                  :parent sd-esdid
                                  :name seg-name
                                  :length seg-len
                                  :amode (or (getf (seg-properties seg) :goff-amode) 0)
                                  :rmode (or (getf (seg-properties seg) :goff-rmode) 0)
                                  :executable (or (getf (seg-properties seg) :goff-exec) 0)
                                  :alignment (%goff-alignment-code (seg-align seg))
                                  :read-only (not (member :write (seg-flags seg)))
                                  :namespace-id +goff-ns-binder+)
                            esd-entries)
                      ;; TXT record for this ED's content
                      (when (plusp seg-len)
                        (push (list :txt ed-esdid octets) txt-entries))))

          ;; LD (Label Definition) for each symbol
          (maphash
           (lambda (name sym)
             (declare (ignore name))
             (when (and (sym-segment sym)
                        (gethash (sym-segment sym) seg-esdid-map))
               (let* ((ld-esdid (alloc-esdid))
                      (parent-ed-esdid (gethash (sym-segment sym) seg-esdid-map))
                      (sym-name-str (string (sym-name sym)))
                      ;; Compute byte offset from assembler units
                      (raw-bytes (seg-bytes (sym-segment sym)))
                      (unit-bytes (cond ((and raw-bytes
                                              (equal (array-element-type raw-bytes)
                                                     '(unsigned-byte 16))) 2)
                                        ((and raw-bytes
                                              (equal (array-element-type raw-bytes)
                                                     '(unsigned-byte 32))) 4)
                                        (t 1)))
                      (byte-offset (* (sym-offset sym) unit-bytes))
                      (byte-size (* (sym-size sym) unit-bytes))
                      (is-function (eq :function (sym-type sym)))
                      (is-global (member (sym-binding sym) '(:global :weak)))
                      (sym-props (sym-properties sym)))
                 (setf (gethash sym sym-esdid-map) ld-esdid)
                 (push (list (if is-function :ld :ld) ld-esdid
                             :parent parent-ed-esdid
                             :name sym-name-str
                             :offset byte-offset
                             :length byte-size
                             :amode (or (getf sym-props :goff-amode) 0)
                             :executable (if is-function 2 0)  ; 2=code
                             :binding-scope (if is-global 2 1) ; 2=module, 1=section
                             :linkage (or (getf sym-props :goff-linkage) 0)
                             :namespace-id +goff-ns-normal+)
                       esd-entries)
                 ;; Check if this is the entry point
                 (when (and (pgm-entry-point pgm)
                            (eq (sym-name sym) (pgm-entry-point pgm)))
                   (setf entry-esdid ld-esdid
                         entry-offset byte-offset
                         entry-amode (or (getf sym-props :goff-amode) 0))))))
           (pun-symbols unit))

          ;; RLD entries from unit's relocations
          (dolist (rel (pun-relocations unit))
            (let ((r-sym (rel-symbol rel))
                  (p-seg (rel-segment rel)))
              (when (and r-sym p-seg)
                (let ((r-esdid (or (gethash r-sym sym-esdid-map)
                                   ;; Try looking up by name in the segment map
                                   (let ((target-seg (sym-segment r-sym)))
                                     (when target-seg
                                       (gethash target-seg seg-esdid-map)))))
                      (p-esdid (gethash p-seg seg-esdid-map)))
                  (when (and r-esdid p-esdid)
                    (push (list r-esdid p-esdid
                                (rel-offset rel)
                                (rel-width rel)
                                (rel-type rel))
                          rld-entries))))))))

      ;; Reverse to get emission order
      (setf esd-entries (nreverse esd-entries)
            txt-entries (nreverse txt-entries)
            rld-entries (nreverse rld-entries))

      ;; -- Pass 2: Write records ------------------------------

      ;; HDR
      (%goff-write-hdr stream)
      (incf record-count)

      ;; ESD records
      (dolist (entry esd-entries)
        (let ((kind (first entry))
              (esdid (second entry))
              (props (cddr entry)))
          (ecase kind
            (:sd
             (%goff-write-esd stream
                              :sym-type +goff-est-sd+
                              :esdid esdid
                              :parent-esdid 0
                              :namespace-id +goff-ns-normal+
                              :amode (getf props :amode 0)
                              :rmode (getf props :rmode 0)
                              :name-string (getf props :name)))
            (:ed
             (%goff-write-esd stream
                              :sym-type +goff-est-ed+
                              :esdid esdid
                              :parent-esdid (getf props :parent)
                              :length (getf props :length 0)
                              :namespace-id (getf props :namespace-id +goff-ns-binder+)
                              :amode (getf props :amode 0)
                              :rmode (getf props :rmode 0)
                              :executable (getf props :executable 0)
                              :alignment (getf props :alignment 0)
                              :read-only (getf props :read-only)
                              :name-string (getf props :name)))
            (:ld
             (%goff-write-esd stream
                              :sym-type +goff-est-ld+
                              :esdid esdid
                              :parent-esdid (getf props :parent)
                              :offset (getf props :offset 0)
                              :length (getf props :length 0)
                              :namespace-id (getf props :namespace-id +goff-ns-normal+)
                              :amode (getf props :amode 0)
                              :executable (getf props :executable 0)
                              :binding-scope (getf props :binding-scope 0)
                              :linkage (getf props :linkage 0)
                              :name-string (getf props :name)))))
        (incf record-count))

      ;; TXT records
      (dolist (entry txt-entries)
        (destructuring-bind (tag esdid data) entry
          (declare (ignore tag))
          (%goff-write-txt stream :esdid esdid :offset 0 :data data)
          ;; Count records: 1 per 56 bytes of data in first record,
          ;; then 1 per 77 bytes for continuations
          (incf record-count
                (if (<= (length data) 56) 1
                    (1+ (ceiling (- (length data) 56) 77))))))

      ;; RLD records
      (dolist (entry rld-entries)
        (destructuring-bind (r-esdid p-esdid offset width rel-type) entry
          (declare (ignore rel-type)) ; TODO: map to GOFF RLD ref-type
          (%goff-write-rld stream
                           :r-esdid r-esdid
                           :p-esdid p-esdid
                           :offset offset
                           :target-length width
                           :ref-type 0   ; R-address (default)
                           :referent-type 0)) ; Label
        (incf record-count))

      ;; END record
      (%goff-write-end stream
                       :entry-esdid entry-esdid
                       :entry-offset entry-offset
                       :amode entry-amode
                       :record-count (1+ record-count))  ; +1 for the END itself

      ;; Return total record count
      (1+ record-count))))

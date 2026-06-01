;;;; system-z/program.lisp — System Z program model extensions
;;;
;;; Provides GOFF-oriented helpers for building System Z programs:
;;; standard segment constructors, GOFF attribute constants,
;;; and convenience functions for typical z/Architecture program layout.

(in-package #:specops.system-z)

;; Import program model symbols from the core specops package
;; (import '(specops::program specops::pgm-assembler specops::pgm-units
;;           specops::pgm-entry-point specops::pgm-properties
;;           specops::program-unit specops::pun-name specops::pun-program
;;           specops::pun-segments specops::pun-symbols specops::pun-relocations
;;           specops::pun-properties
;;           specops::segment specops::seg-name specops::seg-kind specops::seg-items
;;           specops::seg-origin specops::seg-size specops::seg-align
;;           specops::seg-bytes specops::seg-flags specops::seg-properties
;;           specops::symbol-entry specops::sym-name specops::sym-segment
;;           specops::sym-offset specops::sym-binding specops::sym-type
;;           specops::sym-size specops::sym-properties
;;           specops::relocation specops::rel-symbol specops::rel-segment
;;           specops::rel-offset specops::rel-type specops::rel-width specops::rel-addend
;;           specops::segment-item specops::raw-bytes-item specops::rbi-data
;;           specops::instruction-item specops::ii-expression
;;           specops::label-def-item specops::ldi-name specops::ldi-binding
;;           specops::label-ref-item specops::lri-label specops::lri-width
;;           specops::lri-rel-type specops::lri-addend
;;           specops::align-item specops::ali-boundary specops::ali-fill-byte
;;           specops::function-item specops::fi-name specops::fi-body specops::fi-properties
;;           specops::add-segment specops::add-symbol specops::add-relocation
;;           specops::add-item specops::add-unit
;;           specops::lookup-segment specops::lookup-symbol specops::finalize-segment
;;           specops::make-program specops::make-unit specops::make-segment
;;           specops::make-symbol-entry
;;           specops::emit-program specops::build-program
;;           specops::asm-endianness specops::asm-elf-machine
;;           specops::data-word-item specops::dwi-value specops::dwi-width))

;; ===============================================================
;; GOFF behavioral attribute constants
;; ===============================================================

;; AMODE (addressing mode) values — byte 0 of behavioral attributes
(defconstant +goff-amode-unspecified+ 0)
(defconstant +goff-amode-24+          1)
(defconstant +goff-amode-31+          2)
(defconstant +goff-amode-any+         3)
(defconstant +goff-amode-64+          4)
(defconstant +goff-amode-min+        16)

;; RMODE (residency mode) values — byte 1 of behavioral attributes
(defconstant +goff-rmode-unspecified+ 0)
(defconstant +goff-rmode-24+          1)
(defconstant +goff-rmode-31+          3)  ; same value as RMODE ANY
(defconstant +goff-rmode-64+          4)

;; Executable type — byte 3, bits 5-7
(defconstant +goff-exec-unspecified+ 0)
(defconstant +goff-exec-data+        1)
(defconstant +goff-exec-code+        2)

;; Binding strength — byte 4, bits 4-7
(defconstant +goff-binding-strong+ 0)
(defconstant +goff-binding-weak+   1)

;; Binding scope — byte 5, bits 4-7
(defconstant +goff-scope-unspecified+    0)
(defconstant +goff-scope-section+        1)
(defconstant +goff-scope-module+         2)
(defconstant +goff-scope-library+        3)
(defconstant +goff-scope-import-export+  4)

;; Linkage type — byte 6, bit 2
(defconstant +goff-linkage-os+     0)
(defconstant +goff-linkage-xplink+ 1)

;; Alignment values — byte 6, bits 3-7
(defconstant +goff-align-byte+       0)
(defconstant +goff-align-halfword+   1)
(defconstant +goff-align-fullword+   2)
(defconstant +goff-align-doubleword+ 3)
(defconstant +goff-align-quadword+   4)
(defconstant +goff-align-32byte+     5)
(defconstant +goff-align-4k-page+   12)

;; ESD symbol types
(defconstant +goff-sym-sd+ #x00 "Section Definition")
(defconstant +goff-sym-ed+ #x01 "Element Definition")
(defconstant +goff-sym-ld+ #x02 "Label Definition")
(defconstant +goff-sym-pr+ #x03 "Part Reference")
(defconstant +goff-sym-er+ #x04 "External Reference")

;; GOFF record PTV prefix byte (always #x03 for GOFF)
(defconstant +goff-ptv-prefix+ #x03)

;; Record type nibbles (high nibble of PTV byte 1)
(defconstant +goff-rec-esd+ #x00)
(defconstant +goff-rec-txt+ #x10)
(defconstant +goff-rec-rld+ #x20)
(defconstant +goff-rec-len+ #x30)
(defconstant +goff-rec-end+ #x40)
(defconstant +goff-rec-hdr+ #xF0)

;; RLD reference types — byte 1 of RLD item flags, bits 0-3
(defconstant +goff-rld-r-address+             0)
(defconstant +goff-rld-r-offset+              1)
(defconstant +goff-rld-r-length+              2)
(defconstant +goff-rld-relative-immediate+    6)
(defconstant +goff-rld-r-constant+            7)
(defconstant +goff-rld-long-displacement+     9)

;; s390x ELF relocation type codes (for ELF output targeting s390x Linux)
(defconstant +r-390-none+      0)
(defconstant +r-390-8+         1)
(defconstant +r-390-12+        2)
(defconstant +r-390-16+        3)
(defconstant +r-390-32+        4)
(defconstant +r-390-pc32+      5)
(defconstant +r-390-got12+     6)
(defconstant +r-390-got32+     7)
(defconstant +r-390-plt32+     8)
(defconstant +r-390-pc16+     14)
(defconstant +r-390-pc16dbl+  16)
(defconstant +r-390-plt16dbl+ 17)
(defconstant +r-390-pc32dbl+  18)
(defconstant +r-390-plt32dbl+ 19)
(defconstant +r-390-gotpcdbl+ 20)
(defconstant +r-390-64+       22)
(defconstant +r-390-pc64+     23)
(defconstant +r-390-got64+    24)
(defconstant +r-390-plt64+    25)
(defconstant +r-390-gotent+   26)

;; Mapping from relocation type keywords to numeric codes
(defparameter *z-reloc-types*
  '((:r-390-none     .  0) (:r-390-8        .  1)
    (:r-390-12       .  2) (:r-390-16       .  3)
    (:r-390-32       .  4) (:r-390-pc32     .  5)
    (:r-390-got12    .  6) (:r-390-got32    .  7)
    (:r-390-plt32    .  8) (:r-390-pc16     . 14)
    (:r-390-pc16dbl  . 16) (:r-390-plt16dbl . 17)
    (:r-390-pc32dbl  . 18) (:r-390-plt32dbl . 19)
    (:r-390-gotpcdbl . 20) (:r-390-64       . 22)
    (:r-390-pc64     . 23) (:r-390-got64    . 24)
    (:r-390-plt64    . 25) (:r-390-gotent   . 26))
  "Alist mapping relocation type keywords to s390x ELF R_390_* numeric codes.")

;; ===============================================================
;; Standard GOFF class names
;; ===============================================================

(defparameter *goff-class-code64*  "C_CODE64"
  "Standard GOFF class name for 64-bit executable code.")
(defparameter *goff-class-data64*  "C_DATA64"
  "Standard GOFF class name for 64-bit read-only data.")
(defparameter *goff-class-wsa64*   "C_WSA64"
  "Standard GOFF class name for 64-bit writable static area.")

;; ===============================================================
;; Standard segment constructors for System Z
;; ===============================================================

(defun make-z-code-segment (&key (name *goff-class-code64*) (align 8))
  "Create a standard System Z code segment.
Defaults to the C_CODE64 GOFF class name with doubleword alignment."
  (make-segment name :code
                :align align
                :flags '(:read :execute :alloc)
                :properties (list :goff-amode +goff-amode-64+
                                  :goff-rmode +goff-rmode-64+
                                  :goff-exec  +goff-exec-code+)))

(defun make-z-data-segment (&key (name *goff-class-wsa64*) (align 8))
  "Create a standard System Z writable data segment.
Defaults to the C_WSA64 GOFF class name with doubleword alignment."
  (make-segment name :data
                :align align
                :flags '(:read :write :alloc)
                :properties (list :goff-amode +goff-amode-64+
                                  :goff-rmode +goff-rmode-64+
                                  :goff-exec  +goff-exec-data+)))

(defun make-z-rodata-segment (&key (name *goff-class-data64*) (align 8))
  "Create a standard System Z read-only data segment.
Defaults to the C_DATA64 GOFF class name with doubleword alignment."
  (make-segment name :rodata
                :align align
                :flags '(:read :alloc)
                :properties (list :goff-amode +goff-amode-64+
                                  :goff-rmode +goff-rmode-64+
                                  :goff-exec  +goff-exec-data+)))

;; ===============================================================
;; Convenience: build a typical System Z program skeleton
;; ===============================================================

(defun make-z-program (unit-name &key (assembler *assembler-prototype-z*)
                                      (amode 64) (rmode 64))
  "Create a program with a single program-unit configured for System Z.
Sets up the standard C_CODE64 and C_WSA64 segments within the unit.
Returns (values program unit code-segment data-segment)."
  (let* ((pgm  (make-program assembler))
         (unit (make-unit unit-name pgm))
         (code (make-z-code-segment))
         (data (make-z-data-segment)))
    ;; Store AMODE/RMODE on the unit for the SD record
    (setf (pun-properties unit)
          (list :goff-amode (ecase amode (24 +goff-amode-24+) (31 +goff-amode-31+)
                                         (64 +goff-amode-64+))
                :goff-rmode (ecase rmode (24 +goff-rmode-24+) (31 +goff-rmode-31+)
                                         (64 +goff-rmode-64+))))
    (add-segment unit "C_CODE64" code)
    (add-segment unit "C_WSA64"  data)
    (values pgm unit code data)))

;; ===============================================================
;; System Z function item helpers
;; ===============================================================

(defun make-z-function (name &key (amode 64) (linkage :os) (binding :global))
  "Create a function-item configured for System Z conventions.
Sets GOFF-specific properties for AMODE and linkage type."
  (make-instance 'function-item
                 :name name
                 :properties (list :goff-amode (ecase amode
                                                 (24 +goff-amode-24+)
                                                 (31 +goff-amode-31+)
                                                 (64 +goff-amode-64+))
                                   :goff-linkage (ecase linkage
                                                   (:os     +goff-linkage-os+)
                                                   (:xplink +goff-linkage-xplink+))
                                   :binding binding)))

;; ===============================================================
;; Relocation type helpers
;; ===============================================================

(defun z-reloc-code (type-keyword)
  "Look up the numeric ELF relocation code for a System Z relocation type keyword."
  (or (rest (assoc type-keyword *z-reloc-types*))
      (error "Unknown s390x relocation type: ~a" type-keyword)))

(defun make-z-reloc-pc32dbl (symbol segment offset &key (addend 0))
  "Create a PC-relative halfword-shifted relocation (R_390_PC32DBL).
This is the most common relocation type for s390x branch instructions
such as BRASL, BRCL, and LARL."
  (make-instance 'relocation :symbol symbol :segment segment
                              :offset offset :rel-type :r-390-pc32dbl
                              :width 4 :addend addend))

(defun make-z-reloc-64 (symbol segment offset &key (addend 0))
  "Create a 64-bit absolute relocation (R_390_64).
Used for absolute data references in 64-bit code."
  (make-instance 'relocation :symbol symbol :segment segment
                              :offset offset :rel-type :r-390-64
                              :width 8 :addend addend))

(defun make-z-reloc-32 (symbol segment offset &key (addend 0))
  "Create a 32-bit absolute relocation (R_390_32)."
  (make-instance 'relocation :symbol symbol :segment segment
                              :offset offset :rel-type :r-390-32
                              :width 4 :addend addend))

;; ===============================================================
;; Endianness for System Z
;; ===============================================================

(defmethod asm-endianness ((assembler assembler-z))
  "System Z is big-endian."
  :big)

(defmethod asm-elf-machine ((assembler assembler-z))
  "System Z uses EM_S390 (22) for both 31-bit and 64-bit; class distinguishes them."
  22)

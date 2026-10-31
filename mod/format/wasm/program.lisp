;;;; wasm/program.lisp — WebAssembly program model extensions
;;;
;;; Provides WASM-specific segment-item subclasses for the typed
;;; sections of a WebAssembly binary module, plus convenience
;;; constructors and constants for building WASM programs.

(in-package #:specops.wasm)

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
;;           specops::emit-program specops::build-program specops::asm-endianness
;;           specops::data-word-item specops::dwi-value specops::dwi-width))

;; ===============================================================
;; WASM constants
;; ===============================================================

;; Module header
(defparameter *wasm-magic*   #(#x00 #x61 #x73 #x6D) "WASM magic number: \\0asm")
(defparameter *wasm-version* #(#x01 #x00 #x00 #x00) "WASM version 1 (little-endian u32).")

;; Section IDs (in prescribed order, except custom which may appear anywhere)
(defconstant +wasm-section-custom+     0)
(defconstant +wasm-section-type+       1)
(defconstant +wasm-section-import+     2)
(defconstant +wasm-section-function+   3)
(defconstant +wasm-section-table+      4)
(defconstant +wasm-section-memory+     5)
(defconstant +wasm-section-global+     6)
(defconstant +wasm-section-export+     7)
(defconstant +wasm-section-start+      8)
(defconstant +wasm-section-element+    9)
(defconstant +wasm-section-code+      10)
(defconstant +wasm-section-data+      11)
(defconstant +wasm-section-data-count+ 12)
(defconstant +wasm-section-tag+       13)

;; Mapping from segment :kind keywords to WASM section IDs
(defparameter *wasm-section-ids*
  '((:wasm-custom     .  0) (:wasm-type       .  1)
    (:wasm-import     .  2) (:wasm-function   .  3)
    (:wasm-table      .  4) (:wasm-memory     .  5)
    (:wasm-global     .  6) (:wasm-export     .  7)
    (:wasm-start      .  8) (:wasm-element    .  9)
    (:wasm-code       . 10) (:wasm-data       . 11)
    (:wasm-data-count . 12) (:wasm-tag        . 13))
  "Alist mapping WASM segment kind keywords to section ID bytes.")

;; Prescribed section ordering (custom sections excluded; they float)
(defparameter *wasm-section-order*
  '(:wasm-type :wasm-import :wasm-function :wasm-table :wasm-memory
    :wasm-tag :wasm-global :wasm-export :wasm-start :wasm-element
    :wasm-data-count :wasm-code :wasm-data)
  "Prescribed ordering of WASM sections in a binary module.")

;; Value type encodings
(defconstant +wasm-valtype-i32+       #x7F)
(defconstant +wasm-valtype-i64+       #x7E)
(defconstant +wasm-valtype-f32+       #x7D)
(defconstant +wasm-valtype-f64+       #x7C)
(defconstant +wasm-valtype-v128+      #x7B)
(defconstant +wasm-valtype-funcref+   #x70)
(defconstant +wasm-valtype-externref+ #x6F)

(defparameter *wasm-valtypes*
  '((:i32       . #x7F) (:i64       . #x7E)
    (:f32       . #x7D) (:f64       . #x7C)
    (:v128      . #x7B) (:funcref   . #x70)
    (:externref . #x6F))
  "Alist mapping valtype keywords to their single-byte encodings.")

;; Block type sentinel
(defconstant +wasm-blocktype-void+ #x40
  "Block type byte for void blocks: [] -> [].")

;; Export/import kind bytes
(defconstant +wasm-kind-func+   0)
(defconstant +wasm-kind-table+  1)
(defconstant +wasm-kind-memory+ 2)
(defconstant +wasm-kind-global+ 3)
(defconstant +wasm-kind-tag+    4)

(defparameter *wasm-ext-kinds*
  '((:func . 0) (:table . 1) (:memory . 2) (:global . 3) (:tag . 4))
  "Alist mapping external kind keywords to their byte encodings.")

;; Structured control flow opcodes
(defconstant +wasm-op-block+       #x02)
(defconstant +wasm-op-loop+        #x03)
(defconstant +wasm-op-if+          #x04)
(defconstant +wasm-op-else+        #x05)
(defconstant +wasm-op-end+         #x0B)

;; Function type tag
(defconstant +wasm-functype-tag+   #x60
  "Byte tag introducing a function type in the Type section.")

;; ===============================================================
;; WASM-specific segment items
;; ===============================================================

;; -------------------------------------------------------------
;; WASM function — extends function-item with type and locals
;; -------------------------------------------------------------

(defclass wasm-function-item (function-item)
  ((%type-index :accessor wfi-type-index
                :initform 0
                :initarg  :type-index
                :documentation "Index into the WASM Type section for this function's signature.")
   (%locals     :accessor wfi-locals
                :initform nil
                :initarg  :locals
                :documentation "Local variable declarations as a list of (count . valtype) pairs,
e.g. ((3 . :i32) (2 . :f64)).  Parameters come from the type signature
and are not included here."))
  (:documentation
   "A WebAssembly function.  Extends function-item with the type index
and local variable declarations required by the WASM Code section.
The body contains instruction-items and wasm-block-items.
Serialized as: size-prefix, compressed locals, instruction bytes, 0x0B (end)."))

;; -------------------------------------------------------------
;; Structured control flow block
;; -------------------------------------------------------------

(defclass wasm-block-item (segment-item)
  ((%kind      :accessor wbi-kind
               :initform :block
               :initarg  :kind
               :documentation "Block kind: :block, :loop, or :if.
Encoded as opcodes 0x02, 0x03, 0x04 respectively.")
   (%blocktype :accessor wbi-blocktype
               :initform nil
               :initarg  :blocktype
               :documentation "Block type:
  NIL            -> 0x40 (void, [] -> [])
  a valtype kw   -> single-result shorthand, e.g. :i32
  integer >= 0   -> type index for multi-value blocks (positive s33)")
   (%body      :accessor wbi-body
               :initform nil
               :initarg  :body
               :documentation "List of segment-items forming the block body.
May contain further wasm-block-items for nested control flow.")
   (%else-body :accessor wbi-else-body
               :initform nil
               :initarg  :else-body
               :documentation "List of segment-items for the else branch of an :if block.
NIL if this is not an :if block, or if there is no else branch."))
  (:documentation
   "A WASM structured control flow block (block/loop/if).
Nests recursively: the body and else-body may contain further
wasm-block-items.
Serialized as: opcode, blocktype, body instructions, [0x05 else-body,] 0x0B."))

;; -------------------------------------------------------------
;; Type section entry — function signature
;; -------------------------------------------------------------

(defclass wasm-type-item (segment-item)
  ((%params  :accessor wti-params
             :initform nil
             :initarg  :params
             :documentation "Parameter types as a list of valtype keywords, e.g. (:i32 :i32).")
   (%results :accessor wti-results
             :initform nil
             :initarg  :results
             :documentation "Result types as a list of valtype keywords, e.g. (:i32)."))
  (:documentation
   "A WASM function type (signature) entry for the Type section.
Serialized as: 0x60, LEB128 param-count, param-type-bytes...,
LEB128 result-count, result-type-bytes...."))

;; -------------------------------------------------------------
;; Import declaration
;; -------------------------------------------------------------

(defclass wasm-import-item (segment-item)
  ((%module-name :accessor wii-module-name
                 :initform nil
                 :initarg  :module-name
                 :documentation "Import module name (a UTF-8 string).")
   (%field-name  :accessor wii-field-name
                 :initform nil
                 :initarg  :field-name
                 :documentation "Import field name (a UTF-8 string).")
   (%import-kind :accessor wii-import-kind
                 :initform nil
                 :initarg  :import-kind
                 :documentation "Import descriptor kind: :func, :table, :memory, :global, or :tag.")
   (%descriptor  :accessor wii-descriptor
                 :initform nil
                 :initarg  :descriptor
                 :documentation "Type descriptor for the import.
  :func   -> type index (integer)
  :table  -> (reftype min &optional max)
  :memory -> (min &optional max)
  :global -> (valtype mutability-keyword)
  :tag    -> type index (integer)"))
  (:documentation
   "A WASM import declaration for the Import section.
Each import specifies a two-level namespace (module.field) and a
typed descriptor.  Imported entities are numbered before locally-
defined ones in their respective index spaces."))

;; -------------------------------------------------------------
;; Export declaration
;; -------------------------------------------------------------

(defclass wasm-export-item (segment-item)
  ((%export-name :accessor wei-export-name
                 :initform nil
                 :initarg  :export-name
                 :documentation "Exported name (a UTF-8 string).")
   (%export-kind :accessor wei-export-kind
                 :initform nil
                 :initarg  :export-kind
                 :documentation "Export kind: :func, :table, :memory, :global, or :tag.")
   (%index       :accessor wei-index
                 :initform 0
                 :initarg  :index
                 :documentation "Index in the corresponding WASM index space."))
  (:documentation
   "A WASM export declaration for the Export section.
Maps an external name to an entity in one of the five index spaces."))

;; -------------------------------------------------------------
;; Memory declaration
;; -------------------------------------------------------------

(defclass wasm-memory-item (segment-item)
  ((%min-pages :accessor wmi-min-pages
               :initform 1
               :initarg  :min-pages
               :documentation "Minimum memory size in 64KiB pages.")
   (%max-pages :accessor wmi-max-pages
               :initform nil
               :initarg  :max-pages
               :documentation "Maximum memory size in pages, or NIL for no maximum."))
  (:documentation
   "A WASM linear memory declaration for the Memory section.
Serialized as a limits encoding (flag byte + LEB128 min [+ LEB128 max]).
Each page is 64KiB."))

;; -------------------------------------------------------------
;; Table declaration
;; -------------------------------------------------------------

(defclass wasm-table-item (segment-item)
  ((%reftype   :accessor wtbi-reftype
               :initform :funcref
               :initarg  :reftype
               :documentation "Reference type: :funcref or :externref.")
   (%min-size  :accessor wtbi-min-size
               :initform 0
               :initarg  :min-size
               :documentation "Minimum table size (number of entries).")
   (%max-size  :accessor wtbi-max-size
               :initform nil
               :initarg  :max-size
               :documentation "Maximum table size, or NIL for no maximum."))
  (:documentation
   "A WASM table declaration for the Table section.
Serialized as: reftype byte, limits encoding."))

;; -------------------------------------------------------------
;; Global declaration
;; -------------------------------------------------------------

(defclass wasm-global-item (segment-item)
  ((%valtype   :accessor wgi-valtype
               :initform :i32
               :initarg  :valtype
               :documentation "Value type keyword: :i32, :i64, :f32, :f64, :v128.")
   (%mutable   :accessor wgi-mutable
               :initform nil
               :initarg  :mutable
               :documentation "T if the global is mutable, NIL if constant.")
   (%init-expr :accessor wgi-init-expr
               :initform nil
               :initarg  :init-expr
               :documentation "Initializer constant expression as a list of instruction forms,
e.g. ((:i32.const 42)).  Terminated by 0x0B (end) during serialization."))
  (:documentation
   "A WASM global variable declaration for the Global section.
Serialized as: valtype byte, mutability byte (0x00 or 0x01),
init-expr instruction bytes, 0x0B (end)."))

;; -------------------------------------------------------------
;; Data segment
;; -------------------------------------------------------------

(defclass wasm-data-item (segment-item)
  ((%data         :accessor wdi-data
                  :initform nil
                  :initarg  :data
                  :documentation "Byte vector of initialization data.")
   (%memory-index :accessor wdi-memory-index
                  :initform 0
                  :initarg  :memory-index
                  :documentation "Target memory index (usually 0).")
   (%offset-expr  :accessor wdi-offset-expr
                  :initform nil
                  :initarg  :offset-expr
                  :documentation "Offset constant expression for active segments,
e.g. ((:i32.const 0)).  NIL for passive data segments.")
   (%mode         :accessor wdi-mode
                  :initform :active
                  :initarg  :mode
                  :documentation "Data segment mode: :active or :passive.
Active segments are loaded into memory at instantiation time.
Passive segments are referenced via memory.init instructions."))
  (:documentation
   "A WASM data segment for the Data section.
Active segments: 0x00, offset-expr, byte-vector (for memory 0),
  or 0x02, memory-index, offset-expr, byte-vector.
Passive segments: 0x01, byte-vector."))

;; -------------------------------------------------------------
;; Element segment
;; -------------------------------------------------------------

(defclass wasm-element-item (segment-item)
  ((%table-index  :accessor weli-table-index
                  :initform 0
                  :initarg  :table-index
                  :documentation "Target table index (for active segments).")
   (%offset-expr  :accessor weli-offset-expr
                  :initform nil
                  :initarg  :offset-expr
                  :documentation "Offset constant expression for active segments.")
   (%func-indices :accessor weli-func-indices
                  :initform nil
                  :initarg  :func-indices
                  :documentation "List of function indices to place in the table.")
   (%mode         :accessor weli-mode
                  :initform :active
                  :initarg  :mode
                  :documentation "Element segment mode: :active, :passive, or :declarative."))
  (:documentation
   "A WASM element segment for the Element section.
Initializes a table region with function references."))

;; ===============================================================
;; LEB128 encoding utilities
;; ===============================================================

(defun encode-leb128-unsigned (value)
  "Encode a non-negative integer as an unsigned LEB128 byte vector."
  (declare (type (integer 0) value))
  (if (zerop value)
      (vector 0)
      (let ((bytes nil))
        (loop :while (plusp value)
              :do (let ((byte (logand value #x7F)))
                    (setf value (ash value -7))
                    (when (plusp value)
                      (setf byte (logior byte #x80)))
                    (push byte bytes)))
        (coerce (nreverse bytes) '(vector (unsigned-byte 8))))))

(defun encode-leb128-signed (value)
  "Encode an integer as a signed LEB128 byte vector."
  (let ((bytes nil)
        (more t))
    (loop :while more
          :do (let ((byte (logand value #x7F)))
                (setf value (ash value -7))
                (if (or (and (zerop value) (zerop (logand byte #x40)))
                        (and (= value -1)  (not (zerop (logand byte #x40)))))
                    (setf more nil)
                    (setf byte (logior byte #x80)))
                (push byte bytes)))
    (coerce (nreverse bytes) '(vector (unsigned-byte 8)))))

;; ===============================================================
;; Valtype encoding helper
;; ===============================================================

(defun wasm-valtype-byte (valtype)
  "Return the single-byte encoding for a WASM valtype keyword."
  (or (cdr (assoc valtype *wasm-valtypes*))
      (error "Unknown WASM value type: ~a" valtype)))

(defun wasm-ext-kind-byte (kind)
  "Return the byte encoding for a WASM external kind keyword."
  (or (cdr (assoc kind *wasm-ext-kinds*))
      (error "Unknown WASM external kind: ~a" kind)))

(defun wasm-section-id (kind)
  "Return the section ID byte for a WASM segment kind keyword."
  (or (cdr (assoc kind *wasm-section-ids*))
      (error "Unknown WASM section kind: ~a" kind)))

;; ===============================================================
;; Convenience constructors
;; ===============================================================

(defun make-wasm-program (&key assembler)
  "Create a program configured for WebAssembly output.
Sets up a single program-unit with standard WASM sections."
  (let* ((asm (or assembler (when (boundp '*assembler-prototype-wasm*)
                              (symbol-value (find-symbol "*ASSEMBLER-PROTOTYPE-WASM*"
                                                         (find-package :specops.wasm))))))
         (pgm  (make-program asm))
         (unit (make-unit "module" pgm)))
    ;; Create standard sections; only populated sections will be serialized
    (add-segment unit "type"     (make-segment "type"     :wasm-type))
    (add-segment unit "import"   (make-segment "import"   :wasm-import))
    (add-segment unit "function" (make-segment "function" :wasm-function))
    (add-segment unit "table"    (make-segment "table"    :wasm-table))
    (add-segment unit "memory"   (make-segment "memory"   :wasm-memory))
    (add-segment unit "global"   (make-segment "global"   :wasm-global))
    (add-segment unit "export"   (make-segment "export"   :wasm-export))
    (add-segment unit "code"     (make-segment "code"     :wasm-code))
    (add-segment unit "data"     (make-segment "data"     :wasm-data))
    (values pgm unit)))

(defun make-wasm-functype (params results)
  "Create a wasm-type-item representing a function signature.
PARAMS and RESULTS are lists of valtype keywords, e.g. (:i32 :i32) and (:i32)."
  (make-instance 'wasm-type-item :params params :results results))

(defun make-wasm-function (name type-index &key locals)
  "Create a wasm-function-item.
TYPE-INDEX references a type in the Type section.
LOCALS is a list of (count . valtype) pairs."
  (make-instance 'wasm-function-item
                 :name name :type-index type-index :locals locals))

(defun make-wasm-import (module-name field-name kind descriptor)
  "Create a wasm-import-item.
KIND is :func, :table, :memory, :global, or :tag.
DESCRIPTOR depends on kind (type-index for :func, limits for :memory, etc.)."
  (make-instance 'wasm-import-item :module-name module-name :field-name field-name
                                   :import-kind kind :descriptor descriptor))

(defun make-wasm-export (name kind index)
  "Create a wasm-export-item.
NAME is the export name string, KIND is :func/:table/:memory/:global,
INDEX is the index in the corresponding index space."
  (make-instance 'wasm-export-item
                 :export-name name :export-kind kind :index index))

(defun make-wasm-memory (min-pages &optional max-pages)
  "Create a wasm-memory-item with the given page limits."
  (make-instance 'wasm-memory-item :min-pages min-pages :max-pages max-pages))

(defun make-wasm-global (valtype init-expr &key mutable)
  "Create a wasm-global-item.
VALTYPE is a valtype keyword, INIT-EXPR is a list of instruction forms."
  (make-instance 'wasm-global-item
                 :valtype valtype :mutable mutable :init-expr init-expr))

(defun make-wasm-data (data &key (mode :active) (memory-index 0) offset-expr)
  "Create a wasm-data-item.
DATA is a byte vector.  For active segments, OFFSET-EXPR is required."
  (make-instance 'wasm-data-item
                 :data data :mode mode
                 :memory-index memory-index :offset-expr offset-expr))

(defun make-wasm-block (kind &key blocktype body else-body)
  "Create a wasm-block-item for structured control flow.
KIND is :block, :loop, or :if."
  (make-instance 'wasm-block-item :kind kind :blocktype blocktype
                                  :body body :else-body else-body))

;; ===============================================================
;; wasm-program macro — declarative WASM module builder
;; ===============================================================
;;
;; Syntax:
;;   (wasm-program
;;     (types
;;       ((:i32 :i32) -> (:i32))         ; functype 0
;;       (()          -> (:i32)))         ; functype 1
;;     (memory 1)                         ; 1 page min
;;     (memory 1 10)                      ; 1 page min, 10 max
;;     (import "env" "log" (:func 0))    ; import function with type 0
;;     (export "add" :func 0)            ; export function index 0
;;     (global :i32 42)                   ; immutable i32 global
;;     (global :i32 :var 0)              ; mutable i32 global
;;     (table :funcref 1)                ; table with min=1
;;     (data (:i32.const 0) #(1 2 3 4)) ; active data segment
;;     (func name (options...) body...)  ; function definition
;;   )
;;
;; Returns an unemitted program ready for (emit-program pgm :wasm stream).

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun %wasm-sym= (sym name)
    "Test symbol name match, case-insensitive, across packages."
    (and (symbolp sym) (string-equal (symbol-name sym) name)))

  (defun %wasm-expand-functype (form)
    "Expand a type spec like ((:i32 :i32) -> (:i32)) into a make-instance form."
    ;; Allow both ((:i32) -> (:i32)) and ((:i32) (:i32)) forms
    (let (params results)
      (if (and (>= (length form) 3)
               (let ((arrow (second form)))
                 (and (symbolp arrow) (string-equal (symbol-name arrow) "->"))))
          ;; Arrow syntax: (params -> results)
          (setf params (first form) results (third form))
          ;; Juxtaposed syntax: (params results)
          (setf params (first form) results (second form)))
      `(make-instance 'wasm-type-item :params ',params :results ',results)))

  (defun %wasm-expand-func-body-item (item)
    "Expand a WASM function body item into a form evaluating to a segment-item.
Handles instructions, block/loop/if structured control."
    (cond
      ;; (:keyword args...) — instruction
      ((and (listp item) (keywordp (car item)))
       `(make-instance 'instruction-item :expression ',item))

      ;; (block options body...) / (loop options body...) / (if options then [else-body])
      ((and (listp item) (symbolp (car item))
            (member (symbol-name (car item)) '("BLOCK" "LOOP" "IF")
                    :test #'string-equal))
       (let* ((kind-name (symbol-name (car item)))
              (kind-kw (cond ((string-equal kind-name "BLOCK") :block)
                             ((string-equal kind-name "LOOP")  :loop)
                             ((string-equal kind-name "IF")    :if)))
              (rest (cdr item))
              ;; First element might be a blocktype or the first body item
              ;; Blocktype: a valtype keyword like :i32, or an integer
              (blocktype (cond ((null rest) nil)
                               ((and (keywordp (car rest))
                                     (assoc (car rest) *wasm-valtypes*))
                                (prog1 (car rest) (setf rest (cdr rest))))
                               ((integerp (car rest))
                                (prog1 (car rest) (setf rest (cdr rest))))
                               (t nil))))
         ;; For :if, look for an (else ...) form
         (if (eq kind-kw :if)
             (let* ((else-pos (position-if
                               (lambda (x) (and (listp x) (%wasm-sym= (car x) "ELSE")))
                               rest))
                    (then-body (if else-pos (subseq rest 0 else-pos) rest))
                    (else-body (when else-pos (cdr (nth else-pos rest)))))
               `(make-instance 'wasm-block-item
                               :kind ,kind-kw
                               :blocktype ,blocktype
                               :body (list ,@(mapcar #'%wasm-expand-func-body-item then-body))
                               :else-body ,(when else-body
                                             `(list ,@(mapcar #'%wasm-expand-func-body-item
                                                              else-body)))))
             `(make-instance 'wasm-block-item
                             :kind ,kind-kw
                             :blocktype ,blocktype
                             :body (list ,@(mapcar #'%wasm-expand-func-body-item rest))))))

      (t (error "Unrecognized WASM function body item: ~s" item))))

  (defun %wasm-expand-func (form)
    "Expand (func name (options...) body...) into values for building the item.
Returns (values name type-index locals body-forms-list).
Options is a list of keyword pairs: ((:type N) (:locals ...))."
    ;; (func name (options...) body...)
    (destructuring-bind (name options &rest body) (cdr form)
      (let ((type-index 0) (locals nil))
        ;; Parse options as a list of (:key value) pairs
        (dolist (opt options)
          (when (listp opt)
            (cond ((and (keywordp (car opt)) (string-equal (symbol-name (car opt)) "TYPE"))
                   (setf type-index (second opt)))
                  ((and (keywordp (car opt)) (string-equal (symbol-name (car opt)) "LOCALS"))
                   ;; (:locals (count . valtype)...) — rest of form is the locals list
                   (setf locals (cdr opt))))))
        (values name type-index locals
                (mapcar #'%wasm-expand-func-body-item body)))))

  ) ; end eval-when

(defmacro wasm-program (&body body)
  "Build a WASM program model from a declarative specification.

Body forms (order-independent; sections are emitted in prescribed order):
  (types (params -> results)...)         — function type signatures
  (memory min [max])                     — linear memory declaration
  (table reftype min [max])              — table declaration
  (import mod field descriptor)          — import declaration
  (export name kind index)              — export declaration
  (global valtype [init | :var init])   — global variable
  (data (offset-expr) byte-vector)      — data segment (active)
  (data :passive byte-vector)           — data segment (passive)
  (elem (offset-expr) idx...)           — element segment
  (func name (options) body...)         — function definition

Function options:
  :type N         — type section index for this function's signature
  :locals ((count . valtype)...)  — local variable declarations

Function body items:
  (:mnemonic args...)                    — WASM instruction
  (block [blocktype] body...)           — block structured control
  (loop [blocktype] body...)            — loop structured control
  (if [blocktype] then... [(else ...)])  — if/else structured control

Returns a program instance ready for (emit-program pgm :wasm stream)."
  (let ((pgm-var (gensym "PGM"))
        (unit-var (gensym "UNIT"))
        ;; Classify body forms by section type
        (type-forms nil)
        (import-forms nil)
        (memory-forms nil)
        (table-forms nil)
        (global-forms nil)
        (export-forms nil)
        (data-forms nil)
        (elem-forms nil)
        (func-forms nil))

    ;; Classify
    (dolist (form body)
      (when (listp form)
        (let ((head (car form)))
          (cond ((%wasm-sym= head "TYPES")   (setf type-forms (cdr form)))
                ((%wasm-sym= head "MEMORY")  (push form memory-forms))
                ((%wasm-sym= head "TABLE")   (push form table-forms))
                ((%wasm-sym= head "IMPORT")  (push form import-forms))
                ((%wasm-sym= head "EXPORT")  (push form export-forms))
                ((%wasm-sym= head "GLOBAL")  (push form global-forms))
                ((%wasm-sym= head "DATA")    (push form data-forms))
                ((%wasm-sym= head "ELEM")    (push form elem-forms))
                ((%wasm-sym= head "FUNC")    (push form func-forms))
                (t (error "Unknown WASM section declarator: ~a" head))))))

    (setf import-forms (nreverse import-forms)
          memory-forms (nreverse memory-forms)
          table-forms  (nreverse table-forms)
          global-forms (nreverse global-forms)
          export-forms (nreverse export-forms)
          data-forms   (nreverse data-forms)
          elem-forms   (nreverse elem-forms)
          func-forms   (nreverse func-forms))

    `(let* ((,pgm-var (make-instance 'program))
            (,unit-var (make-unit "module" ,pgm-var)))

       ;; -- Type section --------------------------------------
       ,@(when type-forms
           (let ((seg-var (gensym "TSEG")))
             `((let ((,seg-var (make-segment "type" :wasm-type)))
                 (add-segment ,unit-var "type" ,seg-var)
                 ,@(loop :for tf :in type-forms
                         :collect `(add-item ,seg-var ,(%wasm-expand-functype tf)))
                 (finalize-segment ,seg-var)))))

       ;; -- Import section ------------------------------------
       ,@(when import-forms
           (let ((seg-var (gensym "ISEG")))
             `((let ((,seg-var (make-segment "import" :wasm-import)))
                 (add-segment ,unit-var "import" ,seg-var)
                 ,@(loop :for form :in import-forms
                         :collect (destructuring-bind (mod-name field-name desc) (cdr form)
                                    (let ((kind (if (listp desc) (first desc) desc))
                                          (descriptor (if (listp desc) (second desc) nil)))
                                      `(add-item ,seg-var
                                                 (make-instance 'wasm-import-item
                                                                :module-name ,mod-name
                                                                :field-name ,field-name
                                                                :import-kind ,kind
                                                                :descriptor ,descriptor)))))
                 (finalize-segment ,seg-var)))))

       ;; -- Table section -------------------------------------
       ,@(when table-forms
           (let ((seg-var (gensym "TBSEG")))
             `((let ((,seg-var (make-segment "table" :wasm-table)))
                 (add-segment ,unit-var "table" ,seg-var)
                 ,@(loop :for form :in table-forms
                         :collect (destructuring-bind (reftype min &optional max) (cdr form)
                                    `(add-item ,seg-var
                                               (make-instance 'wasm-table-item
                                                              :reftype ,reftype
                                                              :min-size ,min
                                                              :max-size ,max))))
                 (finalize-segment ,seg-var)))))

       ;; -- Memory section ------------------------------------
       ,@(when memory-forms
           (let ((seg-var (gensym "MSEG")))
             `((let ((,seg-var (make-segment "memory" :wasm-memory)))
                 (add-segment ,unit-var "memory" ,seg-var)
                 ,@(loop :for form :in memory-forms
                         :collect (destructuring-bind (min &optional max) (cdr form)
                                    `(add-item ,seg-var
                                               (make-instance 'wasm-memory-item
                                                              :min-pages ,min
                                                              :max-pages ,max))))
                 (finalize-segment ,seg-var)))))

       ;; -- Global section ------------------------------------
       ,@(when global-forms
           (let ((seg-var (gensym "GSEG")))
             `((let ((,seg-var (make-segment "global" :wasm-global)))
                 (add-segment ,unit-var "global" ,seg-var)
                 ,@(loop :for form :in global-forms
                         :collect
                         ;; (global :i32 42) or (global :i32 :var 0)
                         (let* ((args (cdr form))
                                (valtype (first args))
                                (mutable (and (keywordp (second args))
                                              (string-equal (symbol-name (second args)) "VAR")))
                                (init-val (if mutable (third args) (second args))))
                           `(add-item ,seg-var
                                      (make-instance 'wasm-global-item
                                                     :valtype ,valtype
                                                     :mutable ,(if mutable t nil)
                                                     :init-expr ',(list
                                                                   (list
                                                                    (intern
                                                                     (format nil "~a.CONST"
                                                                             (string valtype))
                                                                     "KEYWORD")
                                                                    init-val))))))
                 (finalize-segment ,seg-var)))))

       ;; -- Export section ------------------------------------
       ,@(when export-forms
           (let ((seg-var (gensym "ESEG")))
             `((let ((,seg-var (make-segment "export" :wasm-export)))
                 (add-segment ,unit-var "export" ,seg-var)
                 ,@(loop :for form :in export-forms
                         :collect (destructuring-bind (name kind index) (cdr form)
                                    `(add-item ,seg-var
                                               (make-instance 'wasm-export-item
                                                              :export-name ,name
                                                              :export-kind ,kind
                                                              :index ,index))))
                 (finalize-segment ,seg-var)))))

       ;; -- Data section --------------------------------------
       ,@(when data-forms
           (let ((seg-var (gensym "DSEG")))
             `((let ((,seg-var (make-segment "data" :wasm-data)))
                 (add-segment ,unit-var "data" ,seg-var)
                 ,@(loop :for form :in data-forms
                         :collect
                         ;; (data (offset-expr) bytes) or (data :passive bytes)
                         (let ((args (cdr form)))
                           (if (and (keywordp (first args))
                                    (string-equal (symbol-name (first args)) "PASSIVE"))
                               `(add-item ,seg-var
                                          (make-instance 'wasm-data-item
                                                         :mode :passive
                                                         :data ,(second args)))
                               `(add-item ,seg-var
                                          (make-instance 'wasm-data-item
                                                         :mode :active
                                                         :offset-expr ',(first args)
                                                         :data ,(second args))))))
                 (finalize-segment ,seg-var)))))

       ;; -- Element section -----------------------------------
       ,@(when elem-forms
           (let ((seg-var (gensym "ELSEG")))
             `((let ((,seg-var (make-segment "element" :wasm-element)))
                 (add-segment ,unit-var "element" ,seg-var)
                 ,@(loop :for form :in elem-forms
                         :collect
                         (destructuring-bind (offset-expr &rest indices) (cdr form)
                           `(add-item ,seg-var
                                      (make-instance 'wasm-element-item
                                                     :offset-expr ',offset-expr
                                                     :func-indices ',indices))))
                 (finalize-segment ,seg-var)))))

       ;; -- Code section (func declarations) ------------------
       ,@(when func-forms
           (let ((seg-var (gensym "CSEG")))
             `((let ((,seg-var (make-segment "code" :wasm-code)))
                 (add-segment ,unit-var "code" ,seg-var)
                 ,@(loop :for form :in func-forms
                         :collect
                         (multiple-value-bind (name type-index locals body-forms)
                             (%wasm-expand-func form)
                           `(let ((func (make-instance 'wasm-function-item
                                                       :name ',name
                                                       :type-index ,type-index
                                                       :locals ',locals)))
                              (setf (fi-body func)
                                    (list ,@body-forms))
                              (add-item ,seg-var func))))
                 (finalize-segment ,seg-var)))))

       ,pgm-var)))

;; ===============================================================
;; IEEE 754 float encoding helpers
;; ===============================================================

#+sbcl
(defun single-float-bits (f)
  "Return the IEEE 754 bit pattern of a single-float as an integer."
  (sb-kernel:single-float-bits f))

#+sbcl
(defun double-float-bits (f)
  "Return the IEEE 754 bit pattern of a double-float as an integer."
  (let ((hi (sb-kernel:double-float-high-bits f))
        (lo (sb-kernel:double-float-low-bits f)))
    (logior (ash hi 32) lo)))

#-sbcl
(defun single-float-bits (f)
  "Portable fallback: encode single-float to IEEE 754 bits."
  (declare (ignore f))
  (error "single-float-bits not implemented for this CL implementation"))

#-sbcl
(defun double-float-bits (f)
  "Portable fallback: encode double-float to IEEE 754 bits."
  (declare (ignore f))
  (error "double-float-bits not implemented for this CL implementation"))

;; ===============================================================
;; WASM binary emitter
;; ===============================================================
;;
;; Writes a complete .wasm binary module from an assembled program.

;; -- Byte accumulator -----------------------------------------

(defun %wasm-make-buffer ()
  "Create an adjustable byte buffer."
  (make-array 64 :element-type '(unsigned-byte 8)
                 :adjustable t :fill-pointer 0))

(defun %wasm-emit-byte (buf byte)
  (unless (< (fill-pointer buf) (1- (array-total-size buf)))
    (adjust-array buf (* 2 (array-total-size buf))))
  (vector-push byte buf))

(defun %wasm-emit-bytes (buf bytes)
  (loop :for b :across bytes :do (%wasm-emit-byte buf b)))

(defun %wasm-emit-leb128-u (buf value)
  (%wasm-emit-bytes buf (encode-leb128-unsigned value)))

(defun %wasm-emit-leb128-s (buf value)
  (%wasm-emit-bytes buf (encode-leb128-signed value)))

(defun %wasm-emit-name (buf string)
  "Emit a WASM name (LEB128 length + UTF-8 bytes)."
  (let ((encoded (map '(vector (unsigned-byte 8)) #'char-code string)))
    (%wasm-emit-leb128-u buf (length encoded))
    (%wasm-emit-bytes buf encoded)))

(defun %wasm-emit-valtype (buf valtype)
  (%wasm-emit-byte buf (wasm-valtype-byte valtype)))

(defun %wasm-emit-blocktype (buf blocktype)
  "Emit a block type: NIL→void, keyword→valtype, integer→type index."
  (cond
    ((null blocktype)    (%wasm-emit-byte buf +wasm-blocktype-void+))
    ((keywordp blocktype) (%wasm-emit-valtype buf blocktype))
    ((integerp blocktype) (%wasm-emit-leb128-s buf blocktype))
    (t (error "Invalid block type: ~a" blocktype))))

(defun %wasm-emit-limits (buf min-val &optional max-val)
  "Emit a limits encoding."
  (if max-val (progn (%wasm-emit-byte buf #x01)
                     (%wasm-emit-leb128-u buf min-val)
                     (%wasm-emit-leb128-u buf max-val))
      (progn (%wasm-emit-byte buf #x00)
             (%wasm-emit-leb128-u buf min-val))))

;; -- Section body encoders ------------------------------------

(defun %wasm-encode-type-section (items)
  "Encode the Type section body from a list of wasm-type-items."
  (let ((buf (%wasm-make-buffer)))
    (%wasm-emit-leb128-u buf (length items))
    (dolist (item items)
      (%wasm-emit-byte buf +wasm-functype-tag+)
      ;; params
      (%wasm-emit-leb128-u buf (length (wti-params item)))
      (dolist (p (wti-params item)) (%wasm-emit-valtype buf p))
      ;; results
      (%wasm-emit-leb128-u buf (length (wti-results item)))
      (dolist (r (wti-results item)) (%wasm-emit-valtype buf r)))
    buf))

(defun %wasm-encode-import-section (items)
  "Encode the Import section body."
  (let ((buf (%wasm-make-buffer)))
    (%wasm-emit-leb128-u buf (length items))
    (dolist (item items)
      (%wasm-emit-name buf (wii-module-name item))
      (%wasm-emit-name buf (wii-field-name item))
      (%wasm-emit-byte buf (wasm-ext-kind-byte (wii-import-kind item)))
      (let ((desc (wii-descriptor item)))
        (ecase (wii-import-kind item)
          (:func   (%wasm-emit-leb128-u buf desc))
          (:table  (destructuring-bind (reftype min &optional max) desc
                     (%wasm-emit-valtype buf reftype)
                     (%wasm-emit-limits buf min max)))
          (:memory (if (listp desc)
                       (%wasm-emit-limits buf (first desc) (second desc))
                       (%wasm-emit-limits buf desc)))
          (:global (destructuring-bind (valtype mutability) desc
                     (%wasm-emit-valtype buf valtype)
                     (%wasm-emit-byte buf (if (eq mutability :var) 1 0)))))))
    buf))

(defun %wasm-encode-function-section (func-items)
  "Encode the Function section body (type indices for each function)."
  (let ((buf (%wasm-make-buffer)))
    (%wasm-emit-leb128-u buf (length func-items))
    (dolist (f func-items)
      (%wasm-emit-leb128-u buf (wfi-type-index f)))
    buf))

(defun %wasm-encode-table-section (items)
  "Encode the Table section body."
  (let ((buf (%wasm-make-buffer)))
    (%wasm-emit-leb128-u buf (length items))
    (dolist (item items)
      (%wasm-emit-valtype buf (wtbi-reftype item))
      (%wasm-emit-limits buf (wtbi-min-size item) (wtbi-max-size item)))
    buf))

(defun %wasm-encode-memory-section (items)
  "Encode the Memory section body."
  (let ((buf (%wasm-make-buffer)))
    (%wasm-emit-leb128-u buf (length items))
    (dolist (item items)
      (%wasm-emit-limits buf (wmi-min-pages item) (wmi-max-pages item)))
    buf))

(defun %wasm-encode-global-section (items)
  "Encode the Global section body."
  (let ((buf (%wasm-make-buffer)))
    (%wasm-emit-leb128-u buf (length items))
    (dolist (item items)
      (%wasm-emit-valtype buf (wgi-valtype item))
      (%wasm-emit-byte buf (if (wgi-mutable item) 1 0))
      ;; init expression
      (%wasm-encode-expr-into buf (wgi-init-expr item))
      (%wasm-emit-byte buf +wasm-op-end+))
    buf))

(defun %wasm-encode-export-section (items)
  "Encode the Export section body."
  (let ((buf (%wasm-make-buffer)))
    (%wasm-emit-leb128-u buf (length items))
    (dolist (item items)
      (%wasm-emit-name buf (wei-export-name item))
      (%wasm-emit-byte buf (wasm-ext-kind-byte (wei-export-kind item)))
      (%wasm-emit-leb128-u buf (wei-index item)))
    buf))

(defun %wasm-encode-data-section (items)
  "Encode the Data section body."
  (let ((buf (%wasm-make-buffer)))
    (%wasm-emit-leb128-u buf (length items))
    (dolist (item items)
      (ecase (wdi-mode item)
        (:active
         (if (zerop (wdi-memory-index item))
             (progn
               (%wasm-emit-byte buf 0)  ; variant 0: active, memory 0
               (%wasm-encode-expr-into buf (wdi-offset-expr item))
               (%wasm-emit-byte buf +wasm-op-end+))
             (progn
               (%wasm-emit-byte buf 2)  ; variant 2: active, explicit memory
               (%wasm-emit-leb128-u buf (wdi-memory-index item))
               (%wasm-encode-expr-into buf (wdi-offset-expr item))
               (%wasm-emit-byte buf +wasm-op-end+))))
        (:passive
         (%wasm-emit-byte buf 1)))
      ;; byte vector
      (%wasm-emit-leb128-u buf (length (wdi-data item)))
      (%wasm-emit-bytes buf (wdi-data item)))
    buf))

(defun %wasm-encode-element-section (items)
  "Encode the Element section body (variant 0 only: active, table 0, funcidx)."
  (let ((buf (%wasm-make-buffer)))
    (%wasm-emit-leb128-u buf (length items))
    (dolist (item items)
      ;; Simplified: only variant 0 (active, table 0, funcidx list)
      (%wasm-emit-byte buf 0)
      (%wasm-encode-expr-into buf (weli-offset-expr item))
      (%wasm-emit-byte buf +wasm-op-end+)
      (%wasm-emit-leb128-u buf (length (weli-func-indices item)))
      (dolist (idx (weli-func-indices item))
        (%wasm-emit-leb128-u buf idx)))
    buf))

;; -- Instruction encoding -------------------------------------

(defun %wasm-encode-expr-into (buf expr-items)
  "Encode a list of instruction forms into BUF.
Each item is either an instruction-item or a list like (:i32.const 42)."
  (dolist (item expr-items)
    (let ((form (if (typep item 'instruction-item)
                    (ii-expression item)
                    item)))
      (%wasm-encode-instruction buf form))))

(defun %wasm-instruction-opcode (mnemonic)
  "Look up the single-byte opcode for a WASM instruction mnemonic."
  (or (cdr (assoc mnemonic
                  '((:unreachable . #x00) (:nop . #x01)
                    (:block . #x02) (:loop . #x03) (:if . #x04)
                    (:else . #x05) (:end . #x0B)
                    (:br . #x0C) (:br_if . #x0D) (:br_table . #x0E)
                    (:return . #x0F) (:call . #x10) (:call_indirect . #x11)
                    (:drop . #x1A) (:select . #x1B)
                    (:local.get . #x20) (:local.set . #x21) (:local.tee . #x22)
                    (:global.get . #x23) (:global.set . #x24)
                    (:i32.load . #x28) (:i64.load . #x29)
                    (:f32.load . #x2A) (:f64.load . #x2B)
                    (:i32.store . #x36) (:i64.store . #x37)
                    (:f32.store . #x38) (:f64.store . #x39)
                    (:memory.size . #x3F) (:memory.grow . #x40)
                    (:i32.const . #x41) (:i64.const . #x42)
                    (:f32.const . #x43) (:f64.const . #x44)
                    (:i32.eqz . #x45) (:i32.eq . #x46) (:i32.ne . #x47)
                    (:i32.lt_s . #x48) (:i32.lt_u . #x49)
                    (:i32.gt_s . #x4A) (:i32.gt_u . #x4B)
                    (:i32.le_s . #x4C) (:i32.le_u . #x4D)
                    (:i32.ge_s . #x4E) (:i32.ge_u . #x4F)
                    (:i64.eqz . #x50)
                    (:i32.add . #x6A) (:i32.sub . #x6B)
                    (:i32.mul . #x6C) (:i32.div_s . #x6D) (:i32.div_u . #x6E)
                    (:i32.rem_s . #x6F) (:i32.rem_u . #x70)
                    (:i32.and . #x71) (:i32.or . #x72) (:i32.xor . #x73)
                    (:i32.shl . #x74) (:i32.shr_s . #x75) (:i32.shr_u . #x76)
                    (:i64.add . #x7C) (:i64.sub . #x7D)
                    (:i64.mul . #x7E))))
      (error "Unknown WASM instruction: ~a" mnemonic)))

(defun %wasm-encode-instruction (buf form)
  "Encode a single WASM instruction form into BUF."
  (let ((mnemonic (first form))
        (args (rest form)))
    (let ((opcode (%wasm-instruction-opcode mnemonic)))
      (%wasm-emit-byte buf opcode)
      ;; Encode immediates based on instruction type
      (case mnemonic
        ;; Instructions with u32 immediate
        ((:local.get :local.set :local.tee :global.get :global.set
          :br :br_if :call)
         (%wasm-emit-leb128-u buf (first args)))
        ;; i32.const: signed LEB128
        (:i32.const (%wasm-emit-leb128-s buf (first args)))
        ;; i64.const: signed LEB128
        (:i64.const (%wasm-emit-leb128-s buf (first args)))
        ;; f32.const: 4 bytes IEEE 754 little-endian
        (:f32.const
         (let ((bits (single-float-bits (coerce (first args) 'single-float))))
           (dotimes (i 4)
             (%wasm-emit-byte buf (logand #xFF (ash bits (* -8 i)))))))
        ;; f64.const: 8 bytes IEEE 754 little-endian
        (:f64.const
         (let ((bits (double-float-bits (coerce (first args) 'double-float))))
           (dotimes (i 8)
             (%wasm-emit-byte buf (logand #xFF (ash bits (* -8 i)))))))
        ;; Memory instructions: memarg (align + offset)
        ((:i32.load :i64.load :f32.load :f64.load
          :i32.store :i64.store :f32.store :f64.store)
         (let ((align (or (first args) 0))
               (offset (or (second args) 0)))
           (%wasm-emit-leb128-u buf align)
           (%wasm-emit-leb128-u buf offset)))
        ;; memory.size, memory.grow: memory index (0)
        ((:memory.size :memory.grow)
         (%wasm-emit-byte buf 0))
        ;; call_indirect: typeidx + tableidx
        (:call_indirect
         (%wasm-emit-leb128-u buf (first args))
         (%wasm-emit-leb128-u buf (or (second args) 0)))
        ;; br_table: vec(labelidx) + default labelidx
        (:br_table
         (let ((labels (first args))
               (default (second args)))
           (%wasm-emit-leb128-u buf (length labels))
           (dolist (l labels) (%wasm-emit-leb128-u buf l))
           (%wasm-emit-leb128-u buf default)))
        ;; No immediates for everything else
        (otherwise nil)))))

(defun %wasm-encode-block-item (buf item)
  "Encode a wasm-block-item into BUF."
  (%wasm-emit-byte buf (ecase (wbi-kind item)
                         (:block +wasm-op-block+)
                         (:loop  +wasm-op-loop+)
                         (:if    +wasm-op-if+)))
  (%wasm-emit-blocktype buf (wbi-blocktype item))
  ;; Body
  (%wasm-encode-body-items buf (wbi-body item))
  ;; Else branch (for :if blocks)
  (when (and (eq (wbi-kind item) :if) (wbi-else-body item))
    (%wasm-emit-byte buf +wasm-op-else+)
    (%wasm-encode-body-items buf (wbi-else-body item)))
  ;; End
  (%wasm-emit-byte buf +wasm-op-end+))

(defun %wasm-encode-body-items (buf items)
  "Encode a list of segment-items (instructions and blocks) into BUF."
  (dolist (item items)
    (typecase item
      (wasm-block-item (%wasm-encode-block-item buf item))
      (instruction-item (%wasm-encode-instruction buf (ii-expression item)))
      ;; Plain lists (from init-expr etc.)
      (list (%wasm-encode-instruction buf item)))))

;; -- Code section encoder -------------------------------------

(defun %wasm-encode-code-section (func-items)
  "Encode the Code section body from a list of wasm-function-items."
  (let ((buf (%wasm-make-buffer)))
    (%wasm-emit-leb128-u buf (length func-items))
    (dolist (func func-items)
      ;; Encode function body into a temporary buffer
      (let ((body-buf (%wasm-make-buffer)))
        ;; Local declarations
        (let ((locals (wfi-locals func)))
          (%wasm-emit-leb128-u body-buf (length locals))
          (dolist (local-group locals)
            (destructuring-bind (count . valtype) local-group
              (%wasm-emit-leb128-u body-buf count)
              (%wasm-emit-valtype body-buf valtype))))
        ;; Instructions from function body
        (%wasm-encode-body-items body-buf (fi-body func))
        ;; End opcode
        (%wasm-emit-byte body-buf +wasm-op-end+)
        ;; Emit size-prefixed body into main buffer
        (%wasm-emit-leb128-u buf (fill-pointer body-buf))
        (%wasm-emit-bytes buf body-buf)))
    buf))

;; -- Section writer -------------------------------------------

(defun %wasm-write-section (stream section-id body-buf)
  "Write a WASM section: ID byte + LEB128 size + body bytes."
  (write-byte section-id stream)
  (let ((size-bytes (encode-leb128-unsigned (fill-pointer body-buf))))
    (loop :for b :across size-bytes :do (write-byte b stream)))
  (loop :for i :below (fill-pointer body-buf)
        :do (write-byte (aref body-buf i) stream)))

;; -- Gather items from segments -------------------------------

(defun %wasm-gather-items (unit kind item-type)
  "Collect all items of ITEM-TYPE from the segment of KIND in UNIT."
  (let ((seg (loop :for (name . s) :in (pun-segments unit)
                   :when (eq (seg-kind s) kind) :return s)))
    (when seg
      (loop :for item :in (seg-items seg)
            :when (typep item item-type) :collect item))))

;; ===============================================================
;; emit-program :wasm — WASM binary module emitter
;; ===============================================================

(defmethod emit-program ((pgm program) (format (eql :wasm)) stream &key)
  "Write a WebAssembly binary module to STREAM.
The program should have WASM-specific sections populated with
the appropriate item types.

Sections are written in the prescribed order; empty sections are skipped."
  (let ((unit (first (pgm-units pgm))))
    ;; Magic + version
    (loop :for b :across *wasm-magic* :do (write-byte b stream))
    (loop :for b :across *wasm-version* :do (write-byte b stream))

    ;; Type section
    (let ((types (%wasm-gather-items unit :wasm-type 'wasm-type-item)))
      (when types
        (%wasm-write-section stream +wasm-section-type+
                             (%wasm-encode-type-section types))))

    ;; Import section
    (let ((imports (%wasm-gather-items unit :wasm-import 'wasm-import-item)))
      (when imports
        (%wasm-write-section stream +wasm-section-import+
                             (%wasm-encode-import-section imports))))

    ;; Function section (type indices from code segment's wasm-function-items)
    (let ((funcs (%wasm-gather-items unit :wasm-code 'wasm-function-item)))
      (when funcs
        (%wasm-write-section stream +wasm-section-function+
                             (%wasm-encode-function-section funcs))))

    ;; Table section
    (let ((tables (%wasm-gather-items unit :wasm-table 'wasm-table-item)))
      (when tables
        (%wasm-write-section stream +wasm-section-table+
                             (%wasm-encode-table-section tables))))

    ;; Memory section
    (let ((mems (%wasm-gather-items unit :wasm-memory 'wasm-memory-item)))
      (when mems
        (%wasm-write-section stream +wasm-section-memory+
                             (%wasm-encode-memory-section mems))))

    ;; Global section
    (let ((globals (%wasm-gather-items unit :wasm-global 'wasm-global-item)))
      (when globals
        (%wasm-write-section stream +wasm-section-global+
                             (%wasm-encode-global-section globals))))

    ;; Export section
    (let ((exports (%wasm-gather-items unit :wasm-export 'wasm-export-item)))
      (when exports
        (%wasm-write-section stream +wasm-section-export+
                             (%wasm-encode-export-section exports))))

    ;; Element section
    (let ((elems (%wasm-gather-items unit :wasm-element 'wasm-element-item)))
      (when elems
        (%wasm-write-section stream +wasm-section-element+
                             (%wasm-encode-element-section elems))))

    ;; Code section
    (let ((funcs (%wasm-gather-items unit :wasm-code 'wasm-function-item)))
      (when funcs
        (%wasm-write-section stream +wasm-section-code+
                             (%wasm-encode-code-section funcs))))

    ;; Data section
    (let ((data (%wasm-gather-items unit :wasm-data 'wasm-data-item)))
      (when data
        (%wasm-write-section stream +wasm-section-data+
                             (%wasm-encode-data-section data))))

    pgm))

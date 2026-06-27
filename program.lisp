;;;; program.lisp — Program model for SpecOps
;;;
;;; A program is a structured representation of an assembly-language
;;; program that can be serialized to multiple object formats
;;; (ELF, GOFF, WebAssembly binary).
;;;
;;; The hierarchy is:
;;;   program → program-unit → segment → segment-item
;;;
;;; A program contains one or more program-units (one for ELF/WASM,
;;; potentially several for GOFF where each unit maps to an SD).
;;; Each unit groups segments (code, data, etc.) together with a
;;; symbol table and a relocation list.

(in-package #:specops)

;; ===============================================================
;; Program — top-level container
;; ===============================================================

(defclass program ()
  ((%assembler   :accessor pgm-assembler
                 :initform nil
                 :initarg  :assembler
                 :documentation "The assembler prototype used by this program.")
   (%units       :accessor pgm-units
                 :initform nil
                 :initarg  :units
                 :documentation "Ordered list of program-unit instances.
For GOFF each unit maps to an SD; for ELF/WASM there is typically one.")
   (%entry-point :accessor pgm-entry-point
                 :initform nil
                 :initarg  :entry-point
                 :documentation "Entry-point symbol-entry, or NIL.
ELF: the address placed in e_entry.
GOFF: referenced in the END record.")
   (%properties  :accessor pgm-properties
                 :initform nil
                 :initarg  :properties
                 :documentation "Plist for format-level metadata."))
  (:documentation
   "Top-level program container.  Holds one or more program-units,
each of which groups segments and symbols.  A program with a single
unit is the common case for ELF and WASM; GOFF compilation units
may contain multiple units (section definitions)."))

;; ===============================================================
;; Program-unit — independently-linkable grouping of segments
;; ===============================================================

(defclass program-unit ()
  ((%name        :accessor pun-name
                 :initform nil
                 :initarg  :name
                 :documentation "Unit name (becomes the SD name in GOFF,
or the compilation-unit identifier in ELF).")
   (%program     :accessor pun-program
                 :initform nil
                 :initarg  :program
                 :documentation "Back-reference to the owning program.")
   (%segments    :accessor pun-segments
                 :initform nil
                 :initarg  :segments
                 :documentation "Ordered alist of (name . segment).")
   (%symbols     :accessor pun-symbols
                 :initform (make-hash-table :test #'eq)
                 :initarg  :symbols
                 :documentation "Hash table of symbol-entry instances, keyed by name.")
   (%relocations :accessor pun-relocations
                 :initform nil
                 :initarg  :relocations
                 :documentation "List of relocation instances.
Populated during assembly from label-ref-items.
GOFF: serialized as RLD records.  ELF: serialized as .rela sections.
WASM: unused (all references are index-based).")
   (%properties  :accessor pun-properties
                 :initform nil
                 :initarg  :properties
                 :documentation "Plist for unit-level metadata.
GOFF: AMODE/RMODE defaults for the SD."))
  (:documentation
   "A program unit groups segments and their associated symbols.
Maps to a GOFF Section Definition (SD), or to the sole unit in
an ELF object or WASM module.  Multiple units in one program
represent separately-bindable program objects within a single
compilation unit."))

;; ===============================================================
;; Segment — contiguous region of code or data
;; ===============================================================

(defclass segment ()
  ((%name       :accessor seg-name
                :initform nil
                :initarg  :name
                :documentation "Segment name string (e.g. \".text\", \"C_CODE64\").")
   (%kind       :accessor seg-kind
                :initform nil
                :initarg  :kind
                :documentation "Content kind keyword:
  :code, :data, :rodata, :bss for traditional architectures.
Architecture-specific modules may define additional kinds.")
   (%items      :accessor seg-items
                :initform nil
                :documentation "Accumulated segment-items (reversed during building,
finalized in order before serialization).")
   (%origin     :accessor seg-origin
                :initform 0
                :initarg  :origin
                :documentation "Load address (ELF/GOFF) or section offset.
Typically filled during layout, not at construction time.")
   (%size       :accessor seg-size
                :initform 0
                :initarg  :size
                :documentation "Byte size of this segment's assembled content.
For :bss segments, this is the reservation size with no backing items.
Filled during assembly.")
   (%align      :accessor seg-align
                :initform 1
                :initarg  :align
                :documentation "Required alignment in bytes (must be a power of 2).
ELF: sh_addralign.  GOFF: alignment field in ED behavioral attributes.")
   (%flags      :accessor seg-flags
                :initform '(:read)
                :initarg  :flags
                :documentation "Permission/attribute flags, subset of
  (:read :write :execute :alloc).
ELF: maps to SHF_ALLOC, SHF_WRITE, SHF_EXECINSTR.
GOFF: maps to ED behavioral attributes (executable, read-only).")
   (%bytes      :accessor seg-bytes
                :initform nil
                :documentation "Assembled byte vector, filled by build-program.
NIL before assembly.  For :bss segments, remains NIL (size only).")
   (%properties :accessor seg-properties
                :initform nil
                :initarg  :properties
                :documentation "Plist for format-specific section attributes.
GOFF: text-style, binding-algorithm, tasking-behavior, loading-behavior.
ELF: sh-type override, sh-link, sh-info, sh-entsize."))
  (:documentation
   "A segment represents a contiguous region within a program unit.
Maps to a GOFF Element Definition (ED), an ELF section (.text,
.data, .bss, .rodata, etc.), or a section in a WASM module.
Contains an ordered sequence of segment-items."))

;; ===============================================================
;; Symbol-entry — named symbol in the program's symbol table
;; ===============================================================

(defclass symbol-entry ()
  ((%name       :accessor sym-name
                :initform nil
                :initarg  :name
                :documentation "Symbol name (a string or interned symbol).")
   (%segment    :accessor sym-segment
                :initform nil
                :initarg  :segment
                :documentation "Segment this symbol is defined in, or NIL for external/undefined.
GOFF ER: NIL.  ELF SHN_UNDEF: NIL.")
   (%offset     :accessor sym-offset
                :initform 0
                :initarg  :offset
                :documentation "Byte offset within the segment.
GOFF LD: offset within parent ED.  ELF: st_value.
Filled during assembly.")
   (%binding    :accessor sym-binding
                :initform :local
                :initarg  :binding
                :documentation "Binding strength: :local, :global, or :weak.
ELF: STB_LOCAL/GLOBAL/WEAK.
GOFF: binding-strength in behavioral attributes.")
   (%sym-type   :accessor sym-type
                :initform :notype
                :initarg  :sym-type
                :documentation "Symbol type: :notype, :function, :object, :section.
ELF: STT_NOTYPE/FUNC/OBJECT/SECTION.
GOFF: LD vs PR distinction (LD for :function, PR for :object).")
   (%size       :accessor sym-size
                :initform 0
                :initarg  :size
                :documentation "Byte size of the entity this symbol names (0 if unknown).
ELF: st_size.  GOFF: length field of LD/PR.")
   (%properties :accessor sym-properties
                :initform nil
                :initarg  :properties
                :documentation "Plist for format-specific symbol attributes.
GOFF: amode, rmode, linkage-type, binding-scope, namespace-id.
ELF: visibility (:default, :hidden, :protected).
WASM: export-name, import-module, import-field."))
  (:documentation
   "A symbol table entry.  Represents a named location or external reference.
GOFF: maps to an LD (label) or PR (part) ESD record, or an ER (external ref).
ELF: maps to an Elf64_Sym entry.
WASM: maps to an import or export entry."))

;; ===============================================================
;; Relocation — a fixup to be applied during linking
;; ===============================================================

(defclass relocation ()
  ((%symbol     :accessor rel-symbol
                :initform nil
                :initarg  :symbol
                :documentation "The symbol-entry being referenced (relocation target).")
   (%segment    :accessor rel-segment
                :initform nil
                :initarg  :segment
                :documentation "The segment containing the site to be patched.")
   (%offset     :accessor rel-offset
                :initform 0
                :initarg  :offset
                :documentation "Byte offset of the fixup site within the segment.
GOFF RLD: offset field.
ELF RELA: r_offset (from section start in relocatable objects).")
   (%rel-type   :accessor rel-type
                :initform nil
                :initarg  :rel-type
                :documentation "Architecture-specific relocation type keyword.
s390x: :r-390-pc32dbl, :r-390-32, :r-390-64, etc.
x86-64: :r-x86-64-pc32, :r-x86-64-64, :r-x86-64-plt32, etc.
GOFF: :r-address, :r-offset, :r-relative-immediate, etc.")
   (%width      :accessor rel-width
                :initform 4
                :initarg  :width
                :documentation "Byte width of the field to be patched.
GOFF RLD: target-length byte.
ELF: implied by the relocation type.")
   (%addend     :accessor rel-addend
                :initform 0
                :initarg  :addend
                :documentation "Constant addend.
ELF RELA: r_addend.  GOFF: implicit in the RLD action/flags."))
  (:documentation
   "A relocation entry recording a fixup to be resolved during linking.
GOFF: maps to an RLD data item.
ELF: maps to an Elf64_Rela entry.
WASM does not use relocations (all references are index-based)."))

;; ===============================================================
;; Segment items — the contents of segments
;; ===============================================================

(defclass segment-item ()
  ()
  (:documentation "Abstract base class for items within a segment."))

;; ===============================================================
;; Raw bytes — pre-encoded data injected into the segment
;; ===============================================================

(defclass raw-bytes-item (segment-item)
  ((%data :accessor rbi-data
          :initform nil
          :initarg  :data
          :documentation "A (vector (unsigned-byte 8)) of literal bytes."))
  (:documentation "A run of pre-encoded bytes injected directly into the segment."))

;; ===============================================================
;; Instruction — an assembler instruction expression
;; ===============================================================

(defclass instruction-item (segment-item)
  ((%expression :accessor ii-expression
                :initform nil
                :initarg  :expression
                :documentation "An assembler instruction form, e.g. (:lhi 1 10).
Encoded into bytes during assembly via the assembler's lexicon."))
  (:documentation
   "A single assembler instruction.  The expression is resolved
against the assembler's lexicon during the assembly pass."))

;; ===============================================================
;; Label definition — marks a named position in a segment
;; ===============================================================

(defclass label-def-item (segment-item)
  ((%name    :accessor ldi-name
             :initform nil
             :initarg  :name
             :documentation "Label name (a symbol).")
   (%binding :accessor ldi-binding
             :initform :local
             :initarg  :binding
             :documentation "Binding for the symbol generated from this label:
:local, :global, or :weak."))
  (:documentation
   "Marks a label definition point within a segment.
During assembly, the current byte offset is recorded and a
symbol-entry is created (or updated) in the owning unit's
symbol table.
GOFF: may become an LD entry under the parent ED.
ELF: becomes a symbol in .symtab."))

;; ===============================================================
;; Label reference — emits a placeholder and generates a relocation
;; ===============================================================

(defclass label-ref-item (segment-item)
  ((%label    :accessor lri-label
              :initform nil
              :initarg  :label
              :documentation "Name of the referenced label (a symbol).")
   (%width    :accessor lri-width
              :initform 4
              :initarg  :width
              :documentation "Byte width of the reference field to emit.")
   (%rel-type :accessor lri-rel-type
              :initform nil
              :initarg  :rel-type
              :documentation "Relocation type keyword, e.g. :r-390-pc32dbl.
If NIL, a default absolute relocation is chosen for the architecture.")
   (%addend   :accessor lri-addend
              :initform 0
              :initarg  :addend
              :documentation "Constant addend for the relocation."))
  (:documentation
   "A reference to a label, emitting a placeholder of the given width
and generating a relocation entry.  During assembly, a zero-filled
field of %width bytes is written at the current position, and a
relocation is created in the owning unit's relocation list."))

;; ===============================================================
;; Alignment padding
;; ===============================================================

(defclass align-item (segment-item)
  ((%boundary  :accessor ali-boundary
               :initform 1
               :initarg  :boundary
               :documentation "Alignment boundary in bytes (must be a power of 2).")
   (%fill-byte :accessor ali-fill-byte
               :initform 0
               :initarg  :fill-byte
               :documentation "Byte value used for padding (default 0).
For code segments, an architecture-appropriate NOP encoding
may be substituted during assembly."))
  (:documentation
   "Alignment padding.  During assembly, emits zero or more fill bytes
to bring the current offset to a multiple of the boundary value."))

;; ===============================================================
;; Function item — a named, bounded region of instructions
;; ===============================================================

(defclass function-item (segment-item)
  ((%name       :accessor fi-name
                :initform nil
                :initarg  :name
                :documentation "Function name (a symbol or string).")
   (%body       :accessor fi-body
                :initform nil
                :initarg  :body
                :documentation "Ordered list of segment-items forming the function body.
May include instruction-items, label-def-items, label-ref-items,
align-items, and (for WASM) wasm-block-items.")
   (%properties :accessor fi-properties
                :initform nil
                :initarg  :properties
                :documentation "Plist for function-level metadata.
GOFF: amode, linkage-type for the LD entry.
ELF: visibility, binding for the STT_FUNC symbol.
General: calling-convention, stack-frame-size."))
  (:documentation
   "A named function within a code segment.  The body is a list of
segment-items that are flattened into the enclosing segment during
assembly.  A symbol-entry of type :function is created at the
function's starting offset, and its size is recorded after assembly.

GOFF: generates an LD entry under the parent ED.
ELF: generates a STT_FUNC symbol in .symtab.
WASM: use the wasm-function-item subclass for the full type signature
and local variable declarations."))

;; ===============================================================
;; Generic functions for the program model
;; ===============================================================

(defgeneric add-segment (unit name segment)
  (:documentation "Add a segment to a program-unit's segment alist."))

(defmethod add-segment ((unit program-unit) name segment)
  "Append a named segment to the unit's ordered segment alist."
  (setf (pun-segments unit)
        (append (pun-segments unit) (list (cons name segment))))
  segment)

(defgeneric add-symbol (unit symbol-entry)
  (:documentation "Register a symbol-entry in a program-unit's symbol table."))

(defmethod add-symbol ((unit program-unit) (sym symbol-entry))
  "Add a symbol to the unit's symbol table, keyed by its name."
  (setf (gethash (sym-name sym) (pun-symbols unit)) sym)
  sym)

(defgeneric add-relocation (unit relocation)
  (:documentation "Append a relocation to a program-unit's relocation list."))

(defmethod add-relocation ((unit program-unit) (rel relocation))
  "Append a relocation entry to the unit's relocation list."
  (push rel (pun-relocations unit))
  rel)

(defgeneric add-item (segment item)
  (:documentation "Append a segment-item to a segment's item list."))

(defmethod add-item ((seg segment) (item segment-item))
  "Push an item onto the segment's item list (reversed during building)."
  (push item (seg-items seg))
  item)

(defgeneric add-unit (program unit)
  (:documentation "Add a program-unit to a program."))

(defmethod add-unit ((pgm program) (unit program-unit))
  "Append a unit to the program's unit list and set the back-reference."
  (setf (pun-program unit) pgm)
  (setf (pgm-units pgm)
        (append (pgm-units pgm) (list unit)))
  unit)

(defgeneric lookup-segment (unit name)
  (:documentation "Look up a segment by name in a program-unit."))

(defmethod lookup-segment ((unit program-unit) name)
  "Find a segment by name in the unit's segment alist."
  (cdr (assoc name (pun-segments unit) :test #'equal)))

(defgeneric lookup-symbol (unit name)
  (:documentation "Look up a symbol by name in a program-unit."))

(defmethod lookup-symbol ((unit program-unit) name)
  "Find a symbol by name in the unit's symbol table."
  (gethash name (pun-symbols unit)))

(defgeneric finalize-segment (segment)
  (:documentation "Finalize a segment by reversing its item list into the correct order."))

(defmethod finalize-segment ((seg segment))
  "Reverse the segment's item list so items are in emission order."
  (setf (seg-items seg) (nreverse (seg-items seg)))
  seg)

;; ===============================================================
;; Convenience constructors
;; ===============================================================

(defun make-program (assembler &rest properties)
  "Create a program bound to the given assembler prototype."
  (make-instance 'program :assembler assembler :properties properties))

(defun make-unit (name &optional program)
  "Create a program-unit with the given name, optionally adding it to a program."
  (let ((unit (make-instance 'program-unit :name name)))
    (when program (add-unit program unit))
    unit))

(defun make-segment (name kind &key (align 1) (flags '(:read)) properties)
  "Create a segment with the given name, kind, and optional attributes."
  (make-instance 'segment :name name :kind kind
                           :align align :flags flags :properties properties))

(defun make-symbol-entry (name &key segment (offset 0) (binding :local)
                                    (sym-type :notype) (size 0) properties)
  "Create a symbol-entry with the given attributes."
  (make-instance 'symbol-entry :name name :segment segment :offset offset
                                :binding binding :sym-type sym-type
                                :size size :properties properties))

;; ===============================================================
;; Serialization protocol (to be implemented by format modules)
;; ===============================================================

(defgeneric emit-program (program format stream &key &allow-other-keys)
  (:documentation "Serialize PROGRAM to the specified FORMAT, writing bytes to STREAM.
FORMAT is a keyword such as :elf, :goff, or :wasm.
Implementations are provided by format-specific modules."))

;; ===============================================================
;; Data word item — integer data with deferred encoding
;; ===============================================================

(defclass data-word-item (segment-item)
  ((%value :accessor dwi-value
           :initform 0
           :initarg  :value
           :documentation "The integer value to encode.")
   (%width :accessor dwi-width
           :initform nil
           :initarg  :width
           :documentation "Width in bytes (1, 2, 4, 8, or 16).
If NIL, inferred from value magnitude during assembly."))
  (:documentation
   "An integer data value whose byte encoding is deferred to
assembly time.  The assembler determines endianness; width
is either explicit or inferred from the value's magnitude."))

;; ===============================================================
;; Program macro — expansion helpers
;; ===============================================================
;;
;; These functions run at macro-expansion time.  They take source
;; forms from the program body and return code forms to be spliced
;; into the macro expansion.

(eval-when (:compile-toplevel :load-toplevel :execute)

  ;; -- Symbol name comparison --------------------------------
  ;;
  ;; The program macro is used from architecture packages (specops.system-z,
  ;; specops.wasm, etc.) where symbols like FUNCTION, LABEL, CODE are
  ;; interned in the calling package, not in SPECOPS or CL.  We compare
  ;; by symbol name string so the macro works across packages.

  (defun %sym-name= (sym name-string)
    "Test whether SYM is a symbol whose name matches NAME-STRING (case-insensitive)."
    (and (symbolp sym) (string-equal (symbol-name sym) name-string)))

  ;; -- Section metadata --------------------------------------

  (defun %section-kind (section-sym)
    "Map a section declarator symbol to a segment kind keyword."
    (let ((name (symbol-name section-sym)))
      (cond ((string-equal name "CODE")   :code)
            ((string-equal name "DATA")   :data)
            ((string-equal name "RODATA") :rodata)
            ((string-equal name "BSS")    :bss)
            (t (error "Unknown section type: ~a" section-sym)))))

  (defun %section-flags (kind)
    "Return default permission flags for a segment kind."
    (ecase kind
      (:code   '(:read :execute :alloc))
      (:data   '(:read :write :alloc))
      (:rodata '(:read :alloc))
      (:bss    '(:read :write :alloc))))

  ;; -- Width keyword → byte count ----------------------------

  (defun %width-bytes (keyword)
    "Convert a width keyword to a byte count."
    (ecase keyword
      (:byte  1)
      (:half  2)
      (:word  4)
      (:dword 8)
      (:qword 16)))

  ;; -- Instruction expansion ---------------------------------

  (defun %expand-instruction (form)
    "Expand (:mnemonic args...) into a make-instance form for instruction-item.
Operand expressions are spliced so they evaluate at runtime (resolving
register bindings from :store)."
    `(make-instance 'instruction-item :expression (list ,@form)))

  ;; -- Body items (inside functions) -------------------------
  ;;
  ;; %expand-body-item returns a list of forms, each of which
  ;; evaluates to a segment-item.  These get collected into a
  ;; (list ...) form to set as the function body.

  (defun %expand-body-item (item)
    "Expand a single item within a function body.
Returns a list of forms that each evaluate to a segment-item instance."
    (cond
      ;; (:keyword args...) — assembler instruction
      ((and (listp item) (keywordp (car item)))
       (list (%expand-instruction item)))

      ;; bare symbol — shorthand label definition
      ((symbolp item)
       (list `(make-instance 'label-def-item :name ',item)))

      ;; (label name) or (label name :global) etc.
      ((and (listp item) (%sym-name= (car item) "LABEL"))
       (destructuring-bind (name &optional (binding :local)) (cdr item)
         (list `(make-instance 'label-def-item
                               :name ',name :binding ,binding))))

      ;; (align N)
      ((and (listp item) (%sym-name= (car item) "ALIGN"))
       (list `(make-instance 'align-item :boundary ,(second item))))

      ;; (ref label-name &key width rel-type addend)
      ((and (listp item) (%sym-name= (car item) "REF"))
       (destructuring-bind (label-name &key (width 4) rel-type (addend 0))
           (cdr item)
         (list `(make-instance 'label-ref-item
                               :label ',label-name :width ,width
                               :rel-type ,rel-type :addend ,addend))))

      ;; (raw byte-vector-form)
      ((and (listp item) (%sym-name= (car item) "RAW"))
       (list `(make-instance 'raw-bytes-item :data ,(second item))))

      (t (error "Unrecognized function body item: ~s" item))))

  ;; -- Code section items ------------------------------------
  ;;
  ;; %expand-code-item returns a list of forms that add items
  ;; to the segment via add-item.

  (defun %expand-code-item (item seg-var)
    "Expand a single code section item into forms that call add-item on SEG-VAR."
    (cond
      ;; (function name (options-plist) body...)
      ((and (listp item) (%sym-name= (car item) "FUNCTION"))
       (destructuring-bind (name options &rest body) (cdr item)
         (let ((fn-var (gensym "FUNC")))
           (list
            `(let ((,fn-var (make-instance 'function-item
                                           :name ',name
                                           :properties ',options)))
               (setf (fi-body ,fn-var)
                     (list ,@(loop :for bi :in body
                                   :append (%expand-body-item bi))))
               (add-item ,seg-var ,fn-var))))))

      ;; (label name) or (label name :global)
      ((and (listp item) (%sym-name= (car item) "LABEL"))
       (destructuring-bind (name &optional (binding :local)) (cdr item)
         (list `(add-item ,seg-var
                          (make-instance 'label-def-item
                                         :name ',name :binding ,binding)))))

      ;; bare symbol — shorthand label
      ((symbolp item)
       (list `(add-item ,seg-var
                        (make-instance 'label-def-item :name ',item))))

      ;; (align N)
      ((and (listp item) (%sym-name= (car item) "ALIGN"))
       (list `(add-item ,seg-var
                        (make-instance 'align-item :boundary ,(second item)))))

      ;; (ref label-name &key ...)
      ((and (listp item) (%sym-name= (car item) "REF"))
       (destructuring-bind (label-name &key (width 4) rel-type (addend 0))
           (cdr item)
         (list `(add-item ,seg-var
                          (make-instance 'label-ref-item
                                         :label ',label-name :width ,width
                                         :rel-type ,rel-type :addend ,addend)))))

      ;; (raw byte-vector-form)
      ((and (listp item) (%sym-name= (car item) "RAW"))
       (list `(add-item ,seg-var
                        (make-instance 'raw-bytes-item :data ,(second item)))))

      ;; (:keyword args...) — bare instruction (not inside a function)
      ((and (listp item) (keywordp (car item)))
       (list `(add-item ,seg-var ,(%expand-instruction item))))

      (t (error "Unrecognized code section item: ~s" item))))

  ;; -- Data value expansion ----------------------------------

  (defun %expand-data-value (value-form)
    "Generate a form that creates a segment-item for a data value.
Called at macro-expansion time; VALUE-FORM is the source expression."
    (cond
      ;; (:word value), (:byte value), (:half value), etc.
      ((and (listp value-form) (keywordp (car value-form))
            (member (car value-form) '(:byte :half :word :dword :qword)))
       `(make-instance 'data-word-item
                       :value ,(second value-form)
                       :width ,(%width-bytes (car value-form))))

      ;; (raw byte-vector-form)
      ((and (listp value-form) (%sym-name= (car value-form) "RAW"))
       `(make-instance 'raw-bytes-item :data ,(second value-form)))

      ;; (ref label-name &key ...)
      ((and (listp value-form) (%sym-name= (car value-form) "REF"))
       (destructuring-bind (label-name &key (width 4) rel-type (addend 0))
           (cdr value-form)
         `(make-instance 'label-ref-item
                         :label ',label-name :width ,width
                         :rel-type ,rel-type :addend ,addend)))

      ;; literal integer — data-word-item with inferred width
      ((integerp value-form)
       `(make-instance 'data-word-item :value ,value-form))

      ;; literal vector — raw bytes
      ((vectorp value-form)
       `(make-instance 'raw-bytes-item :data ,value-form))

      ;; other runtime expression — assume integer value
      (t `(make-instance 'data-word-item :value ,value-form))))

  ;; -- Data section items ------------------------------------

  (defun %expand-data-item (item seg-var)
    "Expand a single data section item into forms that call add-item on SEG-VAR."
    (cond
      ;; (align N)
      ((and (listp item) (%sym-name= (car item) "ALIGN"))
       (list `(add-item ,seg-var
                        (make-instance 'align-item :boundary ,(second item)))))

      ;; (raw byte-vector-form)
      ((and (listp item) (%sym-name= (car item) "RAW"))
       (list `(add-item ,seg-var
                        (make-instance 'raw-bytes-item :data ,(second item)))))

      ;; (label name &optional binding)
      ((and (listp item) (%sym-name= (car item) "LABEL"))
       (destructuring-bind (name &optional (binding :local)) (cdr item)
         (list `(add-item ,seg-var
                          (make-instance 'label-def-item
                                         :name ',name :binding ,binding)))))

      ;; (name value) — labeled data entry
      ;; name is a non-keyword symbol, value follows
      ((and (listp item) (symbolp (car item)) (not (keywordp (car item)))
            (cdr item))
       (let ((name (car item))
             (value (second item)))
         (list `(add-item ,seg-var
                          (make-instance 'label-def-item :name ',name))
               `(add-item ,seg-var ,(%expand-data-value value)))))

      ;; bare symbol — label shorthand
      ((symbolp item)
       (list `(add-item ,seg-var
                        (make-instance 'label-def-item :name ',item))))

      (t (error "Unrecognized data section item: ~s" item))))

  ;; -- Section expansion -------------------------------------

  (defun %expand-section (form unit-var)
    "Expand a section declaration (code, data, rodata, bss) into
forms that create the segment and populate it with items."
    (let* ((section-type (car form))
           (kind (%section-kind section-type))
           (rest (cdr form))
           ;; optional string name as first argument
           (name (if (stringp (car rest)) (car rest) nil))
           (items (if name (cdr rest) rest))
           (seg-name (or name (string-upcase (symbol-name section-type))))
           (seg-var (gensym "SEG"))
           (item-expander (ecase kind
                            (:code   #'%expand-code-item)
                            ((:data :rodata) #'%expand-data-item)
                            ;; bss: no items, just size reservation
                            (:bss   #'%expand-data-item))))
      (list
       `(let ((,seg-var (make-segment ,seg-name ,kind
                                      :flags ',(%section-flags kind))))
          (add-segment ,unit-var ,seg-name ,seg-var)
          ,@(loop :for item :in items
                  :append (funcall item-expander item seg-var))
          (finalize-segment ,seg-var)))))

  ) ; end eval-when

;; ===============================================================
;; The program macro
;; ===============================================================

(defmacro program (assembler &body body)
  "Build a program model from a declarative specification.

ASSEMBLER is a symbol naming an assembler prototype variable
(evaluated at both macro-expansion time for :store and at runtime).

Body forms:
  (params (:key value)...)       — configuration (see below)
  (code [\"name\"] items...)     — code segment
  (data [\"name\"] items...)     — writable data segment
  (rodata [\"name\"] items...)   — read-only data segment
  (bss [\"name\"] items...)      — zero-initialized data segment

Params keys:
  :name    unit-name-string      — program unit name (default \"main\")
  :entry   symbol                — entry point symbol name
  :store   (sym type)...         — register allocation bindings
  :exmode  keyword               — execution mode for the assembler

Code section items:
  (function name (options-plist) body...)
  (label name [:binding])
  (align N)
  (ref label-name &key width rel-type addend)
  (raw byte-vector-form)
  (:mnemonic args...)            — assembler instruction
  bare-symbol                    — shorthand label definition

Function body items:
  (:mnemonic args...)            — assembler instruction
  (label name [:binding])
  (align N)
  (ref label-name &key width rel-type addend)
  (raw byte-vector-form)
  bare-symbol                    — shorthand label definition

Data section items:
  (name value)                   — labeled data entry
  (label name [:binding])
  (align N)
  (raw byte-vector-form)
  bare-symbol                    — shorthand label definition

Data values:
  integer                        — data-word-item (width inferred)
  (:byte val), (:half val), (:word val), (:dword val), (:qword val)
                                 — data-word-item with explicit width
  (raw byte-vector-form)         — raw-bytes-item
  (ref label &key width rel-type addend) — label-ref-item
  #(bytes...)                    — raw-bytes-item

Returns an unassembled program instance.  Call build-program to
encode instructions and resolve labels."
  (let* (;; Find and parse the (params ...) form
         (params-form (find "PARAMS" body
                            :key (lambda (x) (and (listp x) (symbolp (car x))
                                                  (symbol-name (car x))))
                            :test #'string-equal))
         (params (when params-form
                   (loop :for p :in (rest params-form)
                         :collect (cons (first p) (rest p)))))
         (sections (remove params-form body))
         ;; Extract specific params
         (store-specs  (cdr (assoc :store  params)))
         (entry-sym    (car (cdr (assoc :entry  params))))
         (unit-name    (or  (car (cdr (assoc :name   params))) "main"))
         (exmode       (car (cdr (assoc :exmode params))))
         ;; Gensyms
         (pgm-var  (gensym "PGM"))
         (unit-var (gensym "UNIT"))
         ;; Register allocation (at macro-expansion time, like assemble)
         (located (when store-specs
                    (locate (symbol-value assembler) store-specs))))
    `(let* ((,pgm-var (make-instance 'program :assembler ,assembler
                                     ,@(when exmode
                                         `(:properties (list :exmode ,exmode)))))
            (,unit-var (make-unit ,unit-name ,pgm-var))
            ,@located)
       ,@(loop :for section :in sections
               :append (%expand-section section unit-var))
       ,@(when entry-sym
           `((setf (pgm-entry-point ,pgm-var) ',entry-sym)))
       ,pgm-var)))

;; ===============================================================
;; Assembler endianness protocol
;; ===============================================================

(defgeneric asm-endianness (assembler)
  (:documentation "Return the byte order for this assembler: :big or :little.
Used by build-program to encode data-word-items.")
  (:method ((assembler assembler))
    "Default: big-endian (conservative; most historical ISAs are big-endian)."
    :big))

(defgeneric asm-elf-machine (assembler)
  (:documentation "Return the ELF e_machine value for this assembler.
E.g. 22 for EM_S390, 62 for EM_X86_64.")
  (:method ((assembler assembler))
    "Default: error — architecture must provide a method."
    (error "No ELF machine type defined for assembler ~a" assembler)))

(defun %seg-bytes-as-octets (seg assembler)
  "Convert a segment's byte vector to a (unsigned-byte 8) vector.
Code segments may use wider element types (e.g. System Z uses 16-bit
units); this converts them to bytes in the assembler's endianness."
  (let ((bytes (seg-bytes seg))
        (endian (asm-endianness assembler)))
    (when (null bytes) (return-from %seg-bytes-as-octets #()))
    (let ((etype (array-element-type bytes)))
      (cond
        ((equal etype '(unsigned-byte 8)) bytes)
        ((equal etype '(unsigned-byte 16))
         (let ((out (make-array (* 2 (length bytes))
                                :element-type '(unsigned-byte 8))))
           (loop :for i :below (length bytes)
                 :for w := (aref bytes i)
                 :for j := (* 2 i)
                 :do (ecase endian
                       (:big    (setf (aref out j)      (logand #xFF (ash w -8))
                                      (aref out (1+ j)) (logand #xFF w)))
                       (:little (setf (aref out j)      (logand #xFF w)
                                      (aref out (1+ j)) (logand #xFF (ash w -8))))))
           out))
        ((equal etype '(unsigned-byte 32))
         (let ((out (make-array (* 4 (length bytes))
                                :element-type '(unsigned-byte 8))))
           (loop :for i :below (length bytes)
                 :for w := (aref bytes i)
                 :for j := (* 4 i)
                 :do (loop :for k :below 4
                           :do (setf (aref out (+ j (ecase endian
                                                       (:big (- 3 k))
                                                       (:little k))))
                                     (logand #xFF (ash w (* -8 k))))))
           out))
        (t (error "Unsupported element type ~a" etype))))))

;; ===============================================================
;; Data encoding helpers
;; ===============================================================

(defun integer-byte-width (value)
  "Return the minimum number of bytes needed to represent VALUE as unsigned.
Returns 1 for 0."
  (if (zerop value)
      1
      (ceiling (integer-length (abs value)) 8)))

(defun encode-integer-bytes (value width endianness)
  "Encode an integer VALUE into a byte vector of WIDTH bytes.
ENDIANNESS is :big or :little."
  (let ((bytes (make-array width :element-type '(unsigned-byte 8) :initial-element 0)))
    (loop :for i :below width
          :do (setf (aref bytes (if (eq endianness :big)
                                    (- width 1 i)
                                    i))
                    (logand #xFF (ash value (* -8 i)))))
    bytes))

;; ===============================================================
;; build-program — assemble all segments
;; ===============================================================

(defgeneric build-program (program &key &allow-other-keys)
  (:documentation "Assemble all segments in PROGRAM.
Encodes instructions in code segments, encodes data values in data
segments, resolves intra-segment labels, generates symbol-entry and
relocation records, and stores the resulting byte vectors in each
segment's %bytes slot.

After build-program, each segment has:
  - seg-bytes: assembled byte vector (NIL for :bss)
  - seg-size: byte length of assembled content
The unit's symbol table and relocation list are populated."))

(defmethod build-program ((pgm program) &key &allow-other-keys)
  "Assemble all units and segments in the program."
  (let ((assembler (pgm-assembler pgm)))
    (dolist (unit (pgm-units pgm))
      (loop :for (seg-name . seg) :in (pun-segments unit)
            :do (ecase (seg-kind seg)
                  (:code
                   (assemble-code-segment assembler seg unit pgm))
                  ((:data :rodata)
                   (assemble-data-segment assembler seg unit pgm))
                  (:bss
                   ;; BSS: no bytes, just accumulate size from items
                   (assemble-bss-segment seg unit)))))
    pgm))

;; ===============================================================
;; Code segment assembly
;; ===============================================================

(defun %make-code-api (assembler pgm codes marked-points tag-points)
  "Create the program-api closure for instruction assembly.
This replicates the api-access closure from compose, providing the
same interface that lexicon functions expect."
  (let ((exmode (getf (pgm-properties pgm) :exmode)))
    (lambda (mode &rest args)
      (case mode
        (:assembler-type (asm-type assembler))
        (:exmode (or exmode (first (asm-exmodes assembler))))
        (:label (destructuring-bind (field-length offset-bits symbol) args
                  (typecase symbol
                    (integer symbol)
                    (symbol
                     (let ((spec (list symbol offset-bits field-length
                                      (fill-pointer codes))))
                       (or (and (assoc symbol marked-points)
                                (locate-relative assembler spec
                                                 (list :marked-points marked-points)))
                           (progn (push spec (cdr tag-points))
                                  0)))))))))))

(defun %emit-bytes-to-vector (bytes codes)
  "Append a byte vector to the adjustable codes vector."
  (loop :for b :across bytes
        :do (unless (< (fill-pointer codes) (1- (length codes)))
              (adjust-array codes (+ 1024 (length codes)) :initial-element 0))
            (vector-push b codes)))

(defun %assemble-item-list (items assembler unit seg codes
                            marked-points tag-points relocations api)
  "Walk a list of segment-items, encoding them into CODES.
MARKED-POINTS, TAG-POINTS, and RELOCATIONS are modified in place."
  (let ((breadth (asm-breadth assembler)))
    (flet ((add-value (value)
             (unless (< (fill-pointer codes) (1- (length codes)))
               (adjust-array codes (+ 1024 (length codes)) :initial-element 0))
             (vector-push value codes)))
      (dolist (item items)
        (typecase item

          (instruction-item
           (let* ((expr (ii-expression item))
                  (mnemonic (first expr))
                  (operands (rest expr))
                  (build-fn (of-lexicon assembler mnemonic)))
             (when (not build-fn)
               (error "Unknown instruction mnemonic: ~a" mnemonic))
             (multiple-value-bind (code properties)
                 (if (not (functionp build-fn))
                     build-fn
                     (apply build-fn api operands))
               (if (functionp code)
                   ;; deferred-resolution instruction (tag-point)
                   (let ((bw (or (getf properties :breadth) 0)))
                     (push (append (list (fill-pointer codes) code) properties)
                           (cdr tag-points))
                     (dotimes (i bw) (add-value 0)))
                   ;; immediate result: serialize into the byte vector
                   (if (listp code)
                       (loop :for c :in code :when c
                             :do (serialize c breadth #'add-value))
                       (serialize code breadth #'add-value))))))

          (label-def-item
           (let ((name (ldi-name item))
                 (binding (ldi-binding item)))
             ;; Record position for intra-segment label resolution
             (push (cons name (fill-pointer codes)) (cdr marked-points))
             ;; Create/update symbol in the unit's symbol table
             (let ((sym (or (gethash name (pun-symbols unit))
                            (setf (gethash name (pun-symbols unit))
                                  (make-instance 'symbol-entry :name name)))))
               (setf (sym-segment sym) seg
                     (sym-offset sym) (fill-pointer codes)
                     (sym-binding sym) binding))))

          (function-item
           (let ((fn-start (fill-pointer codes))
                 (name (fi-name item))
                 (binding (or (getf (fi-properties item) :binding) :local)))
             ;; Create symbol for function start
             (let ((sym (or (gethash name (pun-symbols unit))
                            (setf (gethash name (pun-symbols unit))
                                  (make-instance 'symbol-entry :name name)))))
               (setf (sym-segment sym) seg
                     (sym-offset sym) fn-start
                     (sym-binding sym) binding
                     (sym-type sym) :function
                     (sym-properties sym) (fi-properties item))
               ;; Record as a marked-point for label resolution
               (push (cons name fn-start) (cdr marked-points))
               ;; Recurse into function body
               (%assemble-item-list (fi-body item) assembler unit seg codes
                                    marked-points tag-points relocations api)
               ;; Record function size
               (setf (sym-size sym) (- (fill-pointer codes) fn-start)))))

          (label-ref-item
           (let ((pos (fill-pointer codes))
                 (width (lri-width item)))
             ;; Emit zero-filled placeholder
             (dotimes (i width) (add-value 0))
             ;; Create relocation entry
             (push (make-instance 'relocation
                                  :symbol nil  ; resolved later by name
                                  :segment seg
                                  :offset pos
                                  :rel-type (lri-rel-type item)
                                  :width width
                                  :addend (lri-addend item))
                   (cdr relocations))
             ;; Also record for potential intra-segment resolution
             ;; using the label name on the relocation's properties
             (setf (rel-symbol (cadr relocations))
                   (make-instance 'symbol-entry :name (lri-label item)))))

          (align-item
           (let ((boundary (ali-boundary item))
                 (fill (ali-fill-byte item)))
             (loop :while (not (zerop (mod (fill-pointer codes) boundary)))
                   :do (add-value fill))))

          (raw-bytes-item
           (loop :for b :across (rbi-data item)
                 :do (add-value b)))

          (data-word-item
           ;; Can appear in code segments (inline data)
           (let* ((endian (asm-endianness assembler))
                  (value (dwi-value item))
                  (width (or (dwi-width item)
                             (max 1 (integer-byte-width value))))
                  (bytes (encode-integer-bytes value width endian)))
             (%emit-bytes-to-vector bytes codes))))))))

(defun assemble-code-segment (assembler seg unit pgm)
  "Assemble a code segment: encode instructions, resolve labels,
populate symbols and relocations."
  (let* ((breadth (asm-breadth assembler))
         (codes (make-array 1024 :element-type (list 'unsigned-byte breadth)
                                 :initial-element 0 :adjustable t :fill-pointer 0))
         ;; Use cons cells with a dummy head so we can push onto cdr in-place
         (marked-points (list :head))
         (tag-points (list :head))
         (relocations (list :head))
         (api (%make-code-api assembler pgm codes
                              (cdr marked-points) (cdr tag-points))))
    ;; Pass 1: walk items, encode instructions
    (%assemble-item-list (seg-items seg) assembler unit seg codes
                         marked-points tag-points relocations api)

    ;; Pass 2: resolve forward references (tag-points)
    (let ((two-power (floor (log breadth 2)))
          (mp (cdr marked-points)))
      (loop :for tag-spec :in (cdr tag-points)
            :do (destructuring-bind (symbol bit-offset field-length index) tag-spec
                  (let ((props-for-locate (list :marked-points mp)))
                    (let ((value (locate-relative assembler tag-spec props-for-locate))
                          (unaligned-bits (logand bit-offset (1- (ash 1 two-power)))))
                      (setf (fill-pointer codes) (+ index (ash bit-offset (- two-power))))
                      (unless (zerop unaligned-bits)
                        (let ((first-original (aref codes (fill-pointer codes))))
                          (serialize (+ first-original
                                        (ash value (- (- field-length
                                                         (- breadth unaligned-bits)))))
                                     breadth
                                     (lambda (v)
                                       (unless (< (fill-pointer codes) (1- (length codes)))
                                         (adjust-array codes (+ 1024 (length codes))
                                                       :initial-element 0))
                                       (vector-push v codes)))))
                      (serialize (rest (assoc symbol mp))
                                 breadth
                                 (lambda (v)
                                   (unless (< (fill-pointer codes) (1- (length codes)))
                                     (adjust-array codes (+ 1024 (length codes))
                                                   :initial-element 0))
                                   (vector-push v codes))))))))

    ;; Store relocations on the unit
    (dolist (rel (cdr relocations))
      (push rel (pun-relocations unit)))

    ;; Convert to final byte vector and store on segment
    (let* ((len (fill-pointer codes))
           (output (make-array len :element-type (list 'unsigned-byte breadth))))
      (loop :for i :below len :do (setf (aref output i) (aref codes i)))
      (setf (seg-bytes seg) output
            (seg-size seg) len))
    seg))

;; ===============================================================
;; Data segment assembly
;; ===============================================================

(defun assemble-data-segment (assembler seg unit pgm)
  "Assemble a data or rodata segment: encode data values, record labels."
  (declare (ignore pgm))
  (let* ((endian (asm-endianness assembler))
         (codes (make-array 256 :element-type '(unsigned-byte 8)
                                :initial-element 0 :adjustable t :fill-pointer 0)))
    (flet ((add-byte (b)
             (unless (< (fill-pointer codes) (1- (length codes)))
               (adjust-array codes (+ 1024 (length codes)) :initial-element 0))
             (vector-push b codes)))
      (dolist (item (seg-items seg))
        (typecase item

          (label-def-item
           (let ((name (ldi-name item))
                 (binding (ldi-binding item)))
             (let ((sym (or (gethash name (pun-symbols unit))
                            (setf (gethash name (pun-symbols unit))
                                  (make-instance 'symbol-entry :name name)))))
               (setf (sym-segment sym) seg
                     (sym-offset sym) (fill-pointer codes)
                     (sym-binding sym) binding))))

          (data-word-item
           (let* ((value (dwi-value item))
                  (width (or (dwi-width item)
                             (max 1 (integer-byte-width value))))
                  (bytes (encode-integer-bytes value width endian)))
             (%emit-bytes-to-vector bytes codes)))

          (raw-bytes-item
           (loop :for b :across (rbi-data item)
                 :do (add-byte b)))

          (label-ref-item
           (let ((pos (fill-pointer codes))
                 (width (lri-width item)))
             (dotimes (i width) (add-byte 0))
             (push (make-instance 'relocation
                                  :symbol (make-instance 'symbol-entry
                                                         :name (lri-label item))
                                  :segment seg
                                  :offset pos
                                  :rel-type (lri-rel-type item)
                                  :width width
                                  :addend (lri-addend item))
                   (pun-relocations unit))))

          (align-item
           (let ((boundary (ali-boundary item))
                 (fill (ali-fill-byte item)))
             (loop :while (not (zerop (mod (fill-pointer codes) boundary)))
                   :do (add-byte fill)))))))

    ;; Store final bytes
    (let* ((len (fill-pointer codes))
           (output (make-array len :element-type '(unsigned-byte 8))))
      (loop :for i :below len :do (setf (aref output i) (aref codes i)))
      (setf (seg-bytes seg) output
            (seg-size seg) len))
    seg))

;; ===============================================================
;; BSS segment handling
;; ===============================================================

(defun assemble-bss-segment (seg unit)
  "Process a BSS segment: no bytes emitted, just accumulate size
from items and record symbols."
  (let ((offset 0))
    (dolist (item (seg-items seg))
      (typecase item
        (label-def-item
         (let ((name (ldi-name item))
               (binding (ldi-binding item)))
           (let ((sym (or (gethash name (pun-symbols unit))
                          (setf (gethash name (pun-symbols unit))
                                (make-instance 'symbol-entry :name name)))))
             (setf (sym-segment sym) seg
                   (sym-offset sym) offset
                   (sym-binding sym) binding))))
        (data-word-item
         (incf offset (or (dwi-width item)
                          (max 1 (integer-byte-width (dwi-value item))))))
        (raw-bytes-item
         (incf offset (length (rbi-data item))))
        (align-item
         (let ((boundary (ali-boundary item)))
           (setf offset (* boundary (ceiling offset boundary)))))))
    (setf (seg-bytes seg) nil
          (seg-size seg) offset)
    seg))

;;;; elf.lisp — ELF binary format emitter for SpecOps
;;;
;;; Writes ELF64 static executables from assembled programs.
;;; Uses separate PT_LOAD segments for code (RX) and data (RW)
;;; with proper page alignment between them.
;;;
;;; Supports any architecture that provides asm-endianness and
;;; asm-elf-machine methods.

(in-package #:specops)

;; ═══════════════════════════════════════════════════════════════
;; ELF constants
;; ═══════════════════════════════════════════════════════════════

(defconstant +elfclass64+       2)
(defconstant +elfdata2lsb+      1)
(defconstant +elfdata2msb+      2)
(defconstant +et-exec+          2)
(defconstant +ev-current+       1)
(defconstant +pt-load+          1)
(defconstant +pf-x+           #x1)
(defconstant +pf-w+           #x2)
(defconstant +pf-r+           #x4)
(defconstant +sht-null+         0)
(defconstant +sht-progbits+     1)
(defconstant +sht-symtab+       2)
(defconstant +sht-strtab+       3)
(defconstant +sht-nobits+       8)
(defconstant +shf-write+      #x1)
(defconstant +shf-alloc+      #x2)
(defconstant +shf-execinstr+  #x4)
(defconstant +stb-local+        0)
(defconstant +stb-global+       1)
(defconstant +stb-weak+         2)
(defconstant +stt-notype+       0)
(defconstant +stt-object+       1)
(defconstant +stt-func+         2)
(defconstant +stt-section+      3)
(defconstant +shn-undef+        0)
(defconstant +elf64-ehdr-size+ 64)
(defconstant +elf64-phdr-size+ 56)
(defconstant +elf64-shdr-size+ 64)
(defconstant +elf64-sym-size+  24)

;; ═══════════════════════════════════════════════════════════════
;; Binary writing helpers
;; ═══════════════════════════════════════════════════════════════

(defun %write-u8 (stream value)
  (write-byte (logand #xFF value) stream))

(defun %write-u16 (stream value endian)
  (ecase endian
    (:big    (%write-u8 stream (ash value -8))
             (%write-u8 stream value))
    (:little (%write-u8 stream value)
             (%write-u8 stream (ash value -8)))))

(defun %write-u32 (stream value endian)
  (ecase endian
    (:big    (%write-u16 stream (ash value -16) :big)
             (%write-u16 stream value :big))
    (:little (%write-u16 stream value :little)
             (%write-u16 stream (ash value -16) :little))))

(defun %write-u64 (stream value endian)
  (ecase endian
    (:big    (%write-u32 stream (ash value -32) :big)
             (%write-u32 stream value :big))
    (:little (%write-u32 stream value :little)
             (%write-u32 stream (ash value -32) :little))))

(defun %write-zeros (stream count)
  (dotimes (i count) (write-byte 0 stream)))

(defun %write-bytes (stream vector)
  (loop :for b :across vector :do (write-byte b stream)))

(defun %align-offset (offset alignment)
  "Round OFFSET up to the next multiple of ALIGNMENT."
  (if (zerop (mod offset alignment))
      offset
      (* alignment (ceiling offset alignment))))

;; %seg-bytes-as-octets is defined in program.lisp (shared with goff.lisp)

(defun %elf-unit-bytes (seg)
  "Return the byte multiplier for a segment's element type.
E.g. (unsigned-byte 16) → 2."
  (let* ((b (seg-bytes seg))
         (etype (when b (array-element-type b))))
    (cond ((null etype) 1)
          ((equal etype '(unsigned-byte 16)) 2)
          ((equal etype '(unsigned-byte 32)) 4)
          (t 1))))

;; ═══════════════════════════════════════════════════════════════
;; String table builder
;; ═══════════════════════════════════════════════════════════════

(defun %build-string-table (names)
  "Build an ELF string table from a list of strings.
Returns (values byte-vector name-to-offset-alist)."
  (let ((offset 1) (mapping nil) (parts (list (vector 0))))
    (dolist (name names)
      (let ((encoded (map '(vector (unsigned-byte 8)) #'char-code name)))
        (push (cons name offset) mapping)
        (push encoded parts)
        (push (vector 0) parts)
        (incf offset (1+ (length encoded)))))
    (let* ((total offset)
           (table (make-array total :element-type '(unsigned-byte 8) :initial-element 0))
           (pos 0))
      (dolist (part (nreverse parts))
        (loop :for b :across part :do (setf (aref table pos) b) (incf pos)))
      (values table (nreverse mapping)))))

;; ═══════════════════════════════════════════════════════════════
;; Structure writers
;; ═══════════════════════════════════════════════════════════════

(defun %write-elf64-ehdr (stream &key endian machine entry phoff shoff
                                      phnum shnum shstrndx)
  (%write-u8 stream #x7F)
  (loop :for c :across "ELF" :do (%write-u8 stream (char-code c)))
  (%write-u8 stream +elfclass64+)
  (%write-u8 stream (ecase endian (:big +elfdata2msb+) (:little +elfdata2lsb+)))
  (%write-u8 stream +ev-current+)
  (%write-u8 stream 0)
  (%write-zeros stream 8)
  (%write-u16 stream +et-exec+ endian)
  (%write-u16 stream machine endian)
  (%write-u32 stream +ev-current+ endian)
  (%write-u64 stream entry endian)
  (%write-u64 stream phoff endian)
  (%write-u64 stream shoff endian)
  (%write-u32 stream 0 endian)
  (%write-u16 stream +elf64-ehdr-size+ endian)
  (%write-u16 stream +elf64-phdr-size+ endian)
  (%write-u16 stream phnum endian)
  (%write-u16 stream +elf64-shdr-size+ endian)
  (%write-u16 stream shnum endian)
  (%write-u16 stream shstrndx endian))

(defun %write-elf64-phdr (stream &key endian type flags offset vaddr paddr
                                      filesz memsz align)
  (%write-u32 stream type endian)
  (%write-u32 stream flags endian)
  (%write-u64 stream offset endian)
  (%write-u64 stream vaddr endian)
  (%write-u64 stream paddr endian)
  (%write-u64 stream filesz endian)
  (%write-u64 stream memsz endian)
  (%write-u64 stream align endian))

(defun %write-elf64-shdr (stream &key endian name type flags addr offset
                                      size link info addralign entsize)
  (%write-u32 stream (or name 0) endian)
  (%write-u32 stream (or type 0) endian)
  (%write-u64 stream (or flags 0) endian)
  (%write-u64 stream (or addr 0) endian)
  (%write-u64 stream (or offset 0) endian)
  (%write-u64 stream (or size 0) endian)
  (%write-u32 stream (or link 0) endian)
  (%write-u32 stream (or info 0) endian)
  (%write-u64 stream (or addralign 0) endian)
  (%write-u64 stream (or entsize 0) endian))

(defun %write-elf64-sym (stream &key endian name info other shndx value size)
  (%write-u32 stream (or name 0) endian)
  (%write-u8  stream (or info 0))
  (%write-u8  stream (or other 0))
  (%write-u16 stream (or shndx 0) endian)
  (%write-u64 stream (or value 0) endian)
  (%write-u64 stream (or size 0) endian))

;; ═══════════════════════════════════════════════════════════════
;; Flag conversion helpers
;; ═══════════════════════════════════════════════════════════════

(defun %sym-binding-to-elf (binding)
  (ecase binding (:local +stb-local+) (:global +stb-global+) (:weak +stb-weak+)))

(defun %sym-type-to-elf (sym-type)
  (ecase sym-type
    (:notype +stt-notype+) (:function +stt-func+)
    (:object +stt-object+) (:section +stt-section+)))

(defun %elf-st-info (binding type) (logior (ash binding 4) type))

(defun %seg-flags-to-elf-shflags (flags)
  (let ((f 0))
    (when (member :alloc flags)   (setf f (logior f +shf-alloc+)))
    (when (member :write flags)   (setf f (logior f +shf-write+)))
    (when (member :execute flags) (setf f (logior f +shf-execinstr+)))
    f))

;; ═══════════════════════════════════════════════════════════════
;; emit-program :elf — ELF64 static executable emitter
;; ═══════════════════════════════════════════════════════════════

(defmethod emit-program ((pgm program) (format (eql :elf)) stream
                         &key (base-vaddr #x400000) (page-align #x1000))
  "Write an ELF64 static executable to STREAM.
The program must have been assembled with build-program first.

Uses separate PT_LOAD segments: RX for code/rodata, RW for writable
data, with page-aligned boundaries between them.

Keyword arguments:
  BASE-VADDR  — virtual address base (default #x400000)
  PAGE-ALIGN  — page alignment for PT_LOAD (default #x1000)"
  (let* ((assembler (pgm-assembler pgm))
         (endian (asm-endianness assembler))
         (machine (asm-elf-machine assembler))
         (unit (first (pgm-units pgm)))
         ;; Collect loadable sections with octet content
         ;; Each entry: (seg name octets file-offset size)
         (all-sections
           (loop :for (name . seg) :in (pun-segments unit)
                 :when (member (seg-kind seg) '(:code :data :rodata))
                   :collect (list seg name (%seg-bytes-as-octets seg assembler) 0 0)))
         ;; Classify into RX and RW
         (rx-sections (loop :for e :in all-sections
                            :unless (member :write (seg-flags (first e))) :collect e))
         (rw-sections (loop :for e :in all-sections
                            :when (member :write (seg-flags (first e))) :collect e))
         (has-rx (not (null rx-sections)))
         (has-rw (not (null rw-sections)))
         (num-phdrs (+ (if has-rx 1 0) (if has-rw 1 0)))
         (content-start (+ +elf64-ehdr-size+ (* num-phdrs +elf64-phdr-size+)))
         (file-pos content-start))

    ;; ── Layout RX sections ────────────────────────────────────
    (dolist (entry rx-sections)
      (let* ((seg (first entry))
             (octets (third entry))
             (aligned (%align-offset file-pos (max 1 (seg-align seg)))))
        (setf (fourth entry) aligned
              (fifth entry) (length octets)
              file-pos (+ aligned (length octets)))))

    (let ((rx-end file-pos))
      ;; ── Page gap + layout RW sections ────────────────────────
      (when has-rw
        (setf file-pos (%align-offset file-pos page-align)))
      (let ((rw-file-start file-pos))
        (dolist (entry rw-sections)
          (let* ((seg (first entry))
                 (octets (third entry))
                 (aligned (%align-offset file-pos (max 1 (seg-align seg)))))
            (setf (fourth entry) aligned
                  (fifth entry) (length octets)
                  file-pos (+ aligned (length octets)))))

        (let* ((section-layout (append rx-sections rw-sections))
               ;; ── String tables ──────────────────────────────────
               (section-names (append (mapcar #'second section-layout)
                                      '(".shstrtab" ".strtab" ".symtab")))
               (symbols-list
                 (let ((syms nil))
                   (maphash (lambda (k sym) (declare (ignore k))
                              (when (sym-segment sym) (push sym syms)))
                            (pun-symbols unit))
                   (stable-sort syms (lambda (a b)
                                       (and (eq (sym-binding a) :local)
                                            (not (eq (sym-binding b) :local)))))))
               (first-global-idx
                 (1+ (or (position-if (lambda (s) (not (eq (sym-binding s) :local)))
                                      symbols-list)
                         (length symbols-list)))))

          (multiple-value-bind (shstrtab shstrtab-map) (%build-string-table section-names)
            (multiple-value-bind (strtab strtab-map)
                (%build-string-table (mapcar (lambda (s) (string (sym-name s))) symbols-list))

              (let* ((shstrtab-offset file-pos)
                     (strtab-offset (+ shstrtab-offset (length shstrtab)))
                     (symtab-offset (+ strtab-offset (length strtab)))
                     (symtab-size (* +elf64-sym-size+ (1+ (length symbols-list))))
                     (shdr-offset (%align-offset (+ symtab-offset symtab-size) 8))
                     (num-shdrs (+ 1 (length section-layout) 3))
                     (shstrtab-shndx (+ 1 (length section-layout)))
                     (strtab-shndx (1+ shstrtab-shndx))
                     (seg-to-shndx (loop :for (seg . nil) :in section-layout
                                         :for i :from 1 :collect (cons seg i)))
                     ;; Entry point
                     (entry-vaddr
                       (let* ((esn (pgm-entry-point pgm))
                              (es (when esn (gethash esn (pun-symbols unit)))))
                         (if es
                             (let ((el (find (sym-segment es) section-layout :key #'car)))
                               (if el (+ base-vaddr (fourth el)
                                         (* (sym-offset es) (%elf-unit-bytes (sym-segment es))))
                                   base-vaddr))
                             base-vaddr))))

                ;; ── Write ───────────────────────────────────────

                ;; ELF header
                (%write-elf64-ehdr stream :endian endian :machine machine
                                   :entry entry-vaddr
                                   :phoff +elf64-ehdr-size+ :shoff shdr-offset
                                   :phnum num-phdrs :shnum num-shdrs
                                   :shstrndx shstrtab-shndx)

                ;; Program headers
                (when has-rx
                  (%write-elf64-phdr stream :endian endian
                                    :type +pt-load+ :flags (logior +pf-r+ +pf-x+)
                                    :offset 0 :vaddr base-vaddr :paddr base-vaddr
                                    :filesz rx-end :memsz rx-end :align page-align))
                (when has-rw
                  (%write-elf64-phdr stream :endian endian
                                    :type +pt-load+ :flags (logior +pf-r+ +pf-w+)
                                    :offset rw-file-start
                                    :vaddr (+ base-vaddr rw-file-start)
                                    :paddr (+ base-vaddr rw-file-start)
                                    :filesz (- file-pos rw-file-start)
                                    :memsz (- file-pos rw-file-start)
                                    :align page-align))

                ;; Section content with alignment padding
                (let ((cur content-start))
                  (dolist (entry section-layout)
                    (let ((foff (fourth entry)) (octets (third entry)))
                      (when (> foff cur) (%write-zeros stream (- foff cur)))
                      (%write-bytes stream octets)
                      (setf cur (+ foff (length octets))))))

                ;; Pad to non-loadable area
                (let ((last-entry (car (last section-layout))))
                  (when last-entry
                    (let ((end-of-last (+ (fourth last-entry) (fifth last-entry))))
                      (when (< end-of-last shstrtab-offset)
                        (%write-zeros stream (- shstrtab-offset end-of-last))))))

                ;; .shstrtab, .strtab
                (%write-bytes stream shstrtab)
                (%write-bytes stream strtab)

                ;; .symtab
                (%write-elf64-sym stream :endian endian) ; null entry
                (dolist (sym symbols-list)
                  (let* ((ub (%elf-unit-bytes (sym-segment sym)))
                         (boff (* (sym-offset sym) ub))
                         (bsz  (* (sym-size sym) ub)))
                    (%write-elf64-sym stream :endian endian
                      :name (or (cdr (assoc (string (sym-name sym)) strtab-map :test #'string=))
                                0)
                      :info (%elf-st-info (%sym-binding-to-elf (sym-binding sym))
                                          (%sym-type-to-elf (sym-type sym)))
                      :shndx (or (cdr (assoc (sym-segment sym) seg-to-shndx)) +shn-undef+)
                      :value boff :size bsz)))

                ;; Padding before section headers
                (let ((cur (+ symtab-offset symtab-size)))
                  (when (< cur shdr-offset) (%write-zeros stream (- shdr-offset cur))))

                ;; Section header table
                (%write-elf64-shdr stream :endian endian) ; null

                (dolist (entry section-layout)
                  (destructuring-bind (seg name octets foff size) entry
                    (declare (ignore octets))
                    (%write-elf64-shdr stream :endian endian
                      :name (or (cdr (assoc name shstrtab-map :test #'string=)) 0)
                      :type (if (eq (seg-kind seg) :bss) +sht-nobits+ +sht-progbits+)
                      :flags (%seg-flags-to-elf-shflags (seg-flags seg))
                      :addr (+ base-vaddr foff) :offset foff :size size
                      :addralign (seg-align seg))))

                (%write-elf64-shdr stream :endian endian
                  :name (or (cdr (assoc ".shstrtab" shstrtab-map :test #'string=)) 0)
                  :type +sht-strtab+ :offset shstrtab-offset :size (length shstrtab))
                (%write-elf64-shdr stream :endian endian
                  :name (or (cdr (assoc ".strtab" shstrtab-map :test #'string=)) 0)
                  :type +sht-strtab+ :offset strtab-offset :size (length strtab))
                (%write-elf64-shdr stream :endian endian
                  :name (or (cdr (assoc ".symtab" shstrtab-map :test #'string=)) 0)
                  :type +sht-symtab+ :offset symtab-offset :size symtab-size
                  :link strtab-shndx :info first-global-idx
                  :addralign 8 :entsize +elf64-sym-size+)

                (+ shdr-offset (* num-shdrs +elf64-shdr-size+))))))))))

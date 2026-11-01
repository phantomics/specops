;;;; scan.lisp

(in-package #:specops)

(defclass stop-rule ()
  ((%initargs :initform nil       ;; the constant initargs a :stop-by field gave the rule,
              :accessor rule-initargs))) ;; so that a fresh one can be made at run time

(defclass count-rule        (stop-rule)
  ((n    :initarg :n
         :reader  rule-n)))   ;; integer or form

(defclass terminator-rule   (stop-rule)
  ((codes :initarg :codes
          :reader rule-codes))) ;; list of code forms: one per element of the terminator

(defclass bounded-terminator-rule (terminator-rule)
  ((n    :initarg :n
         :reader rule-n)))    ;; integer or form: the buffer's size in elements

(defclass continuation-rule (stop-rule)
  ((bit  :initarg :bit  :initform 7 :reader rule-bit)))

(defclass field-plan () ())

(defclass scalar-plan (field-plan)
  ((width  :initarg  :width)
   (signed :initarg  :signed
           :initform nil)
   (enum   :initarg  :enum
           :initform nil)))

(defclass scan-plan   (field-plan)
  ((rule      :initarg  :rule)
   (assembler :initarg  :assembler)
   (width     :initarg  :width
              :initform 1)
   (element   :initarg  :element      ;; a plan that reads and writes each element, when
              :initform nil)))        ;; elements are not raw codes of a fixed width

(defclass manifest-plan (field-plan)
  ((manifest :initarg :manifest       ;; the name of the sub-manifest
             :reader plan-manifest)))

(defclass case-plan   (field-plan)
  ((selector :initarg :selector)   ;; form yielding the selector
   (branches :initarg :branches))) ;; ((keys . plan) …), keys or OTHERWISE

(defgeneric known-count (rule)
  (:documentation "The number of data elements RULE yields, as a form, when it is known
before reading; otherwise NIL.")
  (:method ((r stop-rule)) nil)
  (:method ((r count-rule)) (rule-n r)))

(defgeneric known-extent (object)
  (:documentation "For a stop rule, the number of elements it consumes, as a form, when that
is fixed before reading. For a field plan, the number of units it consumes, when that is a
constant. Otherwise NIL.")
  (:method ((r stop-rule)) nil)
  (:method ((r count-rule)) (rule-n r))
  (:method ((r bounded-terminator-rule)) (rule-n r))
  (:method ((p field-plan)) nil)
  (:method ((p scalar-plan)) (slot-value p 'width))
  (:method ((p scan-plan))
    (let ((x (known-extent (slot-value p 'rule))))
      (and (integerp x) (not (slot-value p 'element)) (* x (slot-value p 'width)))))
  (:method ((p case-plan))
    (let ((ws (mapcar (lambda (b) (known-extent (rest b))) (slot-value p 'branches))))
      (and (first ws) (every (lambda (w) (eql w (first ws))) ws) (first ws)))))

(defgeneric stop-effector (rule ctx)
  (:documentation "Read-side fragments for RULE, given CTX (:element, :index, :position,
:count and :extent, the last two variables bound only when known): :bindings, :declare,
:before (stop before reading when true), :after (evaluated after each element is read),
:keep (T when the element that stops the scan is data, NIL when it is a delimiter, or
:DYNAMIC when :after returns NIL, :KEEP or :DROP), and :trailer (a form giving how many
elements already collected to drop at the end)."))

(defgeneric read-effector (plan ctx))  ;; form that returns the value and advances the cursor

(defgeneric write-effector (plan ctx)) ;; form that writes the :value in ctx

(defmethod stop-effector ((r count-rule) ctx)
  (list :before `(>= ,(getf ctx :index) ,(getf ctx :count))))

(defun pattern-matcher (codes)
  "Fragments that match a terminator of several elements, CODES, against the elements
fed to it: :bindings, :declare, :pattern (a variable holding the codes as a vector) and
:feed, an expansion-time function of an element form, returning a form that records the
element and is true when the last elements recorded are the whole pattern."
  (let ((p (length codes)) (pattern (gensym "PAT")) (window (gensym "WIN"))
        (seen (gensym "SEEN")) (j (gensym "J")))
    (list :bindings `((,pattern (vector ,@codes)) (,window (make-array ,p)) (,seen 0))
          :declare `((type fixnum ,seen))
          :pattern pattern
          :feed (lambda (e)
                  ;; the window is a ring; after SEEN elements, the oldest is at (mod SEEN P)
                  `(progn (setf (aref ,window (mod ,seen ,p)) ,e)
                          (incf ,seen)
                          (and (>= ,seen ,p)
                               (loop :for ,j :below ,p
                                     :always (eql (aref ,window (mod (+ ,seen ,j) ,p))
                                                  (aref ,pattern ,j)))))))))

(defmethod stop-effector ((r terminator-rule) ctx)
  (let ((codes (rule-codes r)) (e (getf ctx :element)))
    (if (null (rest codes))
        (let ((tm (gensym "TM")))
          (list :bindings `((,tm ,(first codes)))
                :after `(eql ,e ,tm) :keep nil))
        ;; the elements of the pattern before its last were collected as data; drop them
        ;; when the pattern is found, but not when a bounded scan stops at its count
        (destructuring-bind (&key bindings declare feed &allow-other-keys) (pattern-matcher codes)
          (let ((hit (gensym "HIT")))
            (list :bindings `(,@bindings (,hit nil)) :declare declare
                  :after `(setf ,hit ,(funcall feed e)) :keep nil
                  :trailer `(if ,hit ,(1- (length codes)) 0)))))))

(defmethod stop-effector ((r bounded-terminator-rule) ctx)
  ;; stop at the terminator or at the buffer's end; emit-scan moves past the whole buffer
  (list* :before `(>= ,(getf ctx :index) ,(getf ctx :extent)) (call-next-method)))

(defmethod stop-effector ((r continuation-rule) ctx)
  (list :after `(not (logbitp ,(rule-bit r) ,(getf ctx :element))) :keep t))

;;; The runtime protocol. A stop rule defined by a user with only these methods works at
;;; once: the default effectors below emit calls to them on a fresh instance per field.
;;; Defining stop-effector and frame-effector methods for the class later replaces the
;;; calls with inline code, with no change to the manifests that use it.

(defgeneric done-p (rule)
  (:documentation "Before each element is read: true when the field is complete.")
  (:method ((r stop-rule)) nil))

(defgeneric feed (rule element)
  (:documentation "After ELEMENT is read: NIL to continue, :KEEP to stop with ELEMENT as
data, or :DROP to stop with ELEMENT as a delimiter.")
  (:method ((r stop-rule) element) (declare (ignore element)) nil))

(defgeneric trailer (rule)
  (:documentation "After the scan: how many of the elements collected as data to drop.")
  (:method ((r stop-rule)) 0))

(defgeneric frame-element (rule element last-p)
  (:documentation "On write: the code to write for ELEMENT, the last data element when
LAST-P is true.")
  (:method ((r stop-rule) element last-p) (declare (ignore last-p)) element))

(defgeneric frame-trailer (rule)
  (:documentation "On write: a list of codes to write after the data.")
  (:method ((r stop-rule)) nil))

(defun runtime-rule-form (r)
  "A form making a fresh instance of R's class with R's constant initargs."
  `(make-instance ',(class-name (class-of r)) ,@(mapcar (lambda (x) `',x) (rule-initargs r))))

(defmethod stop-effector ((r stop-rule) ctx)
  (let ((s (gensym "RULE")))
    (list :bindings `((,s ,(runtime-rule-form r)))
          :before `(done-p ,s)
          :after `(feed ,s ,(getf ctx :element))
          :keep :dynamic
          :trailer `(trailer ,s))))

(defclass assembler () ())

(defclass vector-assembler (assembler)
  ((bits :initarg :bits)
   (signed :initarg :signed :initform nil)
   (enum :initarg :enum :initform nil)
   (codec :initarg :codec :initform nil)))

(defclass leb-assembler (assembler)
  ((signed :initarg :signed
           :initform nil)
   (enum   :accessor leb-assembler-enum
           :initarg :enum
           :initform nil)))

(defgeneric assemble-effector (assembler ctx))

(defun codec-form (codec direction)
  `(codec-for ,codec (lambda (in) (error "Failed to ~a input `~a` using ~a: invalid input."
                                         ,direction in ',codec))))

(defun terminator-code-form (name end-by codec)          ; moved from unmarshal, used by both sides
  (typecase end-by
    (character (if codec
                   `(or (funcall ,codec ,end-by)
                        (error "Terminator ~s of field ~s has no code in ~a." ,end-by ',name ',codec))
                   (char-code end-by)))
    (integer end-by)
    (t (error "Field ~s: :end-by must be a character or an integer, not ~s." name end-by))))

(defun terminator-code-forms (name end-by codec)
  "The code forms of a terminator: one for a character or an integer, one per element for
a vector or a string, each encoded by the rule of terminator-code-form."
  (typecase end-by
    ((or character integer) (list (terminator-code-form name end-by codec)))
    (vector (if (zerop (length end-by))
                (error "Field ~s: :end-by is empty." name)
                (map 'list (lambda (x) (terminator-code-form name x codec)) end-by)))
    (t (error "Field ~s: :end-by must be a character, an integer, a vector or a string, not ~s."
              name end-by))))

(defun enum-decode-form (form enum)
  (if enum `(enumerated ,form (gethash ',enum *enums*)) form))

(defmethod assemble-effector ((a vector-assembler) ctx)
  (with-slots (bits signed enum codec) a
    (destructuring-bind (&key name element count trailer &allow-other-keys) ctx
      (let* ((v (gensym "V")) (j (gensym "J"))
             (etype (cond (enum t) ((null bits) 'integer)
                          (signed `(signed-byte ,bits)) (t `(unsigned-byte ,bits))))
             (x (let ((s (if signed
                             `(if (logbitp ,(1- bits) ,element) (- ,element ,(ash 1 bits)) ,element)
                             element)))
                  (if enum `(enumerated ,s (gethash ',enum *enums*)) s))))
        (when (and count trailer)
          (error "Field ~s: a counted field cannot drop trailing elements." name))
        (list :bindings (if count
                            `((,v (make-array ,count :element-type ',etype)) (,j 0))
                            `((,v (make-array 8 :adjustable t :fill-pointer 0))))
              :collect (if count `((setf (aref ,v ,j) ,x) (incf ,j)) `((vector-push-extend ,x ,v)))
              :result (let ((out (cond (count v)
                                       (trailer `(progn (decf (fill-pointer ,v) ,trailer)
                                                        (coerce ,v '(simple-array ,etype (*)))))
                                       (t `(coerce ,v '(simple-array ,etype (*)))))))
                        (if codec `(map 'string ,(codec-form codec "decode") ,out)
                            out)))))))

(defmethod assemble-effector ((a leb-assembler) ctx)
  (with-slots (enum) a
    (let ((v (gensym "V")) (sh (gensym "SH")) (e (getf ctx :element)))
      (list :bindings `((,v 0) (,sh 0))
            :declare `((type unsigned-byte ,v) (type fixnum ,sh))
            :collect `((setf ,v (logior ,v (ash (logand ,e #x7f) ,sh))) (incf ,sh 7))
            :result (enum-decode-form (if (slot-value a 'signed)
                                          `(if (and (plusp ,sh) (logbitp (1- ,sh) ,v)) (- ,v (ash 1 ,sh)) ,v)
                                          v)
                                      enum)))))

(defun emit-scan (name rule assembler read-element cursor src &key width)
  "Return one fused loop that reads, stop-tests and assembles a field, then advances CURSOR.
READ-ELEMENT maps a position variable to a form that reads one raw element there and
advances the variable. WIDTH is the element width in units when it is fixed, else NIL."
  (let* ((e (gensym "E")) (k (gensym "K")) (pos (gensym "POS")) (stop (gensym "STOP"))
         (n (gensym "N")) (x (gensym "X"))
         (count (known-count rule))
         (extent (known-extent rule))
         (extent-var (cond ((null extent) nil) ((eq extent count) n) (t x)))
         (ctx (list :name name :element e :index k :position pos
                    :count (and count n) :extent extent-var))
         (overrun `(error "unmarshal: field ~s runs past the end of the source." ',name)))
    (destructuring-bind (&key bindings declare before after keep trailer) (stop-effector rule ctx)
      (destructuring-bind (&key ((:bindings ab)) ((:declare ad)) collect result)
          (assemble-effector assembler (list* :trailer trailer ctx))
        `(let* ((,pos ,cursor)
                ,@(and count `((,n ,count)))
                ,@(and extent-var (not (eq extent-var n)) `((,x ,extent)))
                (,k 0) ,@bindings ,@ab)
           (declare (type fixnum ,k) ,@declare ,@ad)
           ;; a fixed extent is checked against the source once, before the loop
           ,@(and width extent-var
                  `((when (and (vectorp ,src) (> (+ ,pos (* ,extent-var ,width)) (length ,src)))
                      ,overrun)))
           (loop ,@(and before `(:until ,before))
                 :do ,@(and width (not extent-var)
                            `((when (and (vectorp ,src) (> (+ ,pos ,width) (length ,src)))
                                ,overrun)))
                     (let* ((,e ,(funcall read-element pos))
                            ,@(and after `((,stop ,after))))
                       (incf ,k)
                       ,@(cond ((or (not after) (eq t keep)) collect)
                               ((eq :dynamic keep) `((unless (eq ,stop :drop) ,@collect)))
                               (t `((unless ,stop ,@collect))))
                       ,@(and after `((when ,stop (loop-finish))))))
           ,(if (and width extent-var)
                `(setf ,cursor (+ ,cursor (* ,extent-var ,width)))
                `(setf ,cursor ,pos))
           ,result)))))

(defgeneric frame-effector (rule ctx)
  (:documentation "Write-side fragments for RULE, given CTX (:name, :element, :index, the
number of elements written so far, :value and :write, an expansion-time function of a code
form returning a form that writes it): :bindings, :declare, :prelude (forms run before the
first element), :check (forms run on each data element before it is written), :mark (an
expansion-time function of an element form and a last-p form, returning the form to write),
and :trailer (forms run after the last element)."))

(defmethod frame-effector ((r count-rule) ctx)
  (declare (ignore ctx))
  nil) ;; length vs :count check waits on Plan O5

(defun terminator-frame (r name element write)
  "Write-side fragments shared by the terminator rules: :size (the number of elements in
the terminator), :bindings, :declare, :check (signals an error on data that contains the
terminator), :emit (forms that write the terminator once) and :code-at (an expansion-time
function of an index form, returning a form for the terminator's element at that index,
cycling)."
  (let* ((codes (rule-codes r)) (p (length codes)))
    (if (= 1 p)
        (let ((tm (gensym "TM")))
          (list :size 1 :bindings `((,tm ,(first codes)))
                :check `((when (eql ,element ,tm)
                           (error "Field ~s contains its terminator ~a." ',name ,tm)))
                :emit (list (funcall write tm))
                :code-at (lambda (j) (declare (ignore j)) tm)))
        ;; data and terminator are fed through one matcher, so the pattern must first appear
        ;; at the terminator's last element; earlier, it lies in the data or straddles both
        (destructuring-bind (&key bindings declare pattern feed) (pattern-matcher codes)
          (let ((j (gensym "J")) (c (gensym "C")))
            (list :size p :bindings bindings :declare declare
                  :check `((when ,(funcall feed element)
                             (error "Field ~s contains its terminator ~s." ',name ,pattern)))
                  :emit `((loop :for ,j :below ,p
                                :for ,c := (aref ,pattern ,j)
                                :do (when (and ,(funcall feed c) (< ,j ,(1- p)))
                                      (error "Field ~s: the end of its data and the start of ~
                                              its terminator ~s form the terminator."
                                             ',name ,pattern))
                                    ,(funcall write c)))
                  :code-at (lambda (jf) `(aref ,pattern (mod ,jf ,p)))))))))

(defmethod frame-effector ((r terminator-rule) ctx)
  (destructuring-bind (&key name element write &allow-other-keys) ctx
    (destructuring-bind (&key bindings declare check emit &allow-other-keys)
        (terminator-frame r name element write)
      (list :bindings bindings :declare declare :check check :trailer emit))))

(defmethod frame-effector ((r bounded-terminator-rule) ctx)
  ;; data that fills the buffer needs no terminator; shorter data is followed by the
  ;; terminator, repeated to the buffer's end, and must leave room for a whole one
  (destructuring-bind (&key name element index write &allow-other-keys) ctx
    (unless (rule-n r)
      (error "Field ~s: writing a buffer with a terminator needs an integer :count for now." name))
    (destructuring-bind (&key size bindings declare check emit code-at)
        (terminator-frame r name element write)
      (let ((n (gensym "N")) (room (gensym "ROOM")) (j (gensym "J")))
        (list :bindings `(,@bindings (,n ,(rule-n r))) :declare declare
              :check `((when (>= ,index ,n)
                         (error "Field ~s holds at most ~a elements." ',name ,n))
                       ,@check)
              :trailer `((let ((,room (- ,n ,index)))
                           (when (plusp ,room)
                             ,@(and (> size 1)
                                    `((when (< ,room ,size)
                                        (error "Field ~s leaves room for ~a of the ~a elements ~
                                                of its terminator." ',name ,room ,size))))
                             ,@emit
                             (loop :for ,j :below (- ,room ,size)
                                   :do ,(funcall write (funcall code-at j)))))))))))

(defmethod frame-effector ((r continuation-rule) ctx)
  (declare (ignore ctx))
  (let ((bit (rule-bit r)))
    (list :mark (lambda (e last) `(if ,last ,e (logior ,e ,(ash 1 bit)))))))

(defmethod frame-effector ((r stop-rule) ctx)
  (destructuring-bind (&key write &allow-other-keys) ctx
    (let ((s (gensym "RULE")) (c (gensym "C")))
      (list :bindings `((,s ,(runtime-rule-form r)))
            :mark (lambda (e last) `(frame-element ,s ,e ,last))
            :trailer `((dolist (,c (frame-trailer ,s)) ,(funcall write c)))))))

(defgeneric disassemble-effector (assembler ctx)
  (:documentation "Write-side fragments for ASSEMBLER: :bindings, :declare, :prelude,
:more (true while elements remain, tested before each), :next (yields the next element)
and :last (true when the element just yielded was the final one)."))

(defmethod disassemble-effector ((a leb-assembler) ctx)
  (destructuring-bind (&key name value &allow-other-keys) ctx
    (let ((v (gensym "V")) (g (gensym "G")) (fin (gensym "FIN")))
      (list :bindings `((,v ,value) (,g 0) (,fin nil))
            :declare `((type integer ,v) (type (unsigned-byte 7) ,g))
            :prelude (and (not (slot-value a 'signed))
                          `((when (minusp ,v)
                              (error "Field ~s: ~a is negative, but the LEB is unsigned."
                                     ',name ,v))))
            :more `(not ,fin)
            :next `(progn (setf ,g (logand ,v #x7f) ,v (ash ,v -7)
                                ,fin ,(if (slot-value a 'signed)
                                          `(if (logbitp 6 ,g) (= ,v -1) (zerop ,v))
                                          `(zerop ,v)))
                          ,g)
            :last fin))))

(defmethod disassemble-effector ((a vector-assembler) ctx)
  (with-slots (bits signed enum codec) a
    (destructuring-bind (&key name value &allow-other-keys) ctx
      (let ((src (gensym "SRC")) (j (gensym "J")) (enc (gensym "ENC")) (x (gensym "X")))
        (list :bindings `((,src ,value) (,j 0) ,@(and codec `((,enc ,(codec-form codec "encode")))))
              :declare `((type fixnum ,j))
              :more `(< ,j (length ,src))
              :next `(let ((,x (aref ,src ,j)))
                       (incf ,j)
                       (cond ((characterp ,x) ,(if codec `(funcall ,enc ,x) `(char-code ,x)))
                             ,@(and enum `(((keywordp ,x)
                                            (or (getf (rest (gethash ',enum *enums*)) ,x)
                                                (error "Field ~s: ~s is not in enum ~a." ',name ,x ',enum)))))
                             ,@(and signed `(((minusp ,x)
                                              (when (< ,x ,(- (ash 1 (1- bits))))
                                                (error "Field ~s: ~a does not fit ~a signed bits." ',name ,x ,bits))
                                              (logand ,x ,(1- (ash 1 bits))))))
                             (t ,x)))
              :last `(= ,j (length ,src)))))))

(defun emit-frame (name rule assembler value-form write)
  "Return one loop that splits VALUE-FORM into elements, checks and marks each under
RULE, and writes them with WRITE, an expansion-time function of an element form."
  (let* ((e (gensym "E")) (l (gensym "L")) (k (gensym "K"))
         (ctx (list :name name :element e :index k :value value-form :write write)))
    (destructuring-bind (&key bindings declare prelude check mark trailer) (frame-effector rule ctx)
      (destructuring-bind (&key ((:bindings ab)) ((:declare ad)) ((:prelude ap)) more next last)
          (disassemble-effector assembler ctx)
        `(let* ((,k 0) ,@ab ,@bindings)
           (declare (type fixnum ,k) ,@ad ,@declare)
           ,@ap ,@prelude
           (loop :while ,more
                 :do (let* ((,e ,next) (,l ,last))
                       (declare (ignorable ,l))
                       ,@check
                       ,(funcall write (if mark (funcall mark e l) e))
                       (incf ,k)))
           ,@trailer)))))

(defmethod read-effector ((p scalar-plan) ctx)
  (destructuring-bind (&key read-at cursor &allow-other-keys) ctx
    (with-slots (width signed enum) p
      `(prog1 ,(enum-decode-form (funcall read-at cursor width signed) enum) (incf ,cursor ,width)))))

(defmethod read-effector ((p scan-plan) ctx)
  (destructuring-bind (&key name read-at cursor src &allow-other-keys) ctx
    (with-slots (rule assembler width element) p
      (emit-scan name rule assembler
                 (if element
                     ;; each element is read by its own plan, using the position as its cursor
                     (lambda (pos) (read-effector element (list* :cursor pos ctx)))
                     (lambda (pos) `(prog1 ,(funcall read-at pos width nil) (incf ,pos ,width))))
                 cursor src :width (and (not element) width)))))

(defmethod write-effector ((p scalar-plan) ctx)
  (destructuring-bind (&key value write &allow-other-keys) ctx
    (funcall write (slot-value p 'width) value)))

(defmethod write-effector ((p scan-plan) ctx)
  (destructuring-bind (&key name value write &allow-other-keys) ctx
    (with-slots (rule assembler width element) p
      (emit-frame name rule assembler value
                  (if element
                      (lambda (f) (write-effector element (list* :value f ctx)))
                      (lambda (f) (funcall write width f)))))))

(defun case-clauses (p ctx effector)
  (with-slots (selector branches) p
    (let ((s (gensym "SEL")))
      `(let ((,s ,selector))
         (case ,s
           ,@(loop :for (keys . plan) :in branches
                   :collect `(,(if (eq keys 'otherwise) 'otherwise (if (listp keys) keys (list keys)))
                              ,(funcall effector plan ctx)))
           ,@(unless (assoc 'otherwise branches)
               `((otherwise (error "Field ~s: no branch for selector value ~s."
                                   ',(getf ctx :name) ,s)))))))))

;;; A sub-manifest is expanded by the macro that reads or writes its parent, which keeps the
;;; shared environment and region map; the macro passes the expanding function in CTX, as
;;; :read-manifest or :write-manifest, a function of the sub-manifest's name and CTX.

(defmethod read-effector ((p manifest-plan) ctx)
  (funcall (getf ctx :read-manifest) (plan-manifest p) ctx))

(defmethod write-effector ((p manifest-plan) ctx)
  (funcall (getf ctx :write-manifest) (plan-manifest p) ctx))

(defmethod read-effector  ((p case-plan) ctx) (case-clauses p ctx #'read-effector))

(defmethod write-effector ((p case-plan) ctx) (case-clauses p ctx #'write-effector))

(defun translate-keys (keys enum field)
  "Branch keys as a list of codes, or OTHERWISE. Keyword keys go through ENUM."
  (flet ((code (k)
           (if (keywordp k)
               (or (and enum (getf (rest enum) k))
                   (error "Field ~s: :case key ~s is not in the selector's enum." field k))
               k)))
    (cond ((eq keys 'otherwise) 'otherwise)
          ((listp keys) (mapcar #'code keys))
          (t (list (code keys))))))

(defun check-leb-width (name length unit-bits)
  (unless (and (eql 1 length) (= 8 unit-bits))
    (error "Field ~s: a LEB field must be one 8-bit unit wide." name)))

(defun user-rule (name spec)
  "Make the stop rule a :stop-by key names: a class, or a list of a class and constant
initargs. The class must be defined when the field's code is expanded."
  (destructuring-bind (class &rest initargs) (if (listp spec) spec (list spec))
    (unless (and (symbolp class) (find-class class nil) (subtypep class 'stop-rule))
      (error "Field ~s: :stop-by names ~s, which is not a stop-rule class defined when the ~
              field is expanded." name class))
    (let ((rule (apply #'make-instance class initargs)))
      (setf (rule-initargs rule) initargs)
      rule)))

(defun field-plan (item &key selector-form enum-of count-form (unit-bits 8))
  (destructuring-bind (&key name type actual length signed subtypes count end-by codec enumerate-by
                         stop-by type-indicator type-conditions &allow-other-keys) item
    (when (and stop-by (or end-by count))
      (error "Field ~s: :stop-by cannot be combined with :end-by or :count." name))
    (cond ((eq type :manifest)
           (make-instance 'manifest-plan :manifest (first actual)))
          ((member :case subtypes)
           (let ((enum (funcall enum-of type-indicator)))
             (make-instance 'case-plan
               :selector (funcall selector-form type-indicator name)
               :branches (loop :for b :in type-conditions
                               :collect (cons (translate-keys (getf b :keys) enum name)
                                              ;; a branch inherits the field's :count, :end-by, :codec
                                              (field-plan (append b item)
                                                          :selector-form selector-form :enum-of enum-of
                                                          :count-form count-form :unit-bits unit-bits))))))
          ((member :vec subtypes)
           (let ((leb (member :leb subtypes)))
             (when leb
               (check-leb-width name length unit-bits)
               ;; skipping the rest of a buffer would mean reading elements of unknown width
               (when (and end-by count)
                 (error "Field ~s: a vector of LEB elements cannot be a fixed buffer." name)))
             (make-instance 'scan-plan :width length
               :rule (cond (stop-by (user-rule name stop-by))
                           ((and end-by count)  ; a fixed buffer holding a terminator
                            (make-instance 'bounded-terminator-rule
                                           :codes (terminator-code-forms name end-by codec)
                                           :n (and count-form (funcall count-form item))))
                           (end-by
                            (make-instance 'terminator-rule :codes (terminator-code-forms name end-by codec)))
                           (t (make-instance 'count-rule :n (and count-form (funcall count-form item)))))
               ;; LEB elements are read and written by an inner plan, which handles their sign;
               ;; the vector compares and collects their integer values and decodes the enum
               :element (and leb (make-instance 'scan-plan
                                                :rule (make-instance 'continuation-rule)
                                                :assembler (make-instance 'leb-assembler
                                                                          :signed signed)))
               :assembler (make-instance 'vector-assembler
                                         :bits (and (not leb) (* length unit-bits))
                                         :signed (and (not leb) signed)
                                         :enum enumerate-by :codec codec))))
          ((member :leb subtypes)
           (check-leb-width name length unit-bits)
           (make-instance 'scan-plan :rule (make-instance 'continuation-rule)
                                     :assembler (make-instance 'leb-assembler :signed signed
                                                                              :enum enumerate-by)))
          (t (make-instance 'scalar-plan :width length :signed signed :enum enumerate-by)))))

(defun simplify-plan (p)
  "A case plan whose selector is a constant reduces to its branch at expansion."
  (if (and (typep p 'case-plan) (constantp (slot-value p 'selector)))
      (let ((k (eval (slot-value p 'selector))))
        (or (rest (find-if (lambda (b) (let ((keys (first b)))
                                         (or (eq keys 'otherwise) (eql keys k)
                                             (and (listp keys) (member k keys)))))
                           (slot-value p 'branches)))
            (error "No branch for constant selector ~s." k)))
      p))

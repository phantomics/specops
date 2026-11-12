;;;; manifest.lisp

(in-package #:specops)

(defun codec-for (fn on-error)
  (lambda (x) (or (funcall fn x) (funcall on-error x))))

(defgeneric field-value (record name)
  (:documentation "The value of the field NAME, a keyword, in RECORD, which marshal reads
fields from when it is given (:from RECORD); NIL when RECORD holds no value for it.")
  (:method ((record list) name) (getf record name)))

;; (defun txcodec-ebcdic (a) (specops.map.ebcdic:ebcdic-code-cp037 a))

;; (defun txcodec-ebcdic (a) (specops.map.ebcdic:ebcdic-code-cp037 a))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *manifests* (make-hash-table :test 'eq))
  (defvar *enums*     (make-hash-table :test 'eq)))

(defmacro defenum (name params &body map)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (gethash ',name *enums*)
           ,(append (list 'list (cons 'list params))
                    map))))

(defmacro defmanifest (name params &body fields)
  (destructuring-bind (&key unit endian index vextend-by count-from length pad offset record) params
    (declare (ignore index vextend-by count-from)) ; accepted, but not yet used
    (let* ((unit-spec (typecase unit
                        (integer (if (< unit 4)
                                     ;; the unit may be an exponent of 2 + 3
                                     unit (multiple-value-bind (root remainder) (floor (log unit 2))
                                            (if (zerop remainder) (- root 3)
                                                (error "Invalid unit.")))))
                        (symbol (case unit (:byte 0) (:word 1) (:doubleword 2) (:quadword 3)))))
           ;; the manifest's order of units applies to every field that gives none; big-endian
           ;; when the manifest gives none either
           (order (unit-order endian))
           ;; (:record t) names the record class after the manifest; a symbol names it
           (record-class (if (eq t record) name record))
           (field-specs (list (list 'list :type :config :unit-spec unit-spec
                                    :pad pad :length length
                                    :record (list 'quote record-class)))))

      (labels ((format-slot-spec (spec)
                 ;; quote functions mentioned in slot specs for proper marshal macroexpansion
                 (cons 'list (loop :for item :in spec
                                   :collect (if (not (and (listp item) (eql 'function (first item))))
                                                item (list 'quote item)))))
               (parse-type (predicate)
                 (let (width signed subtype-list type-indicator type-conditions)
                   (typecase predicate
                     (null (setf width 1))
                     (keyword (if (zerop unit-spec)
                                  (case predicate
                                    (:u8  (setf width 1 signed nil))
                                    (:s8  (setf width 1 signed t))
                                    (:u16 (setf width 2 signed nil))
                                    (:s16 (setf width 2 signed t))
                                    (:u32 (setf width 4 signed nil))
                                    (:s32 (setf width 4 signed t))
                                    (:u64 (setf width 8 signed nil))
                                    (:s64 (setf width 8 signed t))
                                    (:f16 (setf width 2 subtype-list '(:float)))
                                    (:f32 (setf width 4 subtype-list '(:float)))
                                    (:f64 (setf width 8 subtype-list '(:float)))
                                    (:f128 (setf width 16 subtype-list '(:float)))
                                    (:str (setf width 1))
                                    ((:u :s) (error "Type ~s gives no width: a field's type is (~s width), ~
                                                     and ~s alone is a masque group's, whose width the ~
                                                     mask gives." predicate predicate predicate))
                                    (t (error "Unknown type ~s." predicate)))
                                  (error "Type symbols are only valid for 8-bit output.")))
                     (list (destructuring-bind (is-signed count &rest subtypes) predicate
                             (if (eq :case is-signed)
                                 (setf type-indicator  count
                                       subtype-list    '(:case)
                                       width           0 ;; variable; the static index can't follow it anyway
                                       type-conditions
                                       (cons 'list
                                             (loop :for (keys . type) :in subtypes
                                                   :collect
                                                   (if (manifest-branch-p type)
                                                       ;; (key (manifest name)): a sub-manifest,
                                                       ;; stored like the manifest directive
                                                       (list 'list :keys (list 'quote keys)
                                                                   :type :manifest
                                                                   :actual (list 'quote (rest (first type))))
                                                       (multiple-value-bind (w s st)
                                                           ;; (0 :u8) and (0 :u 1 :leb) are both allowed
                                                           (parse-type (if (and (null (rest type))
                                                                                (keywordp (first type)))
                                                                           (first type) type))
                                                         (list 'list :keys (list 'quote keys)
                                                                     :length w
                                                                     :signed s
                                                                     :subtypes (list 'quote st)))))))
                                 (setf width        count
                                       subtype-list subtypes))
                             ;; (if (eq :case is-signed)
                             ;;     (setf type-indicator  count
                             ;;           type-conditions
                             ;;           (cons 'list (mapcar (lambda (i) (cons 'list i)) subtypes))
                             ;;           width           1) ;; how to handle right?
                             ;;     (setf width        count
                             ;;           subtype-list subtypes))
                             ;; (print (list :sty subtypes))
                             (when (eq :s is-signed) (setf signed t))
                             ;; (:f width): an IEEE binary float, held as its bit pattern
                             (when (eq :f is-signed) (push :float subtype-list)))))
                   (values width signed subtype-list type-indicator type-conditions)))
               (group-field (spec)
                 ;; a masque group's width and position come from the mask, so its type gives
                 ;; only its signedness: :u or :s alone stand for a group of any width
                 (destructuring-bind (name type &rest options) spec
                   (list* name (case type (:u '(:u 1)) (:s '(:s 1)) (t type)) options)))
               (process-field (field-spec)
                 (destructuring-bind (symbol predicate &key endian enumerate-by end-by
                                                         count default slot codec stop-by place
                                                         &allow-other-keys)
                     field-spec
                   (multiple-value-bind (width signed subtype-list type-indicator type-conditions)
                       (parse-type predicate)
                   (let (;; (width) (signed) (subtype-list) (type-indicator) (type-conditions)
                         (this-order (and endian (unit-order endian))))
                     ;; a width the order cannot group is an error now, not when the field is used
                     (when (and (integerp width) (plusp width))
                       (unit-significances width (or this-order order)))
                     ;; a :case field passes :stop-by to its branches, for its vector branches
                     (when (and stop-by (not (intersection '(:vec :case) subtype-list)))
                       (error "Field ~s: :stop-by applies to :vec fields only." symbol))
                     (list 'list
                           :name (and (not (string= "_" (symbol-name symbol))) symbol)
                           :signed signed :length width
                           :count count :enumerate-by (list 'quote enumerate-by)
                           :codec (list 'quote codec) :slot (format-slot-spec slot)
                           :type-indicator type-indicator :type-conditions type-conditions
                           :subtypes (and subtype-list (cons 'list subtype-list))
                           :end-by end-by :order (list 'quote (or this-order order))
                           :stop-by (list 'quote stop-by) :place place
                           :default default)))))
               (constant-entry (value width)
                 ;; a constant number, written in the manifest's byte order in WIDTH units, or
                 ;; in as few as it needs, and checked on read, as a masque of constant digits is
                 (unless (and (integerp value) (>= value 0))
                   (error "Constant ~s: a constant must be a non-negative integer." value))
                 (let* ((unit-bits (ash 1 (+ 3 unit-spec)))
                        (needed (max 1 (ceiling (integer-length value) unit-bits))))
                   (when width
                     (unless (and (integerp width) (plusp width))
                       (error "Constant #x~x: its width ~s is not a positive integer." value width))
                     (when (< width needed)
                       (error "Constant #x~x needs ~a units of ~a bits, more than its width of ~a."
                              value needed unit-bits width)))
                   (let ((w (or width needed)))
                     (unit-significances w order)
                     (list 'list :type :masque :length w :constant value
                                 :actual (format nil "h:~v,'0X" (/ (* w unit-bits) 4) value)
                                 :order (list 'quote order) :bindings (list 'list)))))
               (process-entry (f accumulator)
                 (cond
                   ;; a bare number: a constant in as few units as it needs
                   ((integerp f) (push (constant-entry f nil) accumulator))
                   ;; (width . value): a constant in WIDTH units
                   ((and (consp f) (integerp (car f)))
                    (unless (integerp (cdr f))
                      (error "Invalid clause ~s: a constant of a given width is (width . value)." f))
                    (push (constant-entry (cdr f) (car f)) accumulator))
                   (t (setf accumulator (process-form f accumulator))))
                 accumulator)
               (process-form (f accumulator)
                 (typecase (first f)
                   (keyword (push (process-field f) accumulator))
                   (symbol (case (intern (symbol-name (first f)) :keyword)
                             (:_ (push (process-field f) accumulator))
                             (:span (let ((sub-items))
                                      (dolist (item (cddr f))
                                        ;; cons the name of the closure to the context list
                                        (setf sub-items (process-entry item sub-items)))
                                      (push (list 'list :type :span :name (second f)
                                                        :items (cons 'list (reverse sub-items)))
                                            accumulator)))
                             (:pad (destructuring-bind (with spec &optional n) (rest f)
                                     (push (list 'list :type :pad :with with
                                                       :mode (if (integerp spec) :count spec)
                                                       ;; :count | :to | :align
                                                       :n    (if (integerp spec) spec n)
                                                       :order (list 'quote order))
                                           accumulator)))
                             (:str (destructuring-bind (actual &key codec encode-by length end-by)
                                       (rest f)
                                     (let ((count (* (length actual) (or length 1))))
                                       ;; strings are read and written in stream order, a unit
                                       ;; to a code; any byte order belongs to the codec
                                       (push (list 'list :type :string :actual actual :order :big
                                                         :end-by end-by :length count
                                                         ;; :index (+ (length actual) (if end-by 1 0))
                                                         :encode-by (list 'quote encode-by)
                                                         :codec (list 'quote codec))
                                             accumulator))))
                             (:masque (destructuring-bind (spec-string &rest bindings) (rest f)
                                        (let ((bindings-out))
                                          (dolist (b bindings)
                                            (push (if (keywordp (second b))
                                                      (let ((field-out (process-field (group-field (rest b)))))
                                                        (setf (getf (rest field-out) :bind-to)
                                                              (list 'quote (first b)))
                                                        field-out)
                                                      (list 'list :binding (list 'quote b)))
                                                  bindings-out))
                                          
                                          (let* ((masque-width
                                                   ;; in bits; a decimal mask counts digits, so its
                                                   ;; width is that of its largest value
                                                   (multiple-value-bind (_ factor __ ___ bits)
                                                       (quantify-mask-string spec-string nil)
                                                     (declare (ignore _ __ ___))
                                                     (if factor (integer-length (1- (expt factor bits))) bits)))
                                                 (unit-width
                                                   (+ (ash masque-width (- (+ 3 unit-spec)))
                                                      (if (zerop (logand masque-width
                                                                         (1- (ash 1 (+ 3 unit-spec)))))
                                                          0 1))))
                                            (unit-significances unit-width order)
                                            (push (list 'list :type :masque :length unit-width
                                                              :actual spec-string :order (list 'quote order)
                                                              :bindings (cons 'list (reverse bindings-out)))
                                                  accumulator)))))
                             ;; :place among the options places the sub-manifest's bytes
                             (:manifest (let* ((options (rest (cddr f)))
                                               (place (getf options :place)))
                                          (push (list 'list :type :manifest :name (second f) :place place
                                                            :actual (list 'quote
                                                                          (cons (third f)
                                                                                (loop :for (k v) :on options
                                                                                      :by #'cddr
                                                                                      :unless (eq k :place)
                                                                                        :append (list k v)))))
                                                accumulator)))
                             ;; (vec unit ...): constant units in stream order, whatever the byte order
                             (:vec (dolist (u (rest f))
                                     (unless (and (integerp u) (<= 0 u (1- (ash 1 (ash 1 (+ 3 unit-spec))))))
                                       (error "vec: element ~s is not an integer that fits one unit of ~a bits."
                                              u (ash 1 (+ 3 unit-spec))))
                                     (push (constant-entry u 1) accumulator)))
                             ;; (place tag :align n): where the fields placed under TAG are written
                             (:place (destructuring-bind (tag &key align) (rest f)
                                       (push (list 'list :type :place :tag tag :align align
                                                         :order (list 'quote order))
                                             accumulator)))))
                   (t (error "Invalid clause; may not start with ~a." (first f))))
                 accumulator))
        
        (dolist (f fields) (setf field-specs (process-entry f field-specs)))

        (when length (push (list 'list :type :pad :with (or pad 0) :offset offset
                                       :mode :to :n length :order (list 'quote order))
                           field-specs))
        
        ;; the record class is defined from the stored fields, so it follows their registration
        `(progn
           (eval-when (:compile-toplevel :load-toplevel :execute)
             (setf (gethash ',name *manifests*)
                   ,(cons 'list (reverse field-specs))))
           ,@(and record-class `((define-manifest-record ,name ,record-class))))))))

(defun manifest-branch-p (type)
  "True when TYPE, the type of a :case branch, is (manifest name), naming a sub-manifest."
  (and (consp type) (null (rest type)) (consp (first type))
       (symbolp (first (first type))) (string= "MANIFEST" (symbol-name (first (first type))))))

(defun manifest-field-names (items)
  "The names of the fields among ITEMS, a manifest's stored fields: span children at the
span's level, keyword groups of masques, and sub-manifest fields; not unnamed fields."
  (loop :for it :in items
        :append (case (getf it :type)
                  (:span   (manifest-field-names (getf it :items)))
                  (:masque (loop :for b :in (getf it :bindings)
                                 :when (getf b :name) :collect (getf b :name)))
                  (t (and (getf it :name) (list (getf it :name)))))))

(defmacro define-manifest-record (manifest class)
  "Define CLASS, the record of the registered MANIFEST: a slot per named field, with an
initarg of the field's keyword and an accessor CLASS-FIELD, a constructor MAKE-CLASS, and a
field-value method. The derived names are interned in CLASS's package."
  (let* ((names (remove-duplicates (manifest-field-names (rest (gethash manifest *manifests*)))))
         (package (symbol-package class))
         (record (gensym "RECORD")) (field (gensym "FIELD")) (stream (gensym "STREAM"))
         (accessors (mapcar (lambda (n) (intern (format nil "~a-~a" (symbol-name class) (symbol-name n))
                                                package))
                            names)))
    `(progn
       (defclass ,class ()
         ,(loop :for n :in names :for a :in accessors
                :collect `(,(intern (symbol-name n) package) :initarg ,n :initform nil :accessor ,a)))
       (defun ,(intern (format nil "MAKE-~a" (symbol-name class)) package) (&rest initargs)
         (apply #'make-instance ',class initargs))
       (defmethod field-value ((,record ,class) ,field)
         (case ,field
           ,@(loop :for n :in names :for a :in accessors :collect `(,n (,a ,record)))
           (t nil)))
       (defmethod print-object ((,record ,class) ,stream)
         (print-unreadable-object (,record ,stream :type t)
           (format ,stream "~{~s~^ ~}"
                   (list ,@(loop :for n :in names :for a :in accessors
                                 :append `(,n (,a ,record)))))))
       ',class)))

(defun walk-manifest (function items)
  "Call FUNCTION on each of ITEMS, a manifest's stored fields, and on the stored fields of
every manifest nested in them: inside spans, as sub-manifest fields and repeats, and as
:case branches, whose code shares the top-level manifest's environment. Each nested
manifest is walked once."
  (let ((visited nil))
    (labels ((child (name)
               (unless (member name visited)
                 (push name visited)
                 (let ((spec (gethash name *manifests*)))
                   (when spec (walk (rest spec))))))
             (walk (items)
               (dolist (item items)
                 (funcall function item)
                 (case (getf item :type)
                   (:span (walk (getf item :items)))
                   (:manifest (child (first (getf item :actual)))))
                 (dolist (b (getf item :type-conditions))
                   (when (eq :manifest (getf b :type))
                     (child (first (getf b :actual))))))))
      (walk items))))

(defun manifest-slots-p (items &optional kind)
  "True when ITEMS, a manifest's stored fields, or a manifest nested in them, has a slot
field: one of KIND, such as :checksum-of, when KIND is given."
  (walk-manifest (lambda (item)
                   (let ((slot (getf item :slot)))
                     (when (and slot (or (null kind) (eq kind (first slot))))
                       (return-from manifest-slots-p t))))
                 items)
  nil)

(defun find-field (items name)
  "The stored field NAME among ITEMS, a manifest's stored fields, inside spans or as a keyword
group of a masque, whose binding is returned; NIL when there is none."
  (find-in-manifest items (lambda (i)
                            (if (eq :masque (getf i :type))
                                (find-if (lambda (b) (and (not (eq :binding (first b)))
                                                          (eq name (getf b :name))))
                                         (getf i :bindings))
                                (and (eq name (getf i :name)) i)))))

(defun manifest-places (items)
  "The tags under which fields are placed in ITEMS, a manifest's stored fields, or in a
manifest nested in them, and the tags of the place directives there, as two values."
  (let ((placed nil) (declared nil))
    (walk-manifest (lambda (item)
                     (if (eq :place (getf item :type))
                         (pushnew (getf item :tag) declared)
                         (when (getf item :place) (pushnew (getf item :place) placed))))
                   items)
    (values placed declared)))

(defun find-in-manifest (spec handler)
  (let ((output))
    (loop :for item :in spec :until output
          :do (setf output (if (eq :span (getf item :type))
                               (find-in-manifest (getf item :items) handler)
                               (funcall handler item))))
    output))

(defun pad-count-form (mode n pos base)
  (ecase mode
    (:count n)
    (:to    (let ((c (gensym "C")))
              `(let ((,c (- (+ ,base ,n) ,pos)))
                 (when (minusp ,c)
                   (error "pad :to ~a overrun: already ~a units into the record." ,n (- ,pos ,base)))
                 ,c)))
    (:align `(mod (- ,n (mod ,pos ,n)) ,n))))          ; absolute position

(defun case-selectors (items)
  (loop :for i :in items
        :append (if (eq :span (getf i :type))
                    (case-selectors (getf i :items))
                    (and (member :case (getf i :subtypes)) (list (getf i :type-indicator))))))

(define-condition manifest-overrun (error)
  ((manifest :initarg :manifest :reader overrun-manifest)
   (size     :initarg :size     :reader overrun-size)
   (position :initarg :position :reader overrun-position))
  (:report (lambda (c stream)
             (format stream "marshal: manifest ~s runs past the end of its destination, ~a units ~
                             long, at unit ~a."
                     (overrun-manifest c) (overrun-size c) (overrun-position c))))
  (:documentation "Signalled by marshal when a vector destination is too small: the unit
at POSITION would lie past SIZE, the vector's allocated size. A fill pointer is neither read
nor set, so a caller that wants a vector to grow sizes it or grows it itself."))

(declaim (ftype (function (t t t) nil) signal-overrun))
(defun signal-overrun (manifest size position)
  (error 'manifest-overrun :manifest manifest :size size :position position))

(defstruct (manifest-buffer (:constructor make-manifest-buffer ()) (:copier nil))
  "A buffer that marshal, given (:buffer B), writes a function or stream destination
through and keeps, grown as needed, so that later calls allocate none."
  (array nil))

(defun reusable-buffer (holder type)
  "The array HOLDER, a manifest-buffer, keeps, when its elements are of TYPE; else NIL."
  (let ((array (manifest-buffer-array holder)))
    (and array (equal (array-element-type array) (upgraded-array-element-type type)) array)))

;;; Patches, (:patch t): a write of the fields given into a record already in a vector, for a
;;; record of fixed size, whose fields are each found at a position fixed at expansion.

(defun item-extent (item unit-spec pos)
  "The units ITEM, a stored field, takes when that is fixed at expansion, POS units into its
manifest; NIL when it varies."
  (destructuring-bind (&key type length count subtypes end-by stop-by actual mode n
                         type-conditions place items &allow-other-keys)
      item
    (cond (place nil)
          ((member type '(:masque)) length)
          ((eq type :string) (+ length (if end-by 1 0)))
          ((eq type :pad) (case mode
                            (:count n)
                            (:to (and (>= n pos) (- n pos)))
                            (t nil)))
          ((eq type :place) nil)
          ((eq type :span)
           (let ((total 0))
             (dolist (i items total)
               (let ((e (item-extent i unit-spec (+ pos total))))
                 (unless e (return nil))
                 (incf total e)))))
          ((eq type :manifest)
           (let* ((options (rest actual))
                  (child (manifest-extent (first actual))))
             (cond ((getf options :until) nil)
                   ((getf options :count) (and (integerp (getf options :count)) child
                                               (* (getf options :count) child)))
                   (t child))))
          ((member :case subtypes)
           (let ((ws (loop :for b :in type-conditions
                           :collect (if (eq :manifest (getf b :type))
                                        (manifest-extent (first (getf b :actual)))
                                        (and (not (intersection '(:vec :leb) (getf b :subtypes)))
                                             (getf b :length))))))
             (and ws (every #'integerp ws) (every (lambda (w) (= w (first ws))) ws) (first ws))))
          ((member :leb subtypes) nil)
          ((member :vec subtypes) (and (integerp count) (not stop-by) (* count length)))
          (t length))))

(defun manifest-layout (name)
  "The extent of each of the stored fields of the manifest NAME, as an alist of fields and
units, and the manifest's size, when every extent is fixed at expansion. Otherwise NIL, and
the first field of varying width."
  (let* ((spec (gethash name *manifests*))
         (unit-spec (getf (first spec) :unit-spec))
         (pos 0) (layout nil))
    (dolist (item (rest spec) (values (reverse layout) pos))
      (let ((e (item-extent item unit-spec pos)))
        (unless e (return (values nil item)))
        (push (cons item e) layout)
        (incf pos e)))))

(defun manifest-extent (name)
  "The size in units of the manifest NAME, when fixed at expansion; otherwise NIL."
  (multiple-value-bind (layout size) (manifest-layout name)
    (and layout size)))

(defun nest-paths (pairs)
  "PAIRS, a marshal call's field pairs, with each pair whose key is a path, a list of field
names, made a nested pair of its first name, merged with any other value given for it."
  (labels ((merge-path (plist path value)
             (let* ((key (first path))
                    (old (getf plist key))
                    (new (if (rest path)
                             (merge-path (and (consp old) (keywordp (first old)) old) (rest path) value)
                             value)))
               (when (and old (not (rest path))
                          (not (and (consp old) (keywordp (first old))
                                    (consp new) (keywordp (first new)))))
                 (error "marshal: field ~s is given twice." key))
               (when (and (consp old) (keywordp (first old)) (consp new) (keywordp (first new))
                          (not (rest path)))
                 (setf new (loop :with merged := (copy-list old)
                                 :for (k v) :on new :by #'cddr
                                 :do (setf merged (merge-path merged (list k) v))
                                 :finally (return merged))))
               (let ((copy (copy-list plist)))
                 (if (member key copy)
                     (setf (getf copy key) new)
                     (setf copy (append copy (list key new))))
                 copy))))
    (loop :with out := nil
          :for (k v) :on pairs :by #'cddr
          :do (setf out (merge-path out (if (consp k) k (list k)) v))
          :finally (return out))))

(defun masque-group-bits (actual char)
  "The width and the position of the group CHAR of the mask ACTUAL, in bits or, for a
decimal mask, in digits, and the mask's factor, 10 for a decimal mask, else NIL."
  (multiple-value-bind (base factor segments symbols) (quantify-mask-string actual nil)
    (declare (ignore base))
    (let ((ix (position (intern (string-upcase char) "KEYWORD") symbols)))
      (unless ix (error "marshal: group ~a not found in masque ~s." char actual))
      (values (- (nth (1+ ix) segments) (nth ix segments)) (nth ix segments) factor))))

(defun checksummed-names (items)
  "The names of the fields of ITEMS, a manifest's stored fields, that lie in a region a
:checksum-of slot covers, at any depth of span."
  (let ((covered (loop :for it :in (manifest-leaves items)
                       :for slot := (getf (first it) :slot)
                       :when (and slot (eq :checksum-of (first slot))) :collect (second slot))))
    (labels ((names-in (item)
               (if (eq :span (getf item :type))
                   (cons (getf item :name) (mapcan #'names-in (getf item :items)))
                   (leaf-names item)))
             (walk (items)
               (loop :for it :in items
                     :append (if (member (getf it :name) covered)
                                 (names-in it)
                                 (and (eq :span (getf it :type)) (walk (getf it :items)))))))
      (walk items))))

(defun locate-field (items name)
  "The stored field among ITEMS, a manifest's stored fields, that is or holds the field NAME,
inside spans or as a keyword group of a masque; its position in units from the manifest's
start; and NAME's binding, when NAME is a masque group. NIL when NAME is not found at a
position fixed at expansion."
  (let ((pos 0))
    (labels ((walk (items)
               (dolist (item items)
                 (let ((binding (and (eq :masque (getf item :type))
                                     (find-if (lambda (b) (and (not (eq :binding (first b)))
                                                               (eq name (getf b :name))))
                                              (getf item :bindings)))))
                   (cond ((or binding (and (eq name (getf item :name))
                                           (not (eq :span (getf item :type)))))
                          (return-from locate-field (values item pos binding)))
                         ((eq :span (getf item :type)) (walk (getf item :items)))
                         (t (let ((e (item-extent item nil pos)))
                              (unless e (return-from locate-field nil))
                              (incf pos e))))))))
      (walk items)
      nil)))

(defun selector-load-form (items sel vector base-form unit-bits)
  "A form loading the code of the :case selector SEL, a field of ITEMS, a manifest's stored
fields, from VECTOR, in the record of that manifest beginning at BASE-FORM."
  (multiple-value-bind (item offset binding) (locate-field items sel)
    (unless item
      (error "marshal: the :case selector ~s is not at a position fixed at expansion." sel))
    (let ((source (make-instance 'vector-source :vector vector))
          (at (if (zerop offset) base-form `(+ ,base-form ,offset))))
      (if binding
          (multiple-value-bind (w p factor) (masque-group-bits (getf item :actual) (getf binding :bind-to))
            (let* ((raw (load-form source (make-encoding (getf item :length) unit-bits
                                                         :order (getf item :order))
                                   at))
                   (group (if factor
                              `(floor (mod ,raw ,(expt factor (+ p w))) ,(expt factor p))
                              `(ldb (byte ,w ,p) ,raw))))
              (if (getf binding :signed)
                  (let ((v (gensym "V")))
                    `(let ((,v ,group)) (if (logbitp ,(1- w) ,v) (- ,v ,(ash 1 w)) ,v)))
                  group)))
          (load-form source (make-encoding (getf item :length) unit-bits :order (getf item :order)
                                           :signed (getf item :signed))
                     at)))))

(defun selector-code-form (enum-name value)
  "VALUE, a selector's value as a patch gives it, as its code through the enum ENUM-NAME."
  (cond ((not enum-name) value)
        ((keywordp value)
         (or (getf (rest (gethash enum-name *enums*)) value)
             (error "~a is not a member of enum ~a." value enum-name)))
        ((constantp value) value)
        (t (let ((v (gensym "V")))
             `(let ((,v ,value))
                (if (keywordp ,v) (getf (rest (gethash ',enum-name *enums*)) ,v) ,v))))))

(defun hoist-patch-selectors (item-lists pairs)
  "PAIRS, a patch's field pairs over the fields of one of the manifests whose stored fields
are ITEM-LISTS, with each selector's value that is a form replaced by a variable, at any
depth of nested pairs; and the bindings of the variables to the forms, in order, so that each
is evaluated once, before the patch checks its branches."
  (let ((bindings nil) (out nil))
    (loop :for (k v) :on pairs :by #'cddr
          :do (let* ((items (find-if (lambda (is) (find-field is k)) item-lists))
                     (item (and items (find-field items k))))
                (cond ((and item (consp v) (keywordp (first v))
                            (or (eq :manifest (getf item :type)) (member :case (getf item :subtypes))))
                       (multiple-value-bind (nested more) (hoist-patch-selectors (field-children item) v)
                         (setf bindings (append bindings more))
                         (push k out) (push nested out)))
                      ((and (not (constantp v))
                            (some (lambda (is) (member k (case-selectors is))) item-lists))
                       (let ((g (gensym (string k))))
                         (setf bindings (append bindings (list (list g v))))
                         (push k out) (push g out)))
                      (t (push k out) (push v out)))))
    (values (nreverse out) bindings)))

(defun patch-branch-checks (manifest items pairs vector base-form unit-bits)
  "Forms checking, before a patch writes, that each key PAIRS give within a :case field of
ITEMS, the stored fields of MANIFEST, is a field of the branch chosen: by the selector's value
in PAIRS, else as the selector stands in VECTOR, in the record beginning at BASE-FORM; at any
depth of nested pairs (SPECOPS-DRAFT-over-arrays-D17)."
  (loop :for (k v) :on pairs :by #'cddr
        :for item := (and (keywordp k) (consp v) (keywordp (first v)) (find-field items k))
        :when item
          :append
          (multiple-value-bind (top offset) (locate-field items k)
            (declare (ignore top))
            (let ((at (if (eql 0 offset) base-form `(+ ,base-form ,offset))))
              (cond ((eq :manifest (getf item :type))
                     (let ((child (first (getf item :actual))))
                       (patch-branch-checks child (rest (gethash child *manifests*)) v vector at
                                            unit-bits)))
                    ((member :case (getf item :subtypes))
                     (let* ((sel (getf item :type-indicator))
                            (enum-name (getf (find-field items sel) :enumerate-by))
                            (enum (and enum-name (gethash enum-name *enums*)))
                            (code (if (loop :for (kk) :on pairs :by #'cddr :thereis (eq kk sel))
                                      (selector-code-form enum-name (getf pairs sel))
                                      (selector-load-form items sel vector base-form unit-bits)))
                            (c (gensym "C"))
                            (branches (getf item :type-conditions)))
                       `((let ((,c ,code))
                           (case ,c
                             ,@(loop :for b :in branches
                                     :for keys := (translate-keys (getf b :keys) enum k)
                                     :collect
                                     `(,keys
                                       ,@(if (eq :manifest (getf b :type))
                                             (let* ((child (first (getf b :actual)))
                                                    (child-items (rest (gethash child *manifests*)))
                                                    (fields (manifest-field-names child-items))
                                                    (absent (loop :for (kk) :on v :by #'cddr
                                                                  :unless (member kk fields) :return kk)))
                                               (if absent
                                                   `((error "marshal: a patch of manifest ~s gives ~s within ~
                                                             ~s, which the branch chosen, manifest ~s, lacks; ~
                                                             that branch was chosen by ~s = ~s."
                                                            ',manifest ',absent ',k ',child ',sel ,c))
                                                   (or (patch-branch-checks child child-items v vector at
                                                                            unit-bits)
                                                       '(nil))))
                                             `((error "marshal: field ~s was given nested values, but its ~
                                                       selector chose a branch that is not a sub-manifest."
                                                      ',k)))))
                             ,@(unless (find 'otherwise branches :key (lambda (b) (getf b :keys)))
                                 `((otherwise (error "Field ~s: no branch for selector value ~s."
                                                     ',k ,c))))))))))))))

(defmacro marshal (name destination &body pairs)
  "Write the fields of the manifest NAME to DESTINATION, a vector, a stream or a function
of one unit, and return the position after the last unit. PAIRS give field values as
keywords and forms, and may begin with a list of options: (:from RECORD) takes every field
not given in PAIRS from RECORD, a plist or a manifest's record, when the code runs, and
(:buffer B) keeps in B, a manifest-buffer, the buffer through which a manifest with slots is
written to a stream or a function, and (:at INDEX) writes the record at INDEX of a vector
destination, returning the position after it; offset slots count from the record's start. A
vector too small signals manifest-overrun. (:patch t) writes only the fields PAIRS give into
a record of fixed size already in a vector, keeping the other units and the other groups of
a masque as they were; a key may then be a path, a list of field names such as (:g1 :ink),
and a sub-manifest's nested pairs write only the fields they give. A :case field is written
in the branch its selector's value in PAIRS chooses, else the selector in the vector. A patch
checks that the record fits before it writes, writes no slot and no field a checksum covers,
and returns the record's end."
  (let* ((options (and (consp (first pairs)) (first pairs)))
         (pairs (if options (rest pairs) pairs))
         ;; a patch's paths, (:g1 :ink) 0, are nested pairs of their first names, :g1 (:ink 0)
         (pairs (progn (when (oddp (length pairs))
                         (error "marshal: the pairs of manifest ~s, ~s, are of odd length~@[; ~s was ~
                                 taken as the options, which come first, and a path is a key of a ~
                                 patch only~]." name pairs options))
                       (unless (getf options :patch)
                         (loop :for (k) :on pairs :by #'cddr
                               :when (consp k)
                                 :do (error "marshal: the path ~s is a key of a patch only; a full ~
                                             write gives a sub-manifest's fields as nested pairs." k)))
                       (if (getf options :patch) (nest-paths pairs) pairs)))
         (from (and (getf options :from) (gensym "REC")))
         (manifest-name name)
         (spec (gethash name *manifests*))
         (params (first spec))
         (base (gensym "BA"))
         (selvars (mapcar (lambda (s) (cons s (gensym (string s))))
                          (case-selectors (rest spec))))
         (nested (getf pairs :-+sub-lexicon+-))
         ;; a manifest with a slot anywhere in it is backpatched, so a function or a stream is
         ;; written through a buffer; the top-level expansion decides
         (backpatching (and (not nested) (not (getf options :patch)) (manifest-slots-p (rest spec))))
         (unit-bits (ash 1 (+ 3 (or (getf params :unit-spec) 0))))
         (unit-type `(unsigned-byte ,unit-bits))
         (holder (and (not nested) (getf options :buffer) (gensym "HOLDER")))
         ;; (:at index): the record begins at INDEX of a vector destination
         (at (and (not nested) (getf options :at)))
         ;; (:patch t): only the fields given are written, into a record of fixed size
         (patch (and (not nested) (getf options :patch) t))
         (origin nil)
         (dest (gensym "DEST")) (in-place (gensym "IN-PLACE")) (limit (gensym "LIMIT"))
         ;; the tags of placed fields, here or nested, when this is the top-level expansion
         (place-tags (and (not nested) (manifest-places (rest spec))))
         regions slots slots-of offset access enter index prefix counts
         values length map b i sub-lexicon written
         ;; where the code being generated stores its units: an entering target, or for a
         ;; record of fixed size in a simple vector, a vector target (SPECOPS-DRAFT-over-arrays-D14)
         target
         ;; whether the fields' spans are mapped, which only slots read
         mapping
         ;; a patch's selector values given as forms, bound once before anything is written
         hoisted
         ;; the record's start, evaluated once whichever path writes it
         start
         ;; the queues of placed fields' writers, by tag, waiting for their place directives
         places)

    (loop :for (key) :on options :by #'cddr
          :unless (member key '(:from :buffer :at :patch))
            :do (error "marshal: unknown option ~s for manifest ~s." key name))

    (when (and (or at patch) (consp destination) (member (first destination) '(function lambda)))
      (error "marshal: ~:[(:at ~s)~;a patch~*~] needs a vector destination, not the function ~s."
             patch at destination))
    (when (and patch (getf options :from))
      (error "marshal: a patch of manifest ~s writes the fields its pairs give; (:from record) ~
              is not yet allowed with (:patch t)." name))

    (unless nested
      (multiple-value-bind (placed declared) (manifest-places (rest spec))
        (dolist (tag placed)
          (unless (member tag declared)
            (error "marshal: fields of manifest ~s are placed under ~s, which no place directive ~
                    declares." name tag)))))

    ;; populate the symbol lexicon or import it from an enclosing marshal macro
    (if (getf pairs :-+sub-lexicon+-)
        (destructuring-bind (&key ac-sym en-sym ba-sym in-sym pr-sym tg-obj
                               vl-sym ln-sym mp-sym ct-sym b-sym i-sym pl-sym or-sym pa-sym)
            (getf pairs :-+sub-lexicon+-)
          (setf access ac-sym enter en-sym base ba-sym index in-sym prefix pr-sym target tg-obj
                values vl-sym length ln-sym map mp-sym counts ct-sym
                b b-sym i i-sym places pl-sym origin or-sym patch pa-sym
                ;; propagate the same lexicon to deeper sub-manifests so nested
                ;; inlining keeps sharing the top-level environment
                sub-lexicon (getf pairs :-+sub-lexicon+-)))
        (setf access (gensym "AC")
              enter (gensym "EN")
              index (gensym "IN")
              values (gensym "VL")
              length (gensym "LN")
              map (gensym "MP")
              counts (gensym "CT")     ; elements written by fields that :count-of slots cover
              b (gensym "BB")
              i (gensym "II")
              places (and place-tags (gensym "PLACES"))
              ;; where the record begins, from which its offset slots count
              origin (gensym "ORIGIN")
              sub-lexicon (list :ac-sym access :en-sym enter :ba-sym base :in-sym index
                                :vl-sym values :ln-sym length
                                :mp-sym map :ct-sym counts :b-sym b :i-sym i
                                :pl-sym places :or-sym origin :pa-sym patch)))
    ;; the spans of fields are mapped only for slots to read, which a patch never writes
    (setf mapping (and (not patch) (manifest-slots-p (rest spec))))


    
    (labels ((preprocess-spec (spec-items)
               (dolist (item spec-items)
                 (destructuring-bind (&key length name (order :big) type slot items &allow-other-keys) item
                   (setf offset (getf item :offset))
                   (push (qualify name) regions)
                   (when (and slot (eq :count-of (first slot)))
                     ;; only a vector or a repeated sub-manifest has elements to count
                     (let ((target (find-in-manifest (rest spec)
                                                     (lambda (i) (and (eq (second slot) (getf i :name)) i)))))
                       (unless (or (member :vec (getf target :subtypes))
                                   (and (eq :manifest (getf target :type))
                                        (let ((options (rest (getf target :actual))))
                                          (or (getf options :count) (getf options :until)))))
                         (error "marshal: the :count-of slot of field ~s covers field ~s, which is ~
                                 not a vector or a repeated sub-manifest." name (second slot)))))
                   (when slot
                     (push item slots)
                     (push (list (qualify name) (first slot) length order)
                           (getf slots-of (qualify (second slot)))))
                   (case type
                     (:span (preprocess-spec items))))))
             (qualify (name)
               (if (not (and prefix name))
                   name (intern (format nil "~a/~a" prefix name) :keyword)))
             (store (order width value-form &key (check t))
               ;; the code storing VALUE-FORM in WIDTH units of ORDER at the target
               (store-form target (make-encoding width unit-bits :order order) value-form
                           :check check))
             (store-at (position-form order width value-form)
               ;; the same, at POSITION-FORM, after which the position is as it was: a slot's
               ;; backpatching, which needs a target that stores where its position is set
               (let ((o (gensym "O")))
                 `(let ((,o ,index))
                    (setf ,index ,position-form)
                    ,(store order width value-form)
                    (setf ,index ,o))))
             (resolve-region (name slot-field slot-type slot-width order)
               (store-at `(second (assoc ',slot-field ,map)) ; the slot's reserved position
                         order slot-width
                         ;; :length-of → (- end start) ; :offset-of → start ;
                         ;; :count-of → the elements written
                         (case slot-type
                           (:length-of `(- (third (assoc ,name ,map))
                                           (second (assoc ,name ,map))))
                           (:offset-of `(- (second (assoc ,name ,map)) ,origin))
                           (:count-of `(rest (assoc ,name ,counts))))))
             (counted-form (qname form)
               ;; FORM writes the field QNAME; when a :count-of slot covers the field, which
               ;; preprocess-spec has checked is a vector or a repeat, record the number of
               ;; elements FORM returns, which the slot is backpatched with
               (if (find :count-of (getf slots-of qname) :key #'second)
                   `(push (cons ,qname ,form) ,counts)
                   form))
             (missing-form (field)
               `(error "marshal: field ~s of manifest ~s has no value, in the record or as a default."
                       ',field ',manifest-name))
             (value-source (field default &optional (from-record t))
               ;; a field's value comes from its literal pair, else from the :from record when
               ;; the code runs, else from its default; slot fields are never read from a record
               (let ((given (getf pairs field)))
                 (cond (given given)
                       ((and from from-record field)
                        `(or (field-value ,from ',field) ,(or default (missing-form field))))
                       (t default))))
             (write-manifest (child ctx)
               ;; expand the sub-manifest CHILD in the shared environment, its fields given by
               ;; literal pairs, then by a record form bound once, when the code runs
               (destructuring-bind (&key name literal record branch-fields selector-value
                                    &allow-other-keys)
                   ctx
                 (when (and patch branch-fields)
                   ;; a patch's keys through a :case field: each must be a field of some branch,
                   ;; and one the branch chosen when the code runs lacks is an error there
                   (let* ((fields (manifest-field-names (rest (gethash child *manifests*))))
                          (keys (loop :for (k) :on literal :by #'cddr :collect k))
                          (unknown (find-if-not (lambda (k) (member k branch-fields)) keys))
                          (absent (find-if-not (lambda (k) (member k fields)) keys)))
                     (when unknown
                       (error "marshal: a patch of manifest ~s gives ~s within ~s, which no branch ~
                               of that field has." manifest-name unknown name))
                     (when absent
                       (return-from write-manifest
                         `(error "marshal: a patch of manifest ~s gives ~s within ~s, which the ~
                                  branch chosen, manifest ~s, lacks~@[; that branch was chosen by ~
                                  ~s = ~s~]."
                                 ',manifest-name ',absent ',name ',child
                                 ,@(let ((sel (getf (find-field (rest spec) name) :type-indicator)))
                                     (if (and sel selector-value)
                                         `(',sel ,selector-value)
                                         '(nil nil))))))))
                 (let* ((child-record (and record (gensym "REC")))
                        (expansion (macroexpand
                                    (append (list 'marshal child destination)
                                            (and child-record `((:from ,child-record)))
                                            (list :-+sub-lexicon+-
                                                  (list* :pr-sym (qualify name) :tg-obj target
                                                         sub-lexicon))
                                            literal))))
                   (if child-record `(let ((,child-record ,record)) ,expansion) expansion))))
             (case-value-context (name default enumerate-by slot plan)
               ;; a :case field's value, for each kind of branch: a sub-manifest branch takes
               ;; literal nested values or a record form; other branches take a value form
               (let* ((given (getf pairs name))
                      (nested (and (consp given) (keywordp (first given)))))
                 (when (and nested (not (typep plan '(or case-plan manifest-plan))))
                   (error "marshal: field ~s was given nested values, but its selector chooses a ~
                           branch that is not a sub-manifest." name))
                 (list :literal (and nested given)
                       :branch-fields (and patch
                                           (remove-duplicates
                                            (loop :for items :in (field-children
                                                                  (find-field (rest spec) name))
                                                  :append (manifest-field-names items))))
                       :record (cond (nested nil)
                                     (given given)
                                     (from `(field-value ,from ',name)))
                       :value (cond (nested
                                     `(error "marshal: field ~s was given nested values, but its ~
                                              selector chose a branch that is not a sub-manifest." ',name))
                                    ((or given default from)
                                     (scalar-value-form name default enumerate-by slot))
                                    (t `(error "marshal: field ~s has no value for a branch that ~
                                                is not a sub-manifest." ',name))))))
             (masque-binder (b)
               (if (eq :binding (first b))
                   (second b)
                   (destructuring-bind (&key name bind-to default enumerate-by &allow-other-keys) b
                     (let ((value (or (enumerate enumerate-by (value-source name default))
                                      (error "Masque field ~a not specified." name)))
                           (selvar (rest (assoc name selvars))))
                       ;; a group may select a :case field's branch, as a field may
                       (push name written)
                       (list bind-to
                             ;; a default goes through the enum like a given value
                             (if selvar `(setf ,selvar ,value) value))))))
             (enumerate (table-name value)
               (cond ((not table-name) value)
                     ((keywordp value)                             ;; literal, resolve now
                      (or (getf (rest (gethash table-name *enums*)) value)
                          (error "~a is not a member of enum ~a." value table-name)))
                     ((or (null value) (constantp value)) value)   ;; nil or a literal number
                     (t (let ((v (gensym "V")))                    ;; runtime form, resolve when it runs
                          `(let ((,v ,value))
                             (if (keywordp ,v) (getf (rest (gethash ',table-name *enums*)) ,v) ,v))))))
             ;; (selector-form (sel)
             ;;   (or (rest (assoc sel raws)) (var-of sel name)))
             (selector-item (sel)
               (or (find-field (rest spec) sel)
                   (error "marshal: :case selector ~s is not a field of ~a." sel name)))
             (selector-form (sel field)
               (let* ((item (selector-item sel))
                      (v (value-source sel (getf item :default))))
                 (cond ((and patch (not (given-p sel)))
                        ;; the selector as it stands in the record, its code loaded directly
                        (selector-load-form (rest spec) sel access base unit-bits))
                       ((not (member sel written))
                        (error "marshal: field ~s needs its :case selector ~s, which must come before it."
                               field sel))
                       ((null v) (error "marshal: :case selector ~s has no value and no default." sel))
                       ((constantp v) (enumerate (getf item :enumerate-by) v)) ;; code known now
                       (t (rest (assoc sel selvars))))))                       ;; set when it was written
             (enum-of (sel)
               (let ((e (getf (find-field (rest spec) sel) :enumerate-by)))
                 (and e (gethash e *enums*))))
             (given-p (key)
               (loop :for (k) :on pairs :by #'cddr :thereis (eq k key)))
             (patch-generate (item pos context)
               ;; a patch's code for ITEM, POS units into the manifest: the item written when it
               ;; is given, its given groups written when it is a masque, and otherwise passed
               ;; over by its extent; returns the code and the extent
               (let ((e (item-extent item (getf params :unit-spec) pos)))
                 (destructuring-bind (&key type name bindings actual (order :big) items slot
                                        &allow-other-keys)
                     item
                   (values
                    (case type
                      (:masque
                       (let ((given (remove-if-not (lambda (b) (and (not (eq :binding (first b)))
                                                                    (getf b :name)
                                                                    (given-p (getf b :name))))
                                                   bindings)))
                         (if given
                             (let ((raw (gensym "RAW")))
                               `((let ((,raw ,(load-form (make-instance 'vector-source :vector access)
                                                         (make-encoding e unit-bits :order order)
                                                         index)))
                                   ,@(loop :for b :in given
                                           :collect (destructuring-bind (&key name bind-to enumerate-by
                                                                          &allow-other-keys)
                                                        b
                                                      (push name written)
                                                      (multiple-value-bind (w p factor)
                                                          (masque-group-bits actual bind-to)
                                                        (let ((v (gensym "V"))
                                                              (selvar (rest (assoc name selvars))))
                                                          `(let ((,v ,(enumerate enumerate-by (getf pairs name))))
                                                             ,@(and selvar `((setf ,selvar ,v)))
                                                             ,(if factor
                                                                  `(progn
                                                                     (unless (and (integerp ,v) (<= 0 ,v ,(1- (expt factor w))))
                                                                       (error "marshal: ~s does not fit group ~a of masque ~s."
                                                                              ,v ',bind-to ,actual))
                                                                     (setf ,raw (+ (- ,raw (* (floor (mod ,raw ,(expt factor (+ p w)))
                                                                                                     ,(expt factor p))
                                                                                              ,(expt factor p)))
                                                                                   (* ,v ,(expt factor p)))))
                                                                  `(progn
                                                                     (unless (and (integerp ,v)
                                                                                  (<= ,(- (ash 1 (1- w))) ,v ,(1- (ash 1 w))))
                                                                       (error "marshal: ~s does not fit group ~a of masque ~s."
                                                                              ,v ',bind-to ,actual))
                                                                     (setf ,raw (dpb ,v (byte ,w ,p) ,raw)))))))))
                                   ,(store order e raw :check nil))))
                             `((incf ,index ,e)))))
                      (:span
                       (let ((forms nil) (p pos))
                         (dolist (i items)
                           (multiple-value-bind (f ie) (patch-generate i p (cons (qualify name) context))
                             (setf forms (append forms f))
                             (incf p ie)))
                         forms))
                      (t
                       (cond ((not (and name (given-p name))) `((incf ,index ,e)))
                             (slot (error "marshal: a patch of manifest ~s cannot write the slot field ~s, ~
                                           which is computed." manifest-name name))
                             ((and (eq type :manifest)
                                   (let ((options (rest actual)))
                                     (or (getf options :count) (getf options :until))))
                              (error "marshal: a patch of manifest ~s cannot write the repeat ~s; paths ~
                                      into the elements of a repeat are not yet supported."
                                     manifest-name name))
                             (t (generate item context)))))
                    e))))
             (vec-source-form (name default count end-by)
               (let ((fallback (cond ((vectorp default) default)
                                     (end-by "")
                                     ((and (integerp default) (integerp count))
                                      ;; built now, a literal in the code
                                      (make-array count :initial-element default)))))
                 (cond ((getf pairs name))
                       ((and from name) `(or (field-value ,from ',name)
                                             ,(or fallback (missing-form name))))
                       (fallback)
                       (t (error "Field ~a: no value, no default, and no integer :count." name)))))
             (scalar-value-form (name default enumerate-by slot)
               ;; a default goes through the enum like a given value; a slot is backpatched
               (let ((value (or (enumerate enumerate-by (value-source name default (not slot)))
                                (and slot 0) (error "Field ~a not specified." name)))
                     (selvar (rest (assoc name selvars))))
                 (push name written)
                 (if selvar `(setf ,selvar ,value) value)))
             (placed-item (name)
               ;; the field NAME of this manifest when it is placed, else NIL
               (find-in-manifest (rest spec) (lambda (i) (and name (eq name (getf i :name))
                                                              (getf i :place) i))))
             (placed-field (item context)
               ;; queue the writing of ITEM, a placed field, under its tag, with the positions
               ;; of the slots over it, which come before it; the place directive writes it and
               ;; backpatches them
               (let* ((name (getf item :name)) (qname (qualify name)) (tag (getf item :place))
                      (entries (getf slots-of qname))
                      (positions (mapcar (lambda (e) (declare (ignore e)) (gensym "AT")) entries))
                      (leaves (mapcar #'first (manifest-leaves (rest spec))))
                      (at (position item leaves))
                      (start (gensym "START")) (end (gensym "END")))
                 (when context
                   (error "marshal: placed field ~s of manifest ~s cannot be inside a span."
                          name manifest-name))
                 (dolist (s slots)
                   (when (eq name (second (getf s :slot)))
                     (unless (member (first (getf s :slot)) '(:length-of :offset-of :checksum-of))
                       (error "marshal: the ~s slot of field ~s cannot cover the placed field ~s."
                              (first (getf s :slot)) (getf s :name) name))
                     (unless (< (position s leaves) at)
                       (error "marshal: the slot field ~s must come before the placed field ~s ~
                               it covers." (getf s :name) name))))
                 ;; the field's own writing, with no slots resolved where it stands
                 (setf (getf slots-of qname) nil)
                 (let ((body (generate (loop :for (k v) :on item :by #'cddr
                                             :unless (eq k :place) :append (list k v)))))
                   (setf (getf slots-of qname) entries)
                   `((let ,(loop :for p :in positions :for e :in entries
                                 :collect `(,p (second (assoc ',(first e) ,map))))
                       (push (lambda ()
                               (let ((,start ,index))
                                 ,@body
                                 (let ((,end ,index))
                                   (declare (ignorable ,end))
                                   ,@(loop :for p :in positions
                                           :for (slot-field kind width order) :in entries
                                           :collect
                                           (store-at p order width
                                                     (ecase kind
                                                       (:length-of `(- ,end ,start))
                                                       (:offset-of `(- ,start ,origin))
                                                       (:checksum-of
                                                        `(funcall ,(getf (cddr (getf (find slot-field slots
                                                                                          :key (lambda (s) (qualify (getf s :name))))
                                                                                    :slot))
                                                                         :by)
                                                                  ,access ,start ,end))))))))
                             (getf ,places ,tag)))))))
             (place-directive (item)
               ;; write the fields queued under the directive's tag, each aligned when asked
               (destructuring-bind (&key tag align (order :big) &allow-other-keys) item
                 (let ((f (gensym "F"))
                       (pad (and align
                                 `(loop :repeat ,(pad-count-form :align align index base)
                                        :do ,(store order 1 0)))))
                   (unless places
                     (error "marshal: manifest ~s has a place directive for ~s, but no field is ~
                             placed under it." manifest-name tag))
                   `(:-place
                     (dolist (,f (reverse (getf ,places ,tag)))
                       ,@(and pad (list pad))
                       (funcall ,f))
                     ,@(and pad (list pad))
                     (setf (getf ,places ,tag) nil)))))
             (generate (item &optional context)
               (destructuring-bind (&key name type (order :big) default count actual bindings with
                                      length end-by codec slot enumerate-by subtypes items mode n
                                      &allow-other-keys) ;; keys only field plans read
                   item
                 (let ((qname (qualify name)))
                   ;; (when context (setf name (intern (format nil "~a/~a" (first context) name)
                   ;;                                  "KEYWORD")))
                   
                   ;; (print (list :nn name context))
                   ;; types: ; :scalar | :pad | :masque | :bytes | :sized | :leb | :name | :slot
                   (when (and (getf item :place) (not (eq type :place)))
                     (return-from generate (placed-field item context)))
                   (case type
                     (:place (place-directive item))
                     (:pad `(:-pad
                             (loop :repeat ,(pad-count-form mode n index base)
                                   :do ,(store order 1 (or with 0)))))
                     (:string
                      (let ((encoder (gensym "EN")) (in (gensym "I")))
                        `(,qname
                          (let ((,encoder ,(and codec
                                                `(codec-for ,codec
                                                            (lambda (,in)
                                                              (error "Failed to encode input `~a` using ~a: ~a"
                                                                     ,in ,(write-to-string codec)
                                                                     "invalid input."))))))
                            (loop :for ch :across ,actual
                                  :do ,(store-code-form target unit-bits
                                                        (if codec `(funcall ,encoder ch) '(char-code ch))))
                            ,@(when end-by (list (store-code-form target unit-bits end-by)))))))
                     (:masque `(:-masque
                                ;; a value too wide for its group is an error, not truncated
                                ,(store order length `(masque ,actual ((:overflow . :error))
                                                              ,@(mapcar #'masque-binder bindings))
                                        :check nil)))
                     (:manifest
                      (append
                       (and mapping `((push (list ,qname ,index ,index) ,map)))
                       (loop :for closure :in context :when mapping
                             :collect `(unless (assoc ,closure ,map)
                                         (push (list ,closure ,index ,index) ,map)))
                       (list name (let ((plan (field-plan item)))
                                    (counted-form
                                     qname
                                     (write-effector
                                      plan
                                      (list* :name name :write-manifest #'write-manifest
                                             (if (typep plan 'manifest-plan)
                                                 ;; literal pairs given for the child win over the
                                                 ;; record's value for this field, read at run time
                                                 (list :literal (getf pairs name)
                                                       :record (and from name
                                                                    `(field-value ,from ',name)))
                                                 ;; a repeat writes a sequence of plists or
                                                 ;; records; none given, or NIL, is no instances
                                                 (list :value (or (getf pairs name)
                                                                  (and from name
                                                                       `(field-value ,from ',name))))))))))
                       (and mapping `((setf (third (assoc ,qname ,map)) ,index)))
                       (loop :for closure :in context :when mapping
                             :collect `(setf (third (assoc ,closure ,map)) ,index))
                       (loop :for slot-of :in (getf slots-of qname)
                             :when (not (eq :checksum-of (second slot-of)))
                               :collect (apply #'resolve-region qname slot-of))))
                     (:span
                      ;; push a fresh per-instance region at the span's start so that
                      ;; repeated span names across sibling sub-manifests don't collide
                      ;; in the shared map (each instance gets its own start/end)
                      (append (and mapping `((push (list ,qname ,index ,index) ,map)))
                              (loop :for i :in items :append (generate i (cons qname context)))
                              (and mapping `((setf (third (assoc ,qname ,map)) ,index)))
                              (loop :for slot-of :in (getf slots-of qname)
                                    :when (not (eq :checksum-of (second slot-of)))
                                      :collect (apply #'resolve-region qname slot-of))))
                     (t (let ((length-form length))
                          ;; (print (list :oo name type-indicator type-conditions))
                          `(,qname
                            ,@(and mapping `((push (list ,qname ,index ,index) ,map)))
                            ,@(loop :for closure :in context :when mapping
                                    :collect `(unless (assoc ,closure ,map)
                                                (push (list ,closure ,index ,index) ,map)))

                            ,(cond
                               ((intersection '(:vec :leb :case :float) subtypes)
                                (let ((plan (simplify-plan
                                             (field-plan item :selector-form #'selector-form
                                                              :enum-of #'enum-of
                                                              ;; on write, a buffer's size is an
                                                              ;; integer :count; a counted vector
                                                              ;; takes its length from its value
                                                              :count-form (lambda (i)
                                                                            (let ((c (getf i :count)))
                                                                              (and (integerp c) c)))
                                                              :unit-bits (ash 1 (+ 3 (getf params
                                                                                           :unit-spec)))))))
                                  (counted-form
                                   qname
                                   (write-effector
                                    plan
                                    (list* :name name
                                           :write (lambda (w f) (store order w f))
                                           :write-manifest #'write-manifest
                                           (cond ((member :case subtypes)
                                                  (case-value-context name default enumerate-by slot plan))
                                                 ((member :vec subtypes)
                                                  (list :value (vec-source-form name default count end-by)))
                                                 (t (list :value (scalar-value-form name default
                                                                                    enumerate-by slot)))))))))
                               ;; ((member :leb subtypes)
                               ;;  (unless (eql 1 length-form)
                               ;;    (error "Field ~s: a LEB field must be one unit wide." name))
                               ;;  (emit-frame name (make-instance 'continuation-rule)
                               ;;              (make-instance 'leb-assembler :signed signed)
                               ;;              (or (enumerate enumerate-by (getf pairs name))
                               ;;                  default (error "Field ~a not specified." name))
                               ;;              (lambda (f) `(funcall (aref ,serializers ,swap-by) (cons 1 ,f)
                               ;;                                    ,enter))))
                               ;; ((member :case subtypes)
                               ;;  (write-effector (simplify-plan (field-plan item #'selector-form #'enum-of))
                               ;;                  (list :name name
                               ;;                        :value (or (enumerate enumerate-by (getf pairs name))
                               ;;                                   default
                               ;;                                   (error "Field ~a not specified." name))
                               ;;                        :write (lambda (w f)
                               ;;                                 `(funcall (aref ,serializers ,swap-by)
                               ;;                                           (cons ,w ,f) ,enter)))))
                               ;; ((member :vec subtypes)
                               ;;  (when (and end-by count)
                               ;;    (error "Field ~a: :count with :end-by is not supported yet." name))
                               ;;  (let* ((encoder (gensym "EN")) (in (gensym "I"))
                               ;;         (term (gensym "TM")) (e (gensym "E")) (c (gensym "C"))
                               ;;         ;; data and characters share one encoding path
                               ;;         (encode (lambda (form)
                               ;;                   (if codec `(funcall ,encoder ,form)
                               ;;                       `(let ((,in ,form))
                               ;;                          (if (characterp ,in) (char-code ,in)
                               ;;                              ,in)))))
                               ;;         ;; a character terminator is encoded like the data;
                               ;;         ;; an integer is a raw code
                               ;;         (term-form
                               ;;           (typecase end-by
                               ;;             (null nil)
                               ;;             (character (funcall encode end-by))
                               ;;             (integer end-by)
                               ;;             (t (error
                               ;;                 "Field ~a: :end-by must be a character or an integer, not ~s."
                               ;;                 name end-by))))
                               ;;         (source (cond ((getf pairs name))
                               ;;                       ((vectorp default) default)
                               ;;                       (end-by "")))
                               ;;         (write-element (lambda (code-form)
                               ;;                          `(funcall (aref ,serializers ,swap-by)
                               ;;                                    (cons ,length-form ,code-form) ,enter))))
                               ;;    `(let* ((,encoder
                               ;;              ,(and codec
                               ;;                    `(codec-for ,codec
                               ;;                                (lambda (,in)
                               ;;                                  (error "Failed to encode input `~a` using ~a: ~a"
                               ;;                                         ,in ,(write-to-string codec)
                               ;;                                         "invalid input.")))))
                               ;;            ,@(and term-form `((,term ,term-form))))
                               ;;       ,(cond
                               ;;          (source
                               ;;           `(loop :for ,e :across ,source
                               ;;                  :for ,c := ,(funcall encode e)
                               ;;                  ,@(and term-form
                               ;;                         `(:when (eql ,c ,term)
                               ;;                            :do (error "Field ~a contains its terminator ~a."
                               ;;                                       ',name ,term)))
                               ;;                  :do ,(funcall write-element c)))
                               ;;          ((integerp count) ;; numeric default, repeated
                               ;;           `(loop :repeat ,count :do ,(funcall write-element default)))
                               ;;          (t (error "Field ~a: no value, no default, and no integer :count."
                               ;;                    name)))
                               ;;       ,@(and term-form (list (funcall write-element term))))))
                               ;; a checksum over a placed field is backpatched where it is placed
                               ((and slot (eq :checksum-of (first slot)) (not (placed-item (second slot))))
                                (store order length-form
                                       `(destructuring-bind (start end)
                                            (rest (assoc ,(qualify (second slot)) ,map))
                                          (funcall ,(getf slot :by) ,access start end))))
                               (t (store order length-form
                                         (scalar-value-form name default enumerate-by slot))))

                            ,@(and mapping `((setf (third (assoc ,qname ,map)) ,index)))
                            ,@(and (getf slots-of qname)
                                   (loop :for slot-of :in (getf slots-of qname)
                                         :when (not (eq :checksum-of (second slot-of)))
                                           :collect (apply #'resolve-region qname slot-of)))
                            ,@(loop :for closure :in context :when mapping
                                    :collect `(setf (third (assoc ,closure ,map)) ,index))
                            ;; (push ,(getf pairs name) ,values)
                            ;; (push ,qname ,values)
                            ;; (print (list :bi ,name ,index ,length))

                            ))))))))

      (preprocess-spec (rest spec))

      (loop :for (key _) :on slots-of :by #'cddr
            :unless (member key regions)
              :do (error "Slot reference found with no matching region."))

      (when patch
        ;; a patch writes no slot, so none is resolved; and it writes only fields of the
        ;; manifest, outside regions a checksum covers
        (setf slots-of nil)
        (let ((names (manifest-field-names (rest spec)))
              (checksummed (checksummed-names (rest spec))))
          (loop :for (k) :on pairs :by #'cddr
                :unless (eq k :-+sub-lexicon+-)
                  :do (unless (member k names)
                        (error "marshal: a patch of manifest ~s gives ~s, which is not one of its ~
                                fields." manifest-name k))
                      (when (member k checksummed)
                        (error "marshal: a patch of manifest ~s cannot write field ~s, which a ~
                                :checksum-of slot covers." manifest-name k))))
        ;; selectors given as forms are evaluated once, before the branches are checked
        (unless nested
          (multiple-value-setq (pairs hoisted) (hoist-patch-selectors (list (rest spec)) pairs))))

      (flet ((body-forms ()
               ;; the fields' code, for the current target
               (setf written nil)
               (cond ((not spec) (error "Manifest not found."))
                     (patch
                      (multiple-value-bind (layout size) (manifest-layout name)
                        (unless layout
                          (error "marshal: a patch needs a manifest of fixed size, and manifest ~s ~
                                  varies in size from field ~s." name (or (getf size :name) (getf size :type))))
                        (append
                         ;; the whole record checked against the vector, and every branch the
                         ;; patch writes through against its keys, before any unit is written
                         (and (not nested)
                              `((let ((,b (+ ,origin ,size)))
                                  (when (> ,b (array-dimension ,access 0))
                                    (signal-overrun ',manifest-name (array-dimension ,access 0) ,b)))))
                         (and (not nested)
                              (patch-branch-checks manifest-name (rest spec) pairs access origin unit-bits))
                         (let ((pos 0) (forms nil))
                           (dolist (item (rest spec) forms)
                             (multiple-value-bind (f e) (patch-generate item pos nil)
                               (setf forms (append forms f))
                               (incf pos e)))))))
                     (t (apply #'append (mapcar #'generate (rest spec))))))
             (record-bindings ()
               ;; the variables the fields' code shares, from the record's start
               `((,index ,start)
                 (,base ,index)
                 (,origin ,index)
                 (,values)
                 ,@(if (getf params :length) `((,length ,(getf params :length))))
                 ,@(mapcar (lambda (p) (list (rest p) nil)) selvars)
                 (,enter) (,map) (,counts))))
        (if nested
            (progn
              (unless target (error "marshal: sub-manifest ~s has no target." name))
              `(let ((,base ,index)
                     ,@(and from `((,from ,(getf options :from))))
                     ,@(mapcar (lambda (p) (list (rest p) nil)) selvars))
                 ,@(body-forms)))
            (let* ((size (manifest-extent name))
                   ;; T4: a record of fixed size, with no slots and no placed fields, is
                   ;; written to a simple vector of its unit type through stores of its own
                   (fast (and size (not backpatching) (not places))))
              (setf start (gensym "START"))
              `(let* ((,access ,destination)
                      ,@(and from `((,from ,(getf options :from))))
                      (,start ,(cond (at `(let ((,i ,at))
                                            (unless (typep ,i '(integer 0))
                                              (error "marshal: (:at ~s) is not an index of ~
                                                      a vector." ,i))
                                            ,i))
                                     (t (or offset 0))))
                      ,@(and holder `((,holder ,(getf options :buffer))))
                      ,@hoisted)
                 ,@(and holder (not backpatching) `((declare (ignore ,holder))))
                 ,(let ((general
                          (progn
                            (setf target (make-instance 'entry-target :enter enter))
                            `(let* (,@(record-bindings)
                                    ,@(and backpatching `((,dest nil)))
                                    ,@(and places `((,places nil))))
                               (declare (ignorable ,base ,origin ,values ,map ,counts))
                               ;; a vector is written in place, with every unit checked against its size
                               (flet ((,in-place (,limit)
                                        (setf ,enter (lambda (,b)
                                                       (when (>= ,index ,limit)
                                                         (signal-overrun ',manifest-name ,limit ,index))
                                                       (setf (aref ,access ,index) ,b)
                                                       (incf ,index)))))
                                 (typecase ,access
                                   ,@(if backpatching
                                         ;; slots are resolved and checksums computed in place only in a
                                         ;; simple array of units, which checksum functions can read;
                                         ;; otherwise in a buffer, passed on once the record is complete
                                         `(((simple-array ,unit-type (*))
                                            (,in-place (array-dimension ,access 0)))
                                           ,@(and (or at patch)
                                                  `(((or function stream)
                                                     (error ,(if patch
                                                                 "marshal: a patch needs a vector destination, not ~*~s."
                                                                 "marshal: (:at ~s) needs a vector destination, not ~s.")
                                                            ,index ,access))))
                                           ((or vector function stream)
                                            (setf ,dest ,access
                                                  ,access (or ,(and holder `(reusable-buffer ,holder ',unit-type))
                                                              (make-array 256 :element-type ',unit-type))
                                                  ,enter (lambda (,b)
                                                           (when (>= ,index (length ,access))
                                                             (setf ,access (grow-buffer ,access ,index)))
                                                           (setf (aref ,access ,index) ,b)
                                                           (incf ,index)))))
                                         `((vector   (,in-place (array-dimension ,access 0)))
                                           ,@(and (or at patch)
                                                  `(((or function stream)
                                                     (error ,(if patch
                                                                 "marshal: a patch needs a vector destination, not ~*~s."
                                                                 "marshal: (:at ~s) needs a vector destination, not ~s.")
                                                            ,index ,access))))
                                           (function (setf ,enter (lambda (,b) (funcall ,access ,b) (incf ,index))))
                                           (stream   (setf ,enter (lambda (,b) (write-byte ,b ,access) (incf ,index))))))
                                   (t (error "marshal: manifest ~s cannot be written to ~s, which is not a vector, ~
                                              a stream or a function." ',manifest-name ,access))))
                               ,@(body-forms)
                               ;; a field placed after the last place directive for its tag was never written
                               ,@(and places
                                      (let ((tag (gensym "TAG")) (queue (gensym "Q")))
                                        `((loop :for (,tag ,queue) :on ,places :by #'cddr
                                                :when ,queue
                                                  :do (error "marshal: fields of manifest ~s placed under ~s come after ~
                                                              its last place directive, so they are not written."
                                                             ',manifest-name ,tag)))))
                               ,@(and backpatching
                                      (let ((u (gensym "U")))
                                        `((when ,dest
                                            (etypecase ,dest
                                              (function (loop :for ,u :from ,base :below ,index
                                                              :do (funcall ,dest (aref ,access ,u))))
                                              (stream (write-sequence ,access ,dest :start ,base :end ,index))
                                              ;; by AREF, which a fill pointer does not bound
                                              (vector (when (> ,index (array-dimension ,dest 0))
                                                        (signal-overrun ',manifest-name (array-dimension ,dest 0)
                                                                        (array-dimension ,dest 0)))
                                                      (loop :for ,u :from ,base :below ,index
                                                            :do (setf (aref ,dest ,u) (aref ,access ,u)))))
                                            ,@(and holder `((when ,holder (setf (manifest-buffer-array ,holder) ,access))))))))
                               ,index))))
                    (if (not fast)
                        general
                        (progn
                          (setf target (make-instance 'vector-target :vector access :index index))
                          `(if (and (typep ,access '(simple-array ,unit-type (*)))
                                    (typep ,start '(integer 0 (,array-dimension-limit))))
                               (let* ((,access ,access) ,@(record-bindings))
                                 (declare (type (simple-array ,unit-type (*)) ,access)
                                          (type (integer 0 (,array-dimension-limit)) ,base ,origin)
                                          (type (integer 0 (,(+ array-dimension-limit size))) ,index)
                                          (ignorable ,base ,origin ,values ,enter ,map ,counts))
                                 ;; one check of the record's extent; an overrun reports the unit
                                 ;; the general path would (SPECOPS-DRAFT-over-arrays-D16)
                                 ,@(and (not patch)
                                        (list (extent-check-form
                                               target size
                                               (lambda (n) `(signal-overrun ',manifest-name ,n (max ,index ,n))))))
                                 ,@(body-forms)
                                 ,index)
                               ,general)))))))))))

(defun enumerated (value form)
  "The keyword that stands for VALUE in the enum FORM, or VALUE itself when the enum names
no such code, so that a record holding a code the enum lacks can be read and written back."
  (loop :for (key val) :on (rest form) :by #'cddr :when (equalp val value) :return key
        :finally (return value)))

(define-condition manifest-check-failed (simple-error)
  ((manifest :initarg :manifest :initform nil :reader check-manifest)
   (field    :initarg :field    :initform nil :reader check-field)
   (kind     :initarg :kind     :initform nil :reader check-kind)
   (expected :initarg :expected :initform nil :reader check-expected)
   (actual   :initarg :actual   :initform nil :reader check-actual)
   (position :initarg :position :initform nil :reader check-position))
  (:documentation "Signalled by unmarshal when what it reads fails a check. KIND is
:checksum, :length, :offset or :count for a slot field, whose value is ACTUAL while the
region it covers gives EXPECTED, or :constant for a constant string, terminator or masque
bits, FIELD then being NIL. POSITION is the unit at which the slot field begins, or for a
constant the first unit that differs: the masque's first unit for masque bits. A CONTINUE
restart accepts the value read; for a slot field, a USE-VALUE restart substitutes another,
such as EXPECTED."))

(defun check-failed (manifest field kind expected actual position substitutable control
                     &rest arguments)
  "Signal manifest-check-failed with CONTROL and ARGUMENTS as its message. Return the
substitute and T when the USE-VALUE restart, offered when SUBSTITUTABLE, is invoked, and
NIL when the CONTINUE restart is."
  (restart-case (error 'manifest-check-failed
                       :manifest manifest :field field :kind kind :expected expected
                       :actual actual :position position
                       :format-control control :format-arguments arguments)
    (continue ()
      :report "Accept the value read and continue reading."
      (values nil nil))
    (use-value (value)
      :test (lambda (c) (declare (ignore c)) substitutable)
      :report "Use another value for the field."
      :interactive (lambda ()
                     (format *query-io* "~&Value to use (evaluated): ")
                     (finish-output *query-io*)
                     (list (eval (read *query-io*))))
      (values value t))))

(defun manifest-regions (items)
  "The region table of ITEMS, a manifest's stored fields, built at expansion. Returns two
values. The first lists the regions, in the order they begin: one per named field and per
span, at any depth of spans, as a plist of :name, :kind (:field, :manifest or :span),
:span (the innermost span holding it, or NIL) and :slots (the slot fields that cover it).
The second lists the slots in field order, as plists of :field, :kind (the slot's kind,
such as :length-of), :region (the name of the region it covers) and :by (a checksum's
function form). Regions inside sub-manifests belong to their own tables."
  (let (regions slots)
    (labels ((walk (items span)
               (dolist (item items)
                 (destructuring-bind (&key name type slot &allow-other-keys) item
                   (when name
                     (push (list :name name
                                 :kind (case type (:span :span) (:manifest :manifest) (t :field))
                                 :span span :slots nil)
                           regions))
                   (when (and name slot)
                     (push (list :field name :kind (first slot) :region (second slot)
                                 :by (getf (cddr slot) :by))
                           slots))
                   (when (eq :span type) (walk (getf item :items) name))))))
      (walk items nil)
      (dolist (r regions)
        (setf (getf r :slots)
              (loop :for s :in slots :when (eq (getf s :region) (getf r :name))
                    :collect (getf s :field))))
      (values (nreverse regions) (nreverse slots)))))

(defun source-units (source unit-spec)
  "SOURCE, an integer or a (units . integer) pair that unmarshal reads, as a vector of its
units, numbered from the most significant as deserializer-for numbers them, so that a
checksum function can read it. A bare integer has as many units as its value needs."
  (let* ((unit (ash 1 (+ 3 unit-spec)))
         (value (if (consp source) (rest source) source))
         (units (if (consp source) (first source) (find-width value unit)))
         (buffer (make-array units :element-type (list 'unsigned-byte unit))))
    (dotimes (i units buffer)
      (setf (aref buffer i) (ldb (byte unit (* unit (- units i 1))) value)))))

(defun manifest-leaves (items)
  "The fields of ITEMS, a manifest's stored fields, in order, with the fields of spans in
place of the spans: a list of (field . top), TOP being the position among ITEMS of the
item that is or holds the field."
  (let ((leaves nil))
    (loop :for item :in items :for top :from 0
          :do (labels ((walk (it)
                         (if (eq :span (getf it :type))
                             (mapc #'walk (getf it :items))
                             (push (cons it top) leaves))))
                (walk item)))
    (nreverse leaves)))

(defun leaf-names (item)
  "The names under which ITEM, a stored field, is read: a masque's keyword groups, or the
field's name."
  (if (eq :masque (getf item :type))
      (loop :for b :in (getf item :bindings) :when (getf b :name) :collect (getf b :name))
      (and (getf item :name) (list (getf item :name)))))

(defun field-dependencies (item mode length-slot)
  "The fields that must be read before ITEM, a stored field, to read it when MODE is :READ
or to pass over it when MODE is :SKIP. LENGTH-SLOT is the field of a :length-of slot over
ITEM read before it, or NIL; a field it covers is passed over by its length."
  (destructuring-bind (&key type subtypes count actual end-by stop-by type-indicator
                       &allow-other-keys)
      item
    (if (and (eq mode :skip) length-slot (not (eq type :pad)))
        (list length-slot)
        (let* ((options (and (eq type :manifest) (rest actual)))
               (count (if (eq type :manifest) (getf options :count) count)))
          (remove nil (list (and (keywordp count) count)
                            (and (member :case subtypes) type-indicator)
                            (and length-slot
                                 (or (eq :length (getf options :until))
                                     (member :case subtypes)
                                     (and (member :vec subtypes) (not count) (not end-by)
                                          (not stop-by))))))))))

(defun field-children (item)
  "The stored fields of the manifests ITEM, a stored field, may hold: a sub-manifest's, or
those of each :case branch that is a sub-manifest. A repeat holds instances, which a path
cannot name, and gives none."
  (flet ((child-items (actual)
           (let ((spec (gethash (first actual) *manifests*)))
             (and spec (rest spec)))))
    (cond ((eq :manifest (getf item :type))
           (let ((options (rest (getf item :actual))))
             (and (not (getf options :count)) (not (getf options :until))
                  (list (child-items (getf item :actual))))))
          ((member :case (getf item :subtypes))
           (loop :for b :in (getf item :type-conditions)
                 :when (eq :manifest (getf b :type))
                   :collect (child-items (getf b :actual)))))))

(defun check-path (items path manifest)
  "Signal an expansion error unless PATH, a list of field names, names a field of ITEMS, a
manifest's stored fields, and each name after the first a field of a manifest the field
before it may hold."
  (labels ((walk (items path)
             (let ((item (find-field items (first path))))
               (cond ((null item) nil)
                     ((null (rest path)) t)
                     (t (some (lambda (child) (walk child (rest path)))
                              (field-children item)))))))
    (unless (and (every #'keywordp path) (walk items path))
      (error "unmarshal: the path ~s names no field of manifest ~s, or of a manifest held by ~
              one; a path names fields, not the elements of a repeat." path manifest))))

(defun branch-name-form (item items value-form)
  "A form giving the name of the manifest of the branch of ITEM, a :case field among ITEMS,
that the selector's value VALUE-FORM chooses, or NIL for a branch that is not a sub-manifest.
A branch's keys match the value as codes or through the selector's enumeration."
  (let* ((sel (find-field items (getf item :type-indicator)))
         (table (and (getf sel :enumerate-by) (rest (gethash (getf sel :enumerate-by) *enums*))))
         (v (gensym "V")))
    (flet ((matches (keys)
             (loop :for k :in (if (listp keys) keys (list keys))
                   :collect k
                   :when (and table (keywordp k)) :collect (getf table k)
                   :when (and table (integerp k))
                     :append (loop :for (name code) :on table :by #'cddr
                                   :when (eql code k) :collect name))))
      `(let ((,v ,value-form))
         (declare (ignorable ,v))
         (cond ,@(loop :for b :in (getf item :type-conditions)
                       :for keys := (getf b :keys)
                       :for name := (and (eq :manifest (getf b :type)) `',(first (getf b :actual)))
                       :collect (if (member keys '(t otherwise))
                                    `(t ,name)
                                    `((member ,v ',(remove nil (matches keys))) ,name))))))))

(defun path-value-form (path head-form manifest items selector-form)
  "A form giving the value at PATH, a list of field names, within HEAD-FORM, the plist of the
field PATH's first name names, among ITEMS, the manifest's stored fields; an error when the
code runs if the branch found there lacks a field the path names, naming the branch and the
selector that chose it. SELECTOR-FORM, a function of a selector's name, gives a form of the
value of a selector of the manifest's own fields."
  (let ((x (gensym "X")) (parent (gensym "P")) (next (gensym "N"))
        (candidates (list items)) (forms nil))
    (loop :for (holder name) :on path
          :for depth :from 0
          :while name
          :do (let* ((item (some (lambda (is) (find-field is holder)) candidates))
                     (holder-items (find-if (lambda (is) (find-field is holder)) candidates))
                     (selector (and item (member :case (getf item :subtypes))
                                    (getf item :type-indicator)))
                     (selector-value (and selector
                                          (if (zerop depth)
                                              (funcall selector-form selector)
                                              `(getf ,parent ',selector)))))
                (push `(let ((,next (let ((tail (and (listp ,x) (nth-value 2 (get-properties ,x '(,name))))))
                                      (if tail
                                          (second tail)
                                          ,(if selector
                                               `(error "unmarshal: the path ~s of manifest ~s names ~s, which ~
                                                        the branch found, ~a, lacks; that branch was chosen by ~
                                                        ~s = ~s." ',path ',manifest ',name
                                                       (let ((b ,(branch-name-form item holder-items selector-value)))
                                                         (if b (format nil "manifest ~s" b)
                                                             "a branch that is not a sub-manifest"))
                                                       ',selector ,selector-value)
                                               `(error "unmarshal: the path ~s of manifest ~s names ~s, which ~
                                                        the field read does not hold." ',path ',manifest ',name))))))
                         (setf ,parent ,x ,x ,next))
                      forms)
                (setf candidates (and item (field-children item)))))
    `(let ((,x ,head-form) (,parent nil))
       (declare (ignorable ,parent))
       ,@(reverse forms)
       ,x)))

(defun manifest-selection (items keys length-sources check-slots &optional offset-sources)
  "What a selective read of KEYS from ITEMS, a manifest's stored fields, does with each
field, worked out at expansion. LENGTH-SOURCES maps a field to the field of the :length-of
slot over it, and OFFSET-SOURCES a placed field to the field of its :offset-of slot, which
is read whether the placed field is read or passed over; CHECK-SLOTS, the manifest's slots
when reads are checked. Returns the modes,
an alist of each field and :READ or :SKIP; the boundary, the position among ITEMS of the
item holding the last key, or -1; and the slots checked, those whose field and region lie
in the items up to the boundary."
  (let* ((leaves (manifest-leaves items))
         (known (loop :for (leaf) :in leaves :append (leaf-names leaf))))
    (dolist (k keys)
      (unless (member k known)
        (error "unmarshal: ~s is not a field of the manifest." k)))
    (labels ((holds (item name)
               (or (member name (leaf-names item)) (eq name (getf item :name))
                   (and (eq :span (getf item :type))
                        (some (lambda (i) (holds i name)) (getf item :items)))))
             (top-of (name)
               ;; the position of the item that is or holds the field or span NAME
               (position-if (lambda (item) (holds item name)) items))
             (position-of (name)
             (position-if (lambda (l) (member name (leaf-names (first l)))) leaves)))
      (let* ((boundary (loop :for (leaf . top) :in leaves
                             :when (intersection keys (leaf-names leaf)) :maximize top
                             :into b :finally (return (or b -1))))
             (checked (loop :for s :in check-slots
                            :when (let ((f (top-of (getf s :field))) (r (top-of (getf s :region))))
                                    (and f r (<= f boundary) (<= r boundary)))
                              :collect s))
             (needed (append keys (mapcar (lambda (s) (getf s :field)) checked)))
             (modes nil))
        (loop :for (leaf) :in (reverse leaves)
              :for at :downfrom (1- (length leaves))
              :do (let* ((mode (if (intersection needed (leaf-names leaf)) :read :skip))
                         (slot (rest (assoc (getf leaf :name) length-sources)))
                         (slot-at (and slot (position-of slot))))
                    (push (cons leaf mode) modes)
                    (setf needed (append (field-dependencies leaf mode (and slot-at (< slot-at at) slot))
                                         (and (getf leaf :place) (not (eq :place (getf leaf :type)))
                                              (list (rest (assoc (getf leaf :name) offset-sources))))
                                         needed))))
        (values modes boundary checked)))))

(defmacro unmarshal (name source &optional params &rest keys)
  "Read the fields of the manifest NAME from SOURCE: a vector, an integer, a (units .
integer) pair, or a stream, which is read to the end of the record and no further. With
KEYS, return their values in order, decoding only those fields and the ones that locate
them, and passing over the rest; a key that names a sub-manifest gives its whole record, and
a key that is a path, a list of field names such as (:g1 :ink), gives the field it names
inside a sub-manifest or a :case branch. Otherwise return every named field, as a plist or,
given (:as :record) in PARAMS, as an instance of the manifest's record class. Checksums,
length, offset and count slots, and constants are checked as they are read, and a failure
signals manifest-check-failed; (:check nil) in PARAMS leaves the checks out. (:buffer B)
keeps in B, a manifest-buffer, the units read from a stream, and (:at INDEX) reads the record
at INDEX of a vector source; offset slots count from the record's start."
  (let* ((spec (gethash name *manifests*))
         (manifest-name name)
         (config (and (eq :config (getf (first spec) :type))
                      (first spec)))
         (nested (getf params :-+sub-lexicon+-))
         (check (getf params :check t))
         (as (getf params :as :plist))
         ;; a nested manifest without a record class is read as a plist inside a record
         (record-class (and (eq as :record) (getf config :record)))
         ;; (all-keys (loop :for item :in (rest spec) :when (getf item :name) :collect (getf item :name)))
         (all-keys (manifest-field-names (rest spec)))
         (length-sources
           (let (acc)
             (labels ((scan (items)
                        (dolist (it items)
                          (let ((slot (getf it :slot)))
                            (when (and slot (eq :length-of (first slot)) (getf it :name))
                              (push (cons (second slot) (getf it :name)) acc)))
                          (when (eq :span (getf it :type)) (scan (getf it :items))))))
               (scan (rest spec)))
             acc))
         ;; (field . offset field): a placed field is read at the offset its :offset-of slot gives
         (offset-sources
           (loop :for (it) :in (manifest-leaves (rest spec))
                 :for slot := (getf it :slot)
                 :when (and slot (eq :offset-of (first slot)) (getf it :name))
                   :collect (cons (second slot) (getf it :name))))
         (in-keys keys)
         ;; the fields a selective read decodes: the keys at the top level, or what a parent
         ;; asks of a nested manifest (NIL to pass over it); :ALL for a full read
         (select (cond (nested (getf params :-+select+- :all))
                       ;; a path is selected by its first name, whose field is read whole
                       (in-keys (remove-duplicates
                                 (mapcar (lambda (k) (if (consp k) (first k) k)) in-keys)
                                 :from-end t))
                       (t :all)))
         (keys (or keys all-keys))
         (base (gensym "BA"))
         ;; a manifest-buffer that a stream source keeps its units in, given (:buffer b)
         (holder (and (not nested) (getf params :buffer) (gensym "HOLDER")))
         ;; (:at index): the record begins at INDEX of a vector source
         (at (and (not nested) (getf params :at)))
         (unit-type `(unsigned-byte ,(ash 1 (+ 3 (or (getf config :unit-spec) 0)))))
         raws sub-lexicon prefix cursor origin
         ;; a buffer of an integer source's units, made at most once, for checksums
         buffer
         ;; a plist of the furthest unit read by fields placed under each tag, for the place
         ;; directive that follows them
         ends)
    
    (loop :for (key) :on params :by #'cddr
          :unless (member key '(:as :check :buffer :at :-+sub-lexicon+- :-+select+-))
            :do (error "unmarshal: unknown option ~s for manifest ~s." key name))
    (unless (member as '(:plist :record))
      (error "unmarshal: :as must be :plist or :record, not ~s." as))
    (when (and (eq as :record) in-keys)
      (error "unmarshal: (:as :record) returns the whole record; it takes no keys."))
    (dolist (k in-keys)
      (when (consp k) (check-path (rest spec) k name)))
    (when (and (eq as :record) (not nested) (not record-class))
      (error "unmarshal: manifest ~s has no record class; give it (:record t)." name))

    (when (and at (or (integerp source)
                      (and (consp source) (eq 'quote (first source)) (consp (second source)))))
      (error "unmarshal: (:at ~s) needs a vector source, not the integer source ~s." at source))

    (if (getf params :-+sub-lexicon+-)
        (destructuring-bind (&key cr-sym pr-sym bf-sym pe-sym or-sym &allow-other-keys)
            (getf params :-+sub-lexicon+-)
          (setf cursor cr-sym prefix pr-sym buffer bf-sym ends pe-sym origin or-sym
                sub-lexicon (getf params :-+sub-lexicon+-)))
        (setf cursor (gensym "CR")
              buffer (and check (manifest-slots-p (rest spec) :checksum-of) (gensym "BUF"))
              ends (and (manifest-places (rest spec)) (gensym "ENDS"))
              ;; where the record begins, from which its offset slots count
              origin (gensym "ORIGIN")
              sub-lexicon (list :cr-sym cursor :bf-sym buffer :pe-sym ends
                                :or-sym origin)))
          
    ;; (loop :for item :in (rest spec) :do (setf (getf fields (getf item :name)) item))

    ;; (print (list :ff fields keys))


    (let* ((vars) (reads) (src (gensym "SRC"))
           ;; every unit is loaded through the code builder, from whatever SRC holds
           (units-source (make-instance 'general-source :source src
                                                        :unit-power (or (getf config :unit-spec) 0)))
           (unit-bits (ash 1 (+ 3 (or (getf config :unit-spec) 0))))
          ;; for checked reads: (region start end), the variables recording where a region a
          ;; slot covers begins and ends, each made only when a check uses it; (field . at),
          ;; where each checked slot field begins; the checks not yet emitted; and the
          ;; names of the fields read and regions closed so far
          (region-vars) (slot-starts) (pending) (done)
          ;; for selective reads: each field's mode, :READ or :SKIP, and the position of the
          ;; item holding the last field asked for
          (modes) (boundary -1)
          ;; the variables holding the cursor while a placed field is read at its offset
          (saves))
      (multiple-value-bind (regions slots) (manifest-regions (rest spec))
        ;; a selective read checks only the slots whose field and region lie in the part read
        (when (listp select)
          (multiple-value-setq (modes boundary slots)
            (manifest-selection (rest spec) select length-sources (and check slots) offset-sources)))
        (when check
          (dolist (s slots)
            (destructuring-bind (&key field kind region by) s
              (unless (find region regions :key (lambda (r) (getf r :name)))
                (error "unmarshal: the ~s slot of field ~s covers ~s, which is not a field or span ~
                        of manifest ~s." kind field region name))
              (unless (member kind '(:length-of :offset-of :count-of :checksum-of))
                (error "unmarshal: field ~s has a slot of unknown kind ~s." field kind))
              (when (and (eq kind :checksum-of) (not by))
                (error "unmarshal: the :checksum-of slot of field ~s gives no function with :by."
                       field))
              ;; a field counted by its :count-of slot's field agrees with it as it is read
              (unless (and (eq kind :count-of)
                           (let ((target (find-in-manifest (rest spec)
                                                           (lambda (i) (and (eq region (getf i :name))
                                                                            i)))))
                             (eq field (if (eq :manifest (getf target :type))
                                           (getf (rest (getf target :actual)) :count)
                                           (getf target :count)))))
                (push s pending)
                (push (cons field (gensym (format nil "~a-AT" (symbol-name field)))) slot-starts)
                (let ((rv (or (assoc region region-vars)
                              (first (push (list region nil nil) region-vars)))))
                  (when (and (not (eq kind :count-of)) (not (second rv)))
                    (setf (second rv) (gensym (format nil "~a-START" (symbol-name region)))))
                  (when (and (member kind '(:length-of :checksum-of)) (not (third rv)))
                    (setf (third rv) (gensym (format nil "~a-END" (symbol-name region)))))))))
          (setf pending (nreverse pending))))
      (labels ((interpret (form enumerate-by)
                 (cond
                   (enumerate-by `(enumerated ,form (gethash ',enumerate-by *enums*)))
                   (t form)))
               (qualify (name)
                 (if (not (and prefix name))
                     name (intern (format nil "~a/~a" prefix name) :keyword)))
               (var-of (field what)
                 ;; WHAT is the name of the field that needs FIELD
                 (or (rest (assoc field vars))
                     (error "unmarshal: field ~s needs field ~s, which isn't read before it." what field)))
               (vec-count-form (name count length)
                 (cond ((integerp count) count)                    ;; :count 4
                       ((keywordp count) (var-of count name))      ;; :count :n-items
                       ((assoc name length-sources)                ;; reverse :length-of
                        `(floor ,(var-of (rest (assoc name length-sources)) name) ,length))
                       (t (error "unmarshal: no element count for vector field ~a ~
                   (give :count, a :length-of slot or :end-by)." name))))
               (read-manifest (child ctx &key (select :all))
                 ;; expand the sub-manifest CHILD at the cursor in CTX, sharing the source, as a
                 ;; record when one is asked for and CHILD has a record class; given SELECT, a
                 ;; list of its fields, read only those, as a plist, and pass over the rest
                 (destructuring-bind (&key name cursor &allow-other-keys) ctx
                   (let ((child-spec (gethash child *manifests*)))
                     (unless child-spec
                       (error "unmarshal: sub-manifest ~a is not defined." child))
                     (unless (eql (getf (first child-spec) :unit-spec) (getf config :unit-spec))
                       (error "unmarshal: sub-manifest ~a uses a different unit than ~a." child name))
                     (macroexpand
                      `(unmarshal ,child ,src
                                  (:-+sub-lexicon+- ,(list* :cr-sym cursor :pr-sym (qualify name)
                                                            sub-lexicon)
                                   ,@(and (eq as :record) (eq select :all) '(:as :record))
                                   ,@(and (listp select) `(:-+select+- ,select))
                                   ,@(and (not check) '(:check nil))))))))
               (skip-manifest (child ctx)
                 ;; pass over the sub-manifest CHILD, reading only what finding its end needs
                 (read-manifest child ctx :select nil))
               (select-manifest (child ctx fields)
                 (read-manifest child ctx :select fields))
               (mode-of (item)
                 ;; :READ to decode ITEM, :SKIP to pass over it
                 (if (listp select) (or (rest (assoc item modes :test #'eq)) :skip) :read))
               (item-plan (item)
                 ;; the field plan of ITEM, a sub-manifest or a :vec, :leb or :case field
                 (if (eq :manifest (getf item :type))
                     (field-plan item :count-form (lambda (i)
                                                    (vec-count-form (getf i :name) (getf i :count) 1))
                                      :length-form #'length-form)
                     (field-plan item :selector-form #'selector-form
                                      :enum-of #'enum-of
                                      :count-form
                                      (lambda (i)
                                        ;; a length slot counts bytes, not elements of varying width
                                        (when (and (member :leb (getf i :subtypes))
                                                   (not (getf i :count)))
                                          (error "unmarshal: vector ~s of LEB elements needs :count, ~
                                                  :end-by or a :length-of slot." (getf i :name)))
                                        (vec-count-form (getf i :name) (getf i :count) (getf i :length)))
                                      :length-form #'length-form
                                      :unit-bits (ash 1 (+ 3 (getf config :unit-spec))))))
               (item-ctx (item)
                 (let ((order (or (getf item :order) :big)))
                   (list :name (getf item :name) :cursor cursor :src src
                         :read-manifest #'read-manifest :skip-manifest #'skip-manifest
                         :select-manifest #'select-manifest
                         :read-at (lambda (at w sg)
                                    (load-form units-source
                                               (make-encoding w unit-bits :order order :signed sg)
                                               at)))))
               (place-form (item)
                 ;; a place directive: move past the furthest unit its placed fields were read
                 ;; to, then align as they were written
                 (destructuring-bind (&key tag align &allow-other-keys) item
                   (unless ends
                     (error "unmarshal: manifest ~s has a place directive for ~s, but no field is ~
                             placed under it." manifest-name tag))
                   `(progn (setf ,cursor (max ,cursor (or (getf ,ends ,tag) ,cursor))
                                 (getf ,ends ,tag) nil)
                           ,@(and align `((incf ,cursor ,(pad-count-form :align align cursor base)))))))
               (skip-item (item)
                 ;; pass over ITEM: by the length a slot read before it gives, by its known
                 ;; width, or by its plan, which reads only what finding its end needs
                 (destructuring-bind (&key name type subtypes length actual end-by mode n
                                      &allow-other-keys)
                     item
                   (let* ((slot (and name (rest (assoc name length-sources))))
                          (len (and slot (not (eq type :pad)) (rest (assoc slot vars)))))
                     (push (cond ((eq type :place) (place-form item))
                                ((eq type :pad) `(incf ,cursor ,(pad-count-form mode n cursor base)))
                                 (len `(incf ,cursor ,len))
                                 ((eq type :string) `(incf ,cursor ,(+ (length actual) (if end-by 1 0))))
                                 ((or (eq type :manifest) (intersection '(:vec :leb :case :float) subtypes))
                                  (skip-effector (item-plan item) (item-ctx item)))
                                 (t `(incf ,cursor ,length)))  ; scalars and masques
                           reads))))
               (length-form (i)
                 ;; the length in units of field I, from a :length-of slot over it, when one is read
                 (let ((source (assoc (getf i :name) length-sources)))
                   (and source (var-of (rest source) (getf i :name)))))
               (record-read (name form)
                 (if name (let ((var (gensym (string name))))
                            (push (cons name var) vars)
                            (push `(setf ,var ,form) reads))
                     (push form reads)))
               (selector-form (sel field)
                 (or (rest (assoc sel raws)) (var-of sel field)))
               (enum-of (sel)
                 (let ((e (getf (find-field (rest spec) sel) :enumerate-by)))
                   (and e (gethash e *enums*))))
               (buffer-form ()
                 ;; what a checksum function reads: the source, the units read from a stream,
                 ;; or the units of an integer
                 (let ((units `(source-units ,src ,(getf config :unit-spec))))
                   `(typecase ,src
                      (vector ,src)
                      (stream-source (stream-source-array ,src))
                      (t ,(if buffer `(or ,buffer (setf ,buffer ,units)) units)))))
               (check-form (s)
                 ;; a form checking the slot S against the region it covers
                 (destructuring-bind (&key field kind region by) s
                   (let* ((var (or (rest (assoc field vars))
                                   (error "unmarshal: slot field ~s of manifest ~s is not read."
                                          field manifest-name)))
                          (at (rest (assoc field slot-starts)))
                          (start (second (assoc region region-vars)))
                          (end (third (assoc region region-vars)))
                          (exp (gensym "EXPECTED")) (v (gensym "V")) (use (gensym "USE")))
                     `(let ((,exp ,(ecase kind
                                     (:length-of `(- ,end ,start))
                                     (:offset-of `(- ,start ,origin))
                                     (:count-of `(length ,(var-of region field)))
                                     (:checksum-of `(funcall ,by ,(buffer-form) ,start ,end)))))
                        (unless (eql ,var ,exp)
                          (multiple-value-bind (,v ,use)
                              (check-failed ',manifest-name ',field
                                            ,(ecase kind (:length-of :length) (:offset-of :offset)
                                                    (:count-of :count) (:checksum-of :checksum))
                                            ,exp ,var ,at t
                                            ,(concatenate
                                              'string "unmarshal: field ~s of manifest ~s, at unit ~a, "
                                              (ecase kind
                                                (:length-of "gives the length ~a, but ~s spans ~a units.")
                                                (:offset-of "gives the offset ~a, but ~s begins at unit ~a.")
                                                (:count-of "gives the count ~a, but ~s has ~a elements.")
                                                (:checksum-of "holds the checksum #x~x, but ~s gives #x~x.")))
                                            ',field ',manifest-name ,at ,var ',region ,exp)
                            (when ,use (setf ,var ,v))))))))
               (emit-ready-checks ()
                 ;; a check is emitted once its slot field is read and its region closed
                 (setf pending
                       (loop :for s :in pending
                             :if (and (member (getf s :field) done) (member (getf s :region) done))
                               :do (push (check-form s) reads)
                             :else :collect s)))
               (constant-failed (expected actual position control &rest arguments)
                 ;; a form signalling a failed constant check, which only CONTINUE answers
                 `(check-failed ',manifest-name nil :constant ,expected ,actual ,position nil
                                ,control ,@arguments))
               (process-item (item)
                 ;; read ITEM, recording where it begins and ends when a check needs it
                 (let* ((rname (getf item :name))
                        (rv (and rname (rest (assoc rname region-vars))))
                        (at (and rname (rest (assoc rname slot-starts))))
                        ;; the tag of a placed field, and where its read returns to
                        (placed (and (not (eq :place (getf item :type))) (getf item :place)))
                        (save (and placed (first (push (gensym "SAVE") saves)))))
                   (when placed
                     ;; a placed field is read where its offset slot says, and the cursor returns
                     (let ((off (rest (assoc rname offset-sources))))
                       (unless off
                         (error "unmarshal: placed field ~s of manifest ~s has no :offset-of slot, ~
                                 so it cannot be found." rname manifest-name))
                       (push `(setf ,save ,cursor ,cursor (+ ,origin ,(var-of off rname))) reads)))
                   (when (first rv) (push `(setf ,(first rv) ,cursor) reads))
                   (when at (push `(setf ,at ,cursor) reads))
                   (if (and (not (eq :span (getf item :type))) (eq :skip (mode-of item)))
                       (skip-item item)
                       (read-item item))
                   (when (second rv) (push `(setf ,(second rv) ,cursor) reads))
                   (when placed
                     (push `(setf (getf ,ends ,placed) (max (or (getf ,ends ,placed) 0) ,cursor)
                                  ,cursor ,save)
                           reads))
                   (when (and rname pending)
                     (push rname done)
                     (emit-ready-checks))))
               (read-item (item)
                 (destructuring-bind (&key name type subtypes length signed (order :big)
                                        items enumerate-by actual codec end-by
                                        bindings mode n &allow-other-keys)
                     item
                   (when nil ;; (member type '(:span)) ;; :manifest))
                     (error "unmarshal: ~a fields not yet supported." type))


                   (case type
                     (:place (push (place-form item) reads))
                    (:pad (push `(incf ,cursor ,(pad-count-form mode n cursor base)) reads))
                     (:manifest
                      (record-read name (read-effector (item-plan item) (item-ctx item))))
                     (:span
                      (loop :for item :in items :do (process-item item)))
                     (:string
                      (let ((i (gensym "I")) (got (gensym "G")) (want (gensym "W")))
                        ;; a mismatch is reported once, at the first unit that differs
                        (when check
                          (push `(loop :for ch :across ,actual :for ,i :from 0
                                       :for ,got := ,(load-form units-source (make-encoding 1 unit-bits)
                                                                `(+ ,cursor ,i))
                                       :for ,want := ,(if codec `(funcall ,codec ch) '(char-code ch))
                                       :unless (eql ,got ,want)
                                         :do ,(constant-failed
                                               want got `(+ ,cursor ,i)
                                               "unmarshal: expected ~s at unit ~a, read #x~x."
                                               actual `(+ ,cursor ,i) got)
                                             (loop-finish))
                                reads)
                          (when end-by
                            (let ((pos `(+ ,cursor ,(length actual))))
                              (push `(let ((,got ,(load-form units-source (make-encoding 1 unit-bits) pos)))
                                       (unless (eql ,end-by ,got)
                                         ,(constant-failed end-by got pos
                                                           "unmarshal: missing terminator after ~s."
                                                           actual)))
                                    reads))))
                        (push `(incf ,cursor ,(+ (length actual) (if end-by 1 0))) reads)))
                     (:masque
                      (multiple-value-bind (base factor segments symbols bits) (quantify-mask-string actual nil)
                        (let ((raw (gensym "MQ"))
                              (checks) (sets)
                              (const-mask (1- (ash 1 bits))))
                          (labels ((group (char)
                                     ;; width and position from the right end of mask group CHAR, in
                                     ;; bits, or in digits for a decimal mask
                                     (let ((ix (position (intern (string-upcase char) "KEYWORD") symbols)))
                                       (unless ix
                                         (error "unmarshal: group ~a not found in masque ~s." char actual))
                                       (values (- (nth (1+ ix) segments) (nth ix segments))
                                               (nth ix segments))))
                                   (extract (w p)
                                     ;; the value of the group of width W at P in the masque read
                                     (if factor
                                         `(floor (mod ,raw ,(expt factor (+ p w))) ,(expt factor p))
                                         `(ldb (byte ,w ,p) ,raw)))
                                   (fit (w form)
                                     ;; FORM reduced to a group of width W, as masque writes it
                                     (if factor `(mod ,form ,(expt factor w)) `(ldb (byte ,w 0) ,form))))
                            ;; bits outside every group are the mask string's constant digits
                            (unless factor
                              (loop :for s :in symbols :for ix :from 0 :when s
                                    :do (setf const-mask
                                              (logandc2 const-mask
                                                        (ash (1- (ash 1 (- (nth (1+ ix) segments)
                                                                           (nth ix segments))))
                                                             (nth ix segments))))))
                            (dolist (b bindings)
                              (if (eq :binding (first b))
                                  ;; static group, e.g. (p +goff-ptv-prefix+): check it against the constant
                                  (destructuring-bind (char form) (second b)
                                    (multiple-value-bind (w p) (group char)
                                      (when check
                                        (push `(unless (= ,(fit w form) ,(extract w p))
                                                 ,(constant-failed
                                                   (fit w form) (extract w p) cursor
                                                   "unmarshal: masque ~s group ~a is ~a, expected ~a."
                                                   actual `',char (extract w p) (fit w form)))
                                              checks))))
                                  ;; keyword group, e.g. (t :kind :u8): read it as a field
                                  (destructuring-bind (&key name bind-to signed enumerate-by &allow-other-keys) b
                                    (when (and signed factor)
                                      (error "unmarshal: group ~a of the decimal masque ~s cannot be signed."
                                             bind-to actual))
                                    (when name
                                      (multiple-value-bind (w p) (group bind-to)
                                        (let* ((var (gensym (string name)))
                                               (v (gensym "V"))
                                               ;; an enumerated :case selector keeps its code as well
                                               (code (and enumerate-by (member name (case-selectors (rest spec)))
                                                          (gensym (format nil "~a-RAW" name))))
                                               (form (if signed
                                                         `(let ((,v ,(extract w p)))
                                                            (if (logbitp ,(1- w) ,v) (- ,v ,(ash 1 w)) ,v))
                                                         (extract w p))))
                                          (push (cons name var) vars)
                                          (when code (push (cons name code) raws))
                                          (push `(setf ,var ,(interpret (if code `(setf ,code ,form) form)
                                                                        enumerate-by))
                                                sets)))))))
                            (push `(let ((,raw ,(load-form units-source
                                                          (make-encoding length unit-bits :order order)
                                                          cursor)))
                                     (declare (ignorable ,raw))
                                     ,@(and check
                                            (if factor
                                                ;; less its groups, a decimal masque is its constant digits
                                                (let ((rest `(- ,raw ,@(loop :for s :in symbols :for ix :from 0
                                                                             :when s
                                                                               :collect (multiple-value-bind (w p)
                                                                                            (values (- (nth (1+ ix) segments)
                                                                                                       (nth ix segments))
                                                                                                    (nth ix segments))
                                                                                          `(* ,(extract w p)
                                                                                              ,(expt factor p)))))))
                                                  `((unless (= ,base ,rest)
                                                      ,(constant-failed
                                                        base rest cursor
                                                        "unmarshal: constant digits of masque ~s don't match (read ~a)."
                                                        actual raw))))
                                                `((unless (= ,base (logand ,const-mask ,raw))
                                                    ,(if (getf item :constant)
                                                         (constant-failed
                                                          base `(logand ,const-mask ,raw) cursor
                                                          "unmarshal: the constant #x~x of manifest ~s doesn't match the units read, #x~x."
                                                          (getf item :constant) `',manifest-name raw)
                                                         (constant-failed
                                                          base `(logand ,const-mask ,raw) cursor
                                                          "unmarshal: constant bits of masque ~s don't match (read #x~x)."
                                                          actual raw))))))
                                     ,@(reverse checks)
                                     ,@(reverse sets))
                                  reads)
                            (push `(incf ,cursor ,length) reads)))))
                     (t
                      (cond
                        ((intersection '(:vec :leb :case :float) subtypes)
                         (record-read name (read-effector (item-plan item) (item-ctx item))))
                        (t
                         (when name
                           (let ((var (gensym (string name))))
                             (push (cons name var) vars)
                             (if (and enumerate-by (member name (case-selectors (rest spec))))
                                 (let ((raw (gensym (format nil "~a-RAW" name))))
                                   (push (cons name raw) raws)
                                   (push `(setf ,var ,(interpret `(setf ,raw ,(load-form units-source
                                                                                         (make-encoding length unit-bits
                                                                                                        :order order :signed signed)
                                                                                         cursor))
                                                                 enumerate-by))
                                         reads))
                                 (push `(setf ,var ,(interpret (load-form units-source
                                                                          (make-encoding length unit-bits
                                                                                         :order order :signed signed)
                                                                          cursor)
                                                               enumerate-by))
                                       reads))))
                         (push (list 'incf cursor length) reads))))))))
        
        ;; a selective read at the top level stops after the item holding its last key; the
        ;; items after it are passed over only when the source is a stream, to take its record
        (let ((tail nil))
          (loop :for item :in (rest spec) :for top :from 0
                :do (if (and (listp select) (not nested) (> top boundary))
                        (push item tail)
                        (process-item item)))
          (when tail
            (let ((main reads))
              (setf reads nil)
              (dolist (item (reverse tail)) (process-item item))
              (setf reads (cons `(when (stream-source-p ,src) ,@(reverse reads)) main)))))
        (when pending
          (error "unmarshal: the slots of fields ~s in manifest ~s cover regions that are not read."
                 (mapcar (lambda (s) (getf s :field)) pending) name))

        ;; (dolist (k keys)
        ;;   (unless (assoc k vars) (error "unmarshal: no field ~a in manifest ~a." k name)))

        `(let* (,@(and holder `((,holder ,(getf params :buffer))))
                ;; at the top level a stream is wrapped, so that its units are kept as read
                (,src ,(cond (nested source)
                             (at (let ((s (gensym "S")))
                                   `(let ((,s ,source))
                                      (unless (vectorp ,s)
                                        (error "unmarshal: (:at ~s) needs a vector source, not ~s."
                                               ,at ,s))
                                      ,s)))
                             (t (let ((s (gensym "S")))
                                  `(let ((,s ,source))
                                     (if (streamp ,s)
                                         (make-stream-source
                                          ,s (or ,(and holder `(reusable-buffer ,holder ',unit-type))
                                                 (make-array 256 :element-type ',unit-type)))
                                         ,s))))))
                ,@(and (not nested)
                       `((,origin ,(if at
                                       (let ((i (gensym "I")))
                                         `(let ((,i ,at))
                                            (unless (typep ,i '(integer 0))
                                              (error "unmarshal: (:at ~s) is not an index of a vector." ,i))
                                            ,i))
                                       0))))
                (,base ,(if (getf params :-+sub-lexicon+-) cursor origin))
                ,@(and (not (getf params :-+sub-lexicon+-))
                       (list (list cursor origin)))
                ,@(and buffer (not nested) `((,buffer nil)))
                ,@(loop :for (nil start end) :in region-vars
                        :when start :collect (list start 0)
                        :when end :collect (list end 0))
                ,@(mapcar (lambda (s) (list (rest s) 0)) slot-starts)
               ,@(and ends (not nested) `((,ends nil)))
               ,@(mapcar (lambda (s) (list s 0)) saves)
                ,@(mapcar #'rest vars)
                ,@(mapcar #'rest raws))
           ,@(reverse reads)
           ;; a stream is read to the end of the record, past trailing pads, so that the next
           ;; record can be read from it
           ,@(and (not nested)
                  `((when (stream-source-p ,src)
                      (fill-stream-source ,src ,cursor)
                      ,@(and holder `((when ,holder
                                        (setf (manifest-buffer-array ,holder)
                                              (stream-source-array ,src))))))))
           ,(cond (in-keys
                   (cons 'values
                         (mapcar (lambda (k)
                                   (if (consp k)
                                       (path-value-form k (rest (assoc (first k) vars)) name (rest spec)
                                                        (lambda (sel) (rest (assoc sel vars))))
                                       (rest (assoc k vars))))
                                 keys)))
                  ;; a nested manifest read in part gives a plist of what was asked of it
                  ((listp select)
                   (cons 'list (loop :for key :in select :append (list key (rest (assoc key vars))))))
                  (record-class
                   `(make-instance ',record-class
                                   ,@(loop :for key :in all-keys :append (list key (rest (assoc key vars))))))
                  (t (cons 'list
                           (loop :for key :in all-keys :append (list key (rest (assoc key vars))))))))))))

#|

;; --- enums (work today) ---
(defenum png-color-type nil :grayscale 0 :rgb 2 :palette 3 :grayscale-alpha 4 :rgba 6)
(defenum png-interlace  nil :none 0 :adam7 1)

;; --- IHDR body, 13 bytes (works today as a standalone marshal) ---
(defmanifest png-ihdr (:unit 8 :endian :big)
  (:width       (:u 4) :default 0)
  (:height      (:u 4) :default 0)
  (:bit-depth   :u8    :default 8)
  (:color-type  :u8    :default 0 :enumerate-by png-color-type)
  (:compression :u8    :default 0)
  (:filter      :u8    :default 0)
  (:interlace   :u8    :default 0 :enumerate-by png-interlace))

;; --- generic chunk: opaque data (span + checksum work; :length-of waits on the patch fix) ---
(defmanifest png-chunk (:unit 8 :endian :big)
  (:length (:u 4) :slot (:length-of :data))                 ; ⚠ patch-before, not yet functional
  (span :type+data
    (:type (:u 1 :vec))                                     ; 4-byte tag, caller-supplied
    (:data (:u 1 :vec)))                                    ; opaque bytes
  (:crc (:u 4) :default 0 :slot (:checksum-of :type+data :by #'crc))) ; ✓ works

;; --- IHDR chunk: body via sub-manifest (waits on sub-manifest fixes #1/#2) ---
(defmanifest png-ihdr-chunk (:unit 8 :endian :big)
  (:length (:u 4) :slot (:length-of :ihdr))                 ; = 13
  (span :type+data
    (:type (:u 1 :vec))                                     ; #(73 72 68 82) "IHDR"
    (manifest :png-ihdr png-ihdr))                          ; ⚠ needs :name + no-let* inlining
  (:crc (:u 4) :default 0 :slot (:checksum-of :type+data :by #'crc)))

;; --- whole file (waits on sub-manifest fixes) ---
(defmanifest png-file (:unit 8 :endian :big)
  (:signature (:u 1 :vec))                                  ; caller passes the 8 magic bytes
  (manifest :ihdr png-ihdr-chunk)
  (manifest :idat png-idat-chunk)                           ; IDAT = opaque zlib blob
  ;; IEND, empty data
  (manifest :iend png-iend-chunk))

(marshal png-ihdr (lambda (i) (print i)) :width 10 :height 10 :bit-depth 8 :color-type :rgb)

Intended call once the fixes land:
(marshal png-ihdr-chunk buf
  :type #(73 72 68 82)
  :png-ihdr (:width 1 :height 1 :bit-depth 8 :color-type :rgb))

(defmanifest stuff (:unit 8 :endian :big)
  (:num :u16 :default 0)
  (:str (:u 1 :vec) :default "     " :count 5 :codec #'specops.map.ebcdic:ebcdic-code-cp037))

(marshal stuff (lambda (a) (print a)) :num 50 :str "Hello")
(unmarshal stuff (make-array 8 :element-type '(unsigned-byte 8) :initial-contents '(0 50 200 133 147 147 150 90))

|#


;; #(55 12 0 0 0 0 5 0 2 0 0 0 0 0 0 0 0 0 0 5 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
;;   0 0 0 0 0 0 0 0 0 0 20 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
;;   0 0 0 0)

;; #(55 12 0 0 0 0 5 0 2 0 0 0 0 0 0 0 0 0 0 5 4 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
;;   0 0 0 0 0 0 0 0 0 0 20 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
;;   0 0 0 0)

;; #(55 12 0 0 0 0 5 0 2 0 0 0 0 0 0 0 0 0 0 5 0 0 0 4 0 0 0 0 0 0 0 0 0 0 0 0 0 0
;;   0 0 0 0 0 0 0 0 0 0 20 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
;;   0 0 0 0)

;; (defmacro unmarshal (name destination &rest keys)
;;   (let ((fn-sym (find-symbol (format nil "⍁UNMARSHAL-COMPOSE-~a" name)
;;                              (package-name (symbol-package name)))))
;;     (if (fboundp fn-sym)
;;         (apply (symbol-function fn-sym) destination keys)
;;         (error "Manifest not found."))))
#|

(macroexpand `(defmanifest png-chunk (:unit 8 :endian :big)
                (:length (:u 4) :slot (:length-of :data))
                (span :type+data
                      (:type (:u 1 :vec))
                      (:data (:u 1 :vec)))
                (:crc (:u 4) :slot (:checksum-of :type+data :by #'crc))))

|#


;; (defmacro make-masquer (string &rest assignments)
;;   (let* ((params (if (listp (caar assignments)) (first assignments) nil))
;;          (assignments (if (not params) assignments (rest assignments)))
;;          (width (rest (assoc :width params)))
;;          (mask  (1- (ash 1 width))))
;;     `(lambda (array offset)
;;        (let ((bound (array-total-size array)))
;;          ,(multiple-value-bind (base segments symbols) (quantify-mask-string string nil)
;;             (print (list :ss base segments symbols))
;;             ;; (format t "~v,'0B~%" 16 (ash (1- (ash 1 length)) (nth index segments)))

;;             (let* ((base-sym 'a)
;;                    (rev-sym (reverse symbols))
;;                    (rev-seg (reverse segments))
;;                    (assign -1)
;;                    (steps (loop :for assignment :in assignments :for sym :in rev-sym
;;                                 :for seg :in rev-seg :for n :from 0
;;                                 :append (destructuring-bind (key value) assignment
;;                                           (let ((index (position (intern (string-upcase key) "KEYWORD")
;;                                                                  symbols))
;;                                                 (seg-length (if (= n (1- (length assignments)))
;;                                                                 (nth n rev-seg)
;;                                                                 (- (nth n rev-seg) (nth (1+ n) rev-seg)))))
;;                                             (when (not index)
;;                                               (error "Symbol ~a not found in mask ~a." key string))
;;                                             (let ((length (- (nth (1+ index) segments)
;;                                                              (nth index segments)))
;;                                                   (divs (/ seg-length width)))
;;                                               (print (list :ee divs length))
;;                                               (if (= 1 divs)
;;                                                   `((setf (aref array offset) ,value)
;;                                                     (bounded-iterate offset bound))
;;                                                   (loop :for d :below divs
;;                                                         :append `((setf (aref array offset)
;;                                                                         (logand (ash ,value ,(- (* 8 d)))
;;                                                                                 ,mask))
;;                                                                   (bounded-iterate offset bound))))))))))
;;               (cond ((not steps) base)
;;                     (t (cons 'progn steps))
;;                     (t `(let ((,base-sym ,base)) ,@steps ,base-sym)))))))))

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
           ;; the manifest's byte order applies to every field that gives none; big-endian
           ;; when the manifest gives none either
           (swap-by (case endian ((nil :big) 0) (:little 1) (:middle 2) (:pdp 2) (t endian)))
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
                                    (:str (setf width 1)))
                                  (error "Type symbols are only valid for 8-bit output.")))
                     (list (destructuring-bind (is-signed count &rest subtypes) predicate
                             (if (eq :case is-signed)
                                 (setf type-indicator  count
                                       subtype-list    '(:case)
                                       width           0 ;; variable; the static index can't follow it anyway
                                       type-conditions
                                       (cons 'list
                                             (loop :for (keys . type) :in subtypes
                                                   :collect (multiple-value-bind (w s st)
                                                                ;; (0 :u8) and (0 :u 1 :leb) are both allowed
                                                                (parse-type (if (and (null (rest type))
                                                                                     (keywordp (first type)))
                                                                                (first type) type))
                                                              (list 'list :keys (list 'quote keys)
                                                                          :length (ash w (- unit-spec))
                                                                          :signed s
                                                                          :subtypes (list 'quote st))))))
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
                             (when (eq :s is-signed) (setf signed t)))))
                   (values width signed subtype-list type-indicator type-conditions)))
               (process-field (field-spec)
                 (destructuring-bind (symbol predicate &key endian enumerate-by end-by
                                                         count default slot codec stop-by
                                                         &allow-other-keys)
                     field-spec
                   (multiple-value-bind (width signed subtype-list type-indicator type-conditions)
                       (parse-type predicate)
                   (let (;; (width) (signed) (subtype-list) (type-indicator) (type-conditions)
                         (this-swap-by (case endian
                                         ((nil) nil) (:big 0) (:little 1) (:middle 2) (:pdp 2)
                                         (t endian))))
                     ;; a :case field passes :stop-by to its branches, for its vector branches
                     (when (and stop-by (not (intersection '(:vec :case) subtype-list)))
                       (error "Field ~s: :stop-by applies to :vec fields only." symbol))
                     (list 'list
                           :name (and (not (string= "_" (symbol-name symbol))) symbol)
                           :signed signed :length (ash width (- unit-spec))
                           :count count :enumerate-by (list 'quote enumerate-by)
                           :codec (list 'quote codec) :slot (format-slot-spec slot)
                           :type-indicator type-indicator :type-conditions type-conditions
                           :subtypes (and subtype-list (cons 'list subtype-list))
                           :end-by end-by :swap-by (or this-swap-by swap-by)
                           :stop-by (list 'quote stop-by)
                           :default default)))))
               (process-entry (f accumulator)
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
                                                       :swap-by swap-by)
                                           accumulator)))
                             (:str (destructuring-bind (actual &key codec encode-by length end-by)
                                       (rest f)
                                     (let ((count (* (length actual) (or length 1))))
                                       ;; strings are read and written in stream order, through
                                       ;; serializer 0; any byte order belongs to the codec
                                       (push (list 'list :type :string :actual actual :swap-by 0
                                                         :end-by end-by :length count
                                                         ;; :index (+ (length actual) (if end-by 1 0))
                                                         :encode-by (list 'quote encode-by)
                                                         :codec (list 'quote codec))
                                             accumulator))))
                             (:masque (destructuring-bind (spec-string &rest bindings) (rest f)
                                        (let ((bindings-out))
                                          (dolist (b bindings)
                                            (push (if (keywordp (second b))
                                                      (let ((field-out (process-field (rest b))))
                                                        (setf (getf (rest field-out) :bind-to)
                                                              (list 'quote (first b)))
                                                        field-out)
                                                      (list 'list :binding (list 'quote b)))
                                                  bindings-out))
                                          
                                          (let* ((masque-width (nth-value 4 (quantify-mask-string
                                                                             spec-string nil)))
                                                 (unit-width
                                                   (+ (ash masque-width (- (+ 3 unit-spec)))
                                                      (if (zerop (logand masque-width
                                                                         (1- (ash 1 (+ 3 unit-spec)))))
                                                          0 1))))
                                            (push (list 'list :type :masque :length unit-width
                                                              :actual spec-string :swap-by swap-by
                                                              :bindings (cons 'list (reverse bindings-out)))
                                                  accumulator)))))
                             (:manifest (push (list 'list :type :manifest :name (second f)
                                                          :actual (list 'quote (cddr f)))
                                              accumulator))))
                   (t (error "Invalid clause; may not start with ~a." (first f))))
                 accumulator))
        
        (dolist (f fields) (setf field-specs (process-entry f field-specs)))

        (when length (push (list 'list :type :pad :with (or pad 0) :offset offset
                                       :mode :to :n length :swap-by swap-by)
                           field-specs))
        
        ;; the record class is defined from the stored fields, so it follows their registration
        `(progn
           (eval-when (:compile-toplevel :load-toplevel :execute)
             (setf (gethash ',name *manifests*)
                   ,(cons 'list (reverse field-specs))))
           ,@(and record-class `((define-manifest-record ,name ,record-class))))))))

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

(defun manifest-swap-specs (items)
  "The byte orders used by ITEMS, a manifest's stored fields, including those inside spans
and nested manifests, whose code shares the top-level manifest's serializers."
  (remove-duplicates
   (loop :for item :in items
         :append (case (getf item :type)
                   (:span (manifest-swap-specs (getf item :items)))
                   (:manifest (let ((child (gethash (first (getf item :actual)) *manifests*)))
                                (and child (manifest-swap-specs (rest child)))))
                   (t (let ((s (getf item :swap-by))) (and s (list s))))))))

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

(defmacro marshal (name destination &body pairs)
  "Write the fields of the manifest NAME to DESTINATION. PAIRS give field values as keywords
and forms, and may begin with a list of options: (:from RECORD) takes every field not given
in PAIRS from RECORD, a plist or a manifest's record, when the code runs."
  (let* ((options (and (consp (first pairs)) (first pairs)))
         (pairs (if options (rest pairs) pairs))
         (from (and (getf options :from) (gensym "REC")))
         (manifest-name name)
         (spec (gethash name *manifests*))
         (params (first spec))
         (base (gensym "BA"))
         (selvars (mapcar (lambda (s) (cons s (gensym (string s))))
                          (case-selectors (rest spec))))
         regions slots slots-of swap-specs offset access enter index tell write prefix
         values length serializers map b i sub-lexicon written)

    (loop :for (key) :on options :by #'cddr
          :unless (member key '(:from))
            :do (error "marshal: unknown option ~s for manifest ~s." key name))

    ;; populate the symbol lexicon or import it from an enclosing marshal macro
    (if (getf pairs :-+sub-lexicon+-)
        (destructuring-bind (&key ac-sym en-sym ba-sym in-sym tl-sym wr-sym pr-sym
                               vl-sym ln-sym sr-sym mp-sym b-sym i-sym)
            (getf pairs :-+sub-lexicon+-)
          (setf access ac-sym enter en-sym base ba-sym index in-sym tell tl-sym write wr-sym prefix pr-sym
                values vl-sym length ln-sym serializers sr-sym map mp-sym b b-sym i i-sym
                ;; propagate the same lexicon to deeper sub-manifests so nested
                ;; inlining keeps sharing the top-level environment
                sub-lexicon (getf pairs :-+sub-lexicon+-)))
        (setf access (gensym "AC")
              enter (gensym "EN")
              index (gensym "IN")
              tell (gensym "TL")
              write (gensym "WR")
              values (gensym "VL")
              length (gensym "LN")
              serializers (gensym "SR")
              map (gensym "MP")
              b (gensym "BB")
              i (gensym "II")
              sub-lexicon (list :ac-sym access :en-sym enter :ba-sym base :in-sym index
                                :tl-sym tell :wr-sym write
                                :vl-sym values :ln-sym length
                                :sr-sym serializers :mp-sym map :b-sym b :i-sym i)))


    
    (labels ((preprocess-spec (spec-items)
               (dolist (item spec-items)
                 (destructuring-bind (&key length name swap-by type slot items &allow-other-keys) item
                   (setf offset (getf item :offset))
                   (push (qualify name) regions)
                   (when slot
                     (push item slots)
                     (push (list (qualify name) (first slot) length swap-by)
                           (getf slots-of (qualify (second slot)))))
                   (case type
                     (:span (preprocess-spec items))))))
             (qualify (name)
               (if (not (and prefix name))
                   name (intern (format nil "~a/~a" prefix name) :keyword)))
             (resolve-region (name slot-field slot-type slot-width swap-by)
               `(funcall ,write (aref ,serializers ,swap-by)
                        (cons ,slot-width
                              ;; :length-of → (- end start) ; :offset-of → start
                              ,(case slot-type
                                 (:length-of `(- (third (assoc ,name ,map))
                                                 (second (assoc ,name ,map))))
                                 (:offset-of `(second (assoc ,name ,map)))))
                        (second (assoc ',slot-field ,map)))) ; slot's reserved position
             (assign-serializer (s)
               `(setf (aref ,serializers ,s)
                      (serializer-for ,(getf params :unit-spec) ,s)))
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
               (destructuring-bind (&key name literal record &allow-other-keys) ctx
                 (let* ((child-record (and record (gensym "REC")))
                        (expansion (macroexpand
                                    (append (list 'marshal child destination)
                                            (and child-record `((:from ,child-record)))
                                            (list :-+sub-lexicon+-
                                                  (list* :pr-sym (qualify name) sub-lexicon))
                                            literal))))
                   (if child-record `(let ((,child-record ,record)) ,expansion) expansion))))
             (masque-binder (b)
               (if (eq :binding (first b))
                   (second b)
                   (destructuring-bind (&key name bind-to default enumerate-by &allow-other-keys) b
                     (list bind-to
                           ;; a default goes through the enum like a given value
                           (or (enumerate enumerate-by (value-source name default))
                               (error "Masque field ~a not specified." name))))))
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
               (or (find-in-manifest (rest spec) (lambda (i) (and (eq sel (getf i :name)) i)))
                   (error "marshal: :case selector ~s is not a field of ~a." sel name)))
             (selector-form (sel field)
               (let* ((item (selector-item sel))
                      (v (value-source sel (getf item :default))))
                 (cond ((not (member sel written))
                        (error "marshal: field ~s needs its :case selector ~s, which must come before it."
                               field sel))
                       ((null v) (error "marshal: :case selector ~s has no value and no default." sel))
                       ((constantp v) (enumerate (getf item :enumerate-by) v)) ;; code known now
                       (t (rest (assoc sel selvars))))))                       ;; set when it was written
             (enum-of (sel)
               (let ((e (getf (find-in-manifest (rest spec) (lambda (i) (and (eq sel (getf i :name)) i)))
                              :enumerate-by)))
                 (and e (gethash e *enums*))))
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
             (generate (item &optional context)
               (destructuring-bind (&key name type (swap-by 0) default count actual bindings with
                                      length end-by codec slot enumerate-by subtypes items mode n
                                      &allow-other-keys) ;; keys only field plans read
                   item
                 (let ((qname (qualify name)))
                   ;; (when context (setf name (intern (format nil "~a/~a" (first context) name)
                   ;;                                  "KEYWORD")))
                   
                   ;; (print (list :nn name context))
                   ;; types: ; :scalar | :pad | :masque | :bytes | :sized | :leb | :name | :slot
                   (case type
                     (:pad `(:-pad
                             (loop :repeat ,(pad-count-form mode n index base)
                                   :do (funcall (aref ,serializers ,swap-by) (cons 1 ,with) ,enter))))
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
                                  :do (funcall (aref ,serializers 0)
                                               ,(if codec `(funcall ,encoder ch) '(char-code ch))
                                               ,enter))
                            ,@(when end-by `((funcall (aref ,serializers 0) ,end-by ,enter)))))))
                     (:masque `(:-masque
                                (funcall (aref ,serializers ,swap-by)
                                         ;; (masque ,actual ,@(mapcar #'masque-binder bindings))
                                         (cons ,length (masque ,actual ,@(mapcar #'masque-binder bindings)))
                                         ,enter)))
                     (:manifest
                      (append
                       `((push (list ,qname ,index ,index) ,map))
                       (loop :for closure :in context
                             :collect `(unless (assoc ,closure ,map)
                                         (push (list ,closure ,index ,index) ,map)))
                       (list name (write-effector
                                   (field-plan item)
                                   ;; literal pairs given for the child win over the record's
                                   ;; value for this field, which is read when the code runs
                                   (list :name name :literal (getf pairs name)
                                         :record (and from name `(field-value ,from ',name))
                                         :write-manifest #'write-manifest)))
                       `((setf (third (assoc ,qname ,map)) ,index))
                       (loop :for closure :in context
                             :collect `(setf (third (assoc ,closure ,map)) ,index))
                       (loop :for slot-of :in (getf slots-of qname)
                             :when (not (eq :checksum-of (second slot-of)))
                               :collect (apply #'resolve-region qname slot-of))))
                     (:span
                      ;; push a fresh per-instance region at the span's start so that
                      ;; repeated span names across sibling sub-manifests don't collide
                      ;; in the shared map (each instance gets its own start/end)
                      (append `((push (list ,qname ,index ,index) ,map))
                              (loop :for i :in items :append (generate i (cons qname context)))
                              `((setf (third (assoc ,qname ,map)) ,index))
                              (loop :for slot-of :in (getf slots-of qname)
                                    :when (not (eq :checksum-of (second slot-of)))
                                      :collect (apply #'resolve-region qname slot-of))))
                     (t (let ((length-form length))
                          ;; (print (list :oo name type-indicator type-conditions))
                          `(,qname
                            (push (list ,qname ,index ,index) ,map)
                            ,@(loop :for closure :in context
                                    :collect `(unless (assoc ,closure ,map)
                                                (push (list ,closure ,index ,index) ,map)))

                            ,(cond
                               ((intersection '(:vec :leb :case) subtypes)
                                (write-effector
                                 (simplify-plan (field-plan item :selector-form #'selector-form
                                                                 :enum-of #'enum-of
                                                                 ;; on write, a buffer's size is an
                                                                 ;; integer :count; a counted vector
                                                                 ;; takes its length from its value
                                                                 :count-form (lambda (i)
                                                                               (let ((c (getf i :count)))
                                                                                 (and (integerp c) c)))
                                                                 :unit-bits (ash 1 (+ 3 (getf params
                                                                                              :unit-spec)))))
                                 (list :name name
                                       :value (if (member :vec subtypes)
                                                  (vec-source-form name default count end-by)
                                                  (scalar-value-form name default enumerate-by slot))
                                       :write (lambda (w f) `(funcall (aref ,serializers ,swap-by)
                                                                      (cons ,w ,f) ,enter)))))
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
                               ((and slot (eq :checksum-of (first slot)))
                                `(funcall (aref ,serializers ,swap-by)
                                          (cons ,length-form
                                                (destructuring-bind (start end)
                                                    (rest (assoc ,(qualify (second slot)) ,map))
                                                  (funcall ,(getf slot :by) ,access start end)))
                                          ,enter))
                               (t `(funcall (aref ,serializers ,swap-by)
                                            (cons ,length-form
                                                  ,(scalar-value-form name default enumerate-by slot))
                                            ,enter)))

                            (setf (third (assoc ,qname ,map)) ,index)
                            ,@(and (getf slots-of qname)
                                   (loop :for slot-of :in (getf slots-of qname)
                                         :when (not (eq :checksum-of (second slot-of)))
                                           :collect (apply #'resolve-region qname slot-of)))
                            ,@(loop :for closure :in context
                                    :collect `(setf (third (assoc ,closure ,map)) ,index))
                            ;; (push ,(getf pairs name) ,values)
                            ;; (push ,qname ,values)
                            ;; (print (list :bi ,name ,index ,length))

                            ))))))))

      (preprocess-spec (rest spec))
      (setf swap-specs (manifest-swap-specs (rest spec)))

      (loop :for (key _) :on slots-of :by #'cddr
            :unless (member key regions)
              :do (error "Slot reference found with no matching region."))
      
      (append (if (getf pairs :-+sub-lexicon+-)
                  `(let ((,base ,index)
                         ,@(and from `((,from ,(getf options :from))))
                         ,@(mapcar (lambda (p) (list (rest p) nil)) selvars)))
                  ;; bind selector variables in sub-manifest's expression
                  `(let* ((,access ,destination)
                          ,@(and from `((,from ,(getf options :from))))
                          (,index ,(or offset 0))
                          (,base ,index)
                          (,values) ;; (,bindings-sym)
                          ,@(if (getf params :length) `((,length ,(getf params :length))))
                          ;; ,@(loop :for c :in (case-selectors (rest spec)) :collect (list c '(gensym)))
                          ,@(mapcar (lambda (p) (list (rest p) nil)) selvars)
                          (,enter) (,tell) (,write) (,map)
                           (,serializers (make-array ,(1+ (reduce #'max swap-specs :initial-value 0)) :initial-element nil)))

                     (typecase ,access
                       (function (setf ,enter (lambda (,b) (funcall ,access ,b) (incf ,index))
                                       ,tell  (lambda () ,index)))
                       (vector   (setf ,enter (lambda (,b) (setf (aref ,access ,index) ,b) (incf ,index))
                                       ,tell  (lambda () ,index)
                                       ,write (lambda (serializer datum position)
                                                (let ((opos ,index))
                                                  (setf ,index position)
                                                  (funcall serializer datum ,enter)
                                                  (setf ,index opos)))))
                       (stream   (setf ,enter (lambda (,b) (incf ,index) (write-byte ,b ,access))
                                       ,tell  (lambda () (file-position ,access))
                                       ,write (lambda (serializer datum position)
                                                (let ((opos (file-position ,access)))
                                                  (file-position ,access position)
                                                  (setf ,index position)
                                                  (funcall serializer datum ,enter)
                                                  (file-position ,access opos)
                                                  (setf ,index opos))))))))

              (unless (getf pairs :-+sub-lexicon+-)
                (mapcar #'assign-serializer swap-specs))
              
              (if spec (apply #'append (mapcar #'generate (rest spec)))
                    (error "Manifest not found."))

              ;; `((print (list :mmm ,map)))
              
              (list index)
              ))))

(defun enumerated (value form)
  "The keyword that stands for VALUE in the enum FORM, or VALUE itself when the enum names
no such code, so that a record holding a code the enum lacks can be read and written back."
  (loop :for (key val) :on (rest form) :by #'cddr :when (equalp val value) :return key
        :finally (return value)))

(defmacro unmarshal (name source &optional params &rest keys)
  "Read the fields of the manifest NAME from SOURCE. With KEYS, return their values in
order; otherwise return every named field, as a plist or, given (:as :record) in PARAMS,
as an instance of the manifest's record class."
  (let* ((spec (gethash name *manifests*))
         (config (and (eq :config (getf (first spec) :type))
                      (first spec)))
         (nested (getf params :-+sub-lexicon+-))
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
         (in-keys keys)
         (keys (or keys all-keys))
         (dvector (make-array 4 :initial-element nil))
         (base (gensym "BA")) (dsym (gensym "DS"))
         raws sub-lexicon prefix cursor)
    
    (loop :for (key) :on params :by #'cddr
          :unless (member key '(:as :-+sub-lexicon+-))
            :do (error "unmarshal: unknown option ~s for manifest ~s." key name))
    (unless (member as '(:plist :record))
      (error "unmarshal: :as must be :plist or :record, not ~s." as))
    (when (and (eq as :record) in-keys)
      (error "unmarshal: (:as :record) returns the whole record; it takes no keys."))
    (when (and (eq as :record) (not nested) (not record-class))
      (error "unmarshal: manifest ~s has no record class; give it (:record t)." name))

    (if (getf params :-+sub-lexicon+-)
        (destructuring-bind (&key cr-sym pr-sym &allow-other-keys) (getf params :-+sub-lexicon+-)
          (setf cursor cr-sym prefix pr-sym sub-lexicon (getf params :-+sub-lexicon+-)))
        (setf cursor (gensym "CR")
              sub-lexicon (list :cr-sym cursor :ds-sym dsym)))
          
    ;; (loop :for item :in (rest spec) :do (setf (getf fields (getf item :name)) item))

    ;; (print (list :ff fields keys))


    (let ((vars) (reads) (src (gensym "SRC")))
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
               (read-manifest (child ctx)
                 ;; expand the sub-manifest CHILD at the cursor in CTX, sharing the source, as a
                 ;; record when one is asked for and CHILD has a record class
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
                                   ,@(and (eq as :record) '(:as :record))))))))
               (record-read (name form)
                 (if name (let ((var (gensym (string name))))
                            (push (cons name var) vars)
                            (push `(setf ,var ,form) reads))
                     (push form reads)))
               (selector-form (sel field)
                 (or (rest (assoc sel raws)) (var-of sel field)))
               (enum-of (sel)
                 (let ((e (getf (find-in-manifest (rest spec) (lambda (i) (and (eq sel (getf i :name)) i)))
                                :enumerate-by)))
                   (and e (gethash e *enums*))))
               (process-item (item)
                 (destructuring-bind (&key name type subtypes length signed (swap-by 0)
                                        items enumerate-by actual codec end-by
                                        bindings mode n &allow-other-keys)
                     item
                   (when nil ;; (member type '(:span)) ;; :manifest))
                     (error "unmarshal: ~a fields not yet supported." type))

                   (unless (aref dvector swap-by)
                     (setf (aref dvector swap-by) `(deserializer-for ,(getf config :unit-spec) ,swap-by)))

                   (case type
                     (:pad (push `(incf ,cursor ,(pad-count-form mode n cursor base)) reads))
                     (:manifest
                      (record-read name (read-effector (field-plan item)
                                                       (list :name name :cursor cursor
                                                             :read-manifest #'read-manifest))))
                     (:span
                      (loop :for item :in items :do (process-item item)))
                     (:string
                      (let ((i (gensym "I")) (got (gensym "G")))
                        (push `(loop :for ch :across ,actual :for ,i :from 0
                                     :for ,got := (funcall (aref ,dsym 0) (+ ,cursor ,i) 1 nil ,src)
                                     :unless (eql ,got ,(if codec `(funcall ,codec ch) '(char-code ch)))
                                       :do (error "unmarshal: expected ~s at unit ~a, read #x~x."
                                                  ,actual (+ ,cursor ,i) ,got))
                              reads)
                        (when end-by
                          (push `(unless (eql ,end-by (funcall (aref ,dsym 0)
                                                               (+ ,cursor ,(length actual)) 1 nil ,src))
                                   (error "unmarshal: missing terminator after ~s." ,actual))
                                reads))
                        (push `(incf ,cursor ,(+ (length actual) (if end-by 1 0))) reads)))
                     (:masque
                      (multiple-value-bind (base _ segments symbols bits) (quantify-mask-string actual nil)
                        (declare (ignore _))
                        (when (= 1 (first segments))
                          (error "unmarshal: decimal masque strings like ~s aren't supported yet." actual))
                        (let ((raw (gensym "MQ"))
                              (checks) (sets)
                              (const-mask (1- (ash 1 bits))))
                          (flet ((group (char)
                                   ;; width and bit position (from the LSB) of mask group CHAR
                                   (let ((ix (position (intern (string-upcase char) "KEYWORD") symbols)))
                                     (unless ix
                                       (error "unmarshal: group ~a not found in masque ~s." char actual))
                                     (values (- (nth (1+ ix) segments) (nth ix segments))
                                             (nth ix segments)))))
                            ;; bits outside every group are the mask string's constant digits
                            (loop :for s :in symbols :for ix :from 0 :when s
                                  :do (setf const-mask
                                            (logandc2 const-mask
                                                      (ash (1- (ash 1 (- (nth (1+ ix) segments)
                                                                         (nth ix segments))))
                                                           (nth ix segments)))))
                            (dolist (b bindings)
                              (if (eq :binding (first b))
                                  ;; static group, e.g. (p +goff-ptv-prefix+): check it against the constant
                                  (destructuring-bind (char form) (second b)
                                    (multiple-value-bind (w p) (group char)
                                      (push `(unless (= (ldb (byte ,w 0) ,form) (ldb (byte ,w ,p) ,raw))
                                               (error "unmarshal: masque ~s group ~a is ~a, expected ~a."
                                                      ,actual ',char (ldb (byte ,w ,p) ,raw)
                                                      (ldb (byte ,w 0) ,form)))
                                            checks)))
                                  ;; keyword group, e.g. (t :kind :u8): read it as a field
                                  (destructuring-bind (&key name bind-to signed enumerate-by &allow-other-keys) b
                                    (when name
                                      (multiple-value-bind (w p) (group bind-to)
                                        (let ((var (gensym (string name)))
                                              (v   (gensym "V")))
                                          (push (cons name var) vars)
                                          (push `(setf ,var
                                                       ,(interpret
                                                         (if signed
                                                             `(let ((,v (ldb (byte ,w ,p) ,raw)))
                                                                (if (logbitp ,(1- w) ,v) (- ,v ,(ash 1 w)) ,v))
                                                             `(ldb (byte ,w ,p) ,raw))
                                                         enumerate-by))
                                                sets)))))))
                            (push `(let ((,raw (funcall (aref ,dsym ,swap-by) ,cursor ,length nil ,src)))
                                     (unless (= ,base (logand ,const-mask ,raw))
                                       (error "unmarshal: constant bits of masque ~s don't match (read #x~x)."
                                              ,actual ,raw))
                                     ,@(reverse checks)
                                     ,@(reverse sets))
                                  reads)
                            (push `(incf ,cursor ,length) reads)))))
                     (t
                      (cond
                        ((intersection '(:vec :leb :case) subtypes)
                         (record-read name ;; enumerate-by subtypes
                                      (read-effector (field-plan item :selector-form #'selector-form
                                                                      :enum-of #'enum-of
                                                                      :count-form
                                                                      (lambda (i)
                                                                        ;; a length slot counts bytes,
                                                                        ;; not elements of varying width
                                                                        (when (and (member :leb (getf i :subtypes))
                                                                                   (not (getf i :count)))
                                                                          (error "unmarshal: vector ~s of LEB ~
                                                                                  elements needs :count or ~
                                                                                  :end-by." (getf i :name)))
                                                                        (vec-count-form (getf i :name)
                                                                                        (getf i :count)
                                                                                        (getf i :length)))
                                                                      :unit-bits (ash 1 (+ 3 (getf config
                                                                                                   :unit-spec))))
                                                     (list :name name :cursor cursor :src src
                                                           :read-at (lambda (at w sg)
                                                                      `(funcall (aref ,dsym ,swap-by)
                                                                                ,at ,w ,sg ,src))))))
                        (t
                         (when name
                           (let ((var (gensym (string name))))
                             (push (cons name var) vars)
                             (if (and enumerate-by (member name (case-selectors (rest spec))))
                                 (let ((raw (gensym (format nil "~a-RAW" name))))
                                   (push (cons name raw) raws)
                                   (push `(setf ,var ,(interpret `(setf ,raw (funcall (aref ,dsym ,swap-by)
                                                                                      ,cursor ,length ,signed ,src))
                                                                 enumerate-by))
                                         reads))
                                 (push `(setf ,var ,(interpret `(funcall (aref ,dsym ,swap-by)
                                                                         ,cursor ,length ,signed ,src)
                                                               enumerate-by))
                                       reads))))
                         (push (list 'incf cursor length) reads))))))))
        
        (dolist (item (rest spec)) (process-item item))

        ;; (dolist (k keys)
        ;;   (unless (assoc k vars) (error "unmarshal: no field ~a in manifest ~a." k name)))

        `(let* ((,src ,source)
                (,base ,(if (getf params :-+sub-lexicon+-) cursor 0))
                (,dsym (vector ,@(coerce dvector 'list)))
                ,@(and (not (getf params :-+sub-lexicon+-))
                       (list (list cursor 0)))
                ,@(mapcar #'rest vars)
                ,@(mapcar #'rest raws))
           ,@(reverse reads)
           ,(cond (in-keys
                   (cons 'values (mapcar (lambda (k) (rest (assoc k vars))) keys)))
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

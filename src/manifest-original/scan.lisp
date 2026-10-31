;;;; scan.lisp

(in-package #:specops)

(defclass stop-rule () ())

(defclass count-rule        (stop-rule)
  ((n    :initarg :n
         :reader  rule-n)))   ;; integer or form

(defclass terminator-rule   (stop-rule)
  ((code :initarg :code
         :reader rule-code))) ;; code form

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
              :initform 1)))

(defclass case-plan   (field-plan)
  ((selector :initarg :selector)   ;; form yielding the selector
   (branches :initarg :branches))) ;; ((keys . plan) …), keys or OTHERWISE

(defgeneric known-count (rule)
  (:method ((r stop-rule)) nil)
  (:method ((r count-rule)) (rule-n r)))

(defgeneric known-extent (plan)
  (:method ((p field-plan)) nil)
  (:method ((p scalar-plan)) (slot-value p 'width))
  (:method ((p case-plan))
    (let ((ws (mapcar (lambda (b) (known-extent (rest b))) (slot-value p 'branches))))
      (and (first ws) (every (lambda (w) (eql w (first ws))) ws) (first ws)))))

(defgeneric stop-effector (rule ctx))

(defgeneric read-effector (plan ctx))  ;; form that returns the value and advances the cursor

(defgeneric write-effector (plan ctx)) ;; form that writes the :value in ctx

(defmethod stop-effector ((r count-rule) ctx)
  (list :before `(>= ,(getf ctx :index) ,(getf ctx :count))))

(defmethod stop-effector ((r terminator-rule) ctx)
  (let ((tm (gensym "TM")))
    (list :bindings `((,tm ,(rule-code r)))
          :after `(eql ,(getf ctx :element) ,tm) :keep nil)))

(defmethod stop-effector ((r continuation-rule) ctx)
  (list :after `(not (logbitp ,(rule-bit r) ,(getf ctx :element))) :keep t))

(defclass assembler () ())

;; (defclass vector-assembler (assembler)
;;   ((etype   :initarg :etype)
;;    (convert :initarg :convert :initform #'identity) ;; expansion-time fn
;;    (decoder :initarg :decoder :initform nil)))      ;; form or nil

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

(defun enum-decode-form (form enum)
  (if enum `(enumerated ,form (gethash ',enum *enums*)) form))

(defmethod assemble-effector ((a vector-assembler) ctx)
  (with-slots (bits signed enum codec) a
    (destructuring-bind (&key element count &allow-other-keys) ctx
      (let* ((v (gensym "V")) (j (gensym "J"))
             (etype (cond (enum t) (signed `(signed-byte ,bits)) (t `(unsigned-byte ,bits))))
             (x (let ((s (if signed
                             `(if (logbitp ,(1- bits) ,element) (- ,element ,(ash 1 bits)) ,element)
                             element)))
                  (if enum `(enumerated ,s (gethash ',enum *enums*)) s))))
        (list :bindings (if count
                            `((,v (make-array ,count :element-type ',etype)) (,j 0))
                            `((,v (make-array 8 :adjustable t :fill-pointer 0))))
              :collect (if count `((setf (aref ,v ,j) ,x) (incf ,j)) `((vector-push-extend ,x ,v)))
              :result (let ((out (if count v `(coerce ,v '(simple-array ,etype (*))))))
                        (if codec `(map 'string ,(codec-form codec "decode") ,out)
                            out)))))))

;; (defmethod assemble-effector ((a vector-assembler) ctx)
;;   (with-slots (etype convert decoder) a
;;     (destructuring-bind (&key element count &allow-other-keys) ctx
;;       (let ((v (gensym "V")) (j (gensym "J")) (x (funcall convert element)))
;;         (list :bindings (if count
;;                             `((,v (make-array ,count :element-type ',etype)) (,j 0))
;;                             `((,v (make-array 8 :adjustable t :fill-pointer 0))))
;;               :collect (if count
;;                            `((setf (aref ,v ,j) ,x) (incf ,j))
;;                            `((vector-push-extend ,x ,v)))
;;               :result (let ((out (if count v `(coerce ,v '(simple-array ,etype (*))))))
;;                         (if decoder `(map 'string ,decoder ,out) out)))))))

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

(defun emit-scan (name rule assembler read-at cursor length src)
  "Return one fused loop that reads, stop-tests and assembles a field, then
advances CURSOR. READ-AT maps an offset form to a form reading one raw element."
  (let* ((e (gensym "E")) (k (gensym "K")) (n (gensym "N")) (stop (gensym "STOP"))
         (count (known-count rule))
         (ctx (list :element e :index k :count (and count n))))
    (destructuring-bind (&key bindings declare before after keep) (stop-effector rule ctx)
      (destructuring-bind (&key ((:bindings ab)) ((:declare ad)) collect result)
          (assemble-effector assembler ctx)
        `(let* (,@(and count `((,n ,count))) (,k 0) ,@bindings ,@ab)
           (declare (type fixnum ,k) ,@declare ,@ad)
           (loop ,@(and before `(:until ,before))
                 :do ,@(and (not count)
                            `((when (and (vectorp ,src)
                                         (> (+ ,cursor (* (1+ ,k) ,length)) (length ,src)))
                                (error "unmarshal: field ~s runs past the end of the source." ',name))))
                     (let* ((,e ,(funcall read-at `(+ ,cursor (* ,k ,length))))
                            ,@(and after `((,stop ,after))))
                       (incf ,k)
                       ,@(if (and after (not keep)) `((unless ,stop ,@collect)) collect)
                       ,@(and after `((when ,stop (loop-finish))))))
           (incf ,cursor (* ,k ,length))
           ,result)))))

(defgeneric frame-effector (rule ctx)
  (:documentation "Write-side fragments for RULE: :bindings, :check (forms run on each
data element), :mark (expansion-time function of an element form and a last-p form,
returning the form to write), :trailer (forms run after the last element)."))

(defmethod frame-effector ((r count-rule) ctx)
  (declare (ignore ctx))
  nil) ;; length vs :count check waits on Plan O5

(defmethod frame-effector ((r terminator-rule) ctx)
  (destructuring-bind (&key name element write &allow-other-keys) ctx
    (let ((tm (gensym "TM")))
      (list :bindings `((,tm ,(rule-code r)))
            :check `((when (eql ,element ,tm)
                       (error "Field ~s contains its terminator ~a." ',name ,tm)))
            :trailer (list (funcall write tm))))))

(defmethod frame-effector ((r continuation-rule) ctx)
  (declare (ignore ctx))
  (let ((bit (rule-bit r)))
    (list :mark (lambda (e last) `(if ,last ,e (logior ,e ,(ash 1 bit)))))))

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
  (let* ((e (gensym "E")) (l (gensym "L"))
         (ctx (list :name name :element e :value value-form :write write)))
    (destructuring-bind (&key bindings check mark trailer) (frame-effector rule ctx)
      (destructuring-bind (&key ((:bindings ab)) ((:declare ad)) prelude more next last)
          (disassemble-effector assembler ctx)
        `(let* (,@ab ,@bindings)
           (declare ,@ad)
           ,@prelude
           (loop :while ,more
                 :do (let* ((,e ,next) (,l ,last))
                       (declare (ignorable ,l))
                       ,@check
                       ,(funcall write (if mark (funcall mark e l) e))))
           ,@trailer)))))

(defmethod read-effector ((p scalar-plan) ctx)
  (destructuring-bind (&key read-at cursor &allow-other-keys) ctx
    (with-slots (width signed enum) p
      `(prog1 ,(enum-decode-form (funcall read-at cursor width signed) enum) (incf ,cursor ,width)))))

(defmethod read-effector ((p scan-plan) ctx)
  (destructuring-bind (&key name read-at cursor src &allow-other-keys) ctx
    (with-slots (rule assembler width) p
      (emit-scan name rule assembler (lambda (at) (funcall read-at at width nil))
                 cursor width src))))

(defmethod write-effector ((p scalar-plan) ctx)
  (destructuring-bind (&key value write &allow-other-keys) ctx
    (funcall write (slot-value p 'width) value)))

(defmethod write-effector ((p scan-plan) ctx)
  (destructuring-bind (&key name value write &allow-other-keys) ctx
    (with-slots (rule assembler width) p
      (emit-frame name rule assembler value (lambda (f) (funcall write width f))))))

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

(defun field-plan (item &key selector-form enum-of count-form (unit-bits 8))
  (destructuring-bind (&key name length signed subtypes count end-by codec enumerate-by
                         type-indicator type-conditions &allow-other-keys) item
    (cond ((member :case subtypes)
           (let ((enum (funcall enum-of type-indicator)))
             (make-instance 'case-plan
               :selector (funcall selector-form type-indicator)
               :branches (loop :for b :in type-conditions
                               :collect (cons (translate-keys (getf b :keys) enum name)
                                              ;; a branch inherits the field's :count, :end-by, :codec
                                              (field-plan (append b item)
                                                          :selector-form selector-form :enum-of enum-of
                                                          :count-form count-form :unit-bits unit-bits))))))
          ((member :vec subtypes)
           (when (and end-by count)          ; until SPECOPS-DRAFT-manifest-complete-D7
             (error "Field ~a: :count with :end-by is not supported yet." name))
           (make-instance 'scan-plan :width length
             :rule (if end-by
                       (make-instance 'terminator-rule :code (terminator-code-form name end-by codec))
                       (make-instance 'count-rule :n (and count-form (funcall count-form item))))
             :assembler (make-instance 'vector-assembler :bits (* length unit-bits) :signed signed
                                                         :enum enumerate-by :codec codec)))
          ((member :leb subtypes)
           (unless (and (eql 1 length) (= 8 unit-bits))
             (error "Field ~s: a LEB field must be one 8-bit unit wide." name))
           (make-instance 'scan-plan :rule (make-instance 'continuation-rule)
                                     :assembler (make-instance 'leb-assembler :signed signed
                                                                              :enum enumerate-by)))
          (t (make-instance 'scalar-plan :width length :signed signed :enum enumerate-by)))))

;; (defun field-plan (item selector-form enum-of)
;;   "Build the plan for the stored field plist ITEM."
;;   (destructuring-bind (&key name length signed subtypes type-indicator type-conditions
;;                        &allow-other-keys) item
;;     (cond ((member :case subtypes)
;;            (let ((enum (funcall enum-of type-indicator)))
;;              (make-instance 'case-plan
;;                :selector (funcall selector-form type-indicator)
;;                :branches (loop :for b :in type-conditions
;;                                :collect (cons (translate-keys (getf b :keys) enum name)
;;                                               (field-plan (list* :name name b)
;;                                                           selector-form enum-of))))))
;;           ((member :leb subtypes)
;;            (make-instance 'scan-plan :rule (make-instance 'continuation-rule)
;;                                      :assembler (make-instance 'leb-assembler :signed signed)))
;;           (t (make-instance 'scalar-plan :width length :signed signed)))))

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

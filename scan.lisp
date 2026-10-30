;;;; scan.lisp

(in-package #:specops)

(defclass stop-rule () ())

(defclass count-rule        (stop-rule)
  ((n    :initarg :n    :reader rule-n)))    ;; integer or form

(defclass terminator-rule   (stop-rule)
  ((code :initarg :code :reader rule-code))) ;; code form

(defclass continuation-rule (stop-rule)
  ((bit  :initarg :bit  :initform 7 :reader rule-bit)))

(defgeneric known-count (rule)
  (:method ((r stop-rule)) nil)
  (:method ((r count-rule)) (rule-n r)))

(defgeneric stop-effector (rule ctx))

(defmethod stop-effector ((r count-rule) ctx)
  (list :before `(>= ,(getf ctx :index) ,(getf ctx :count))))

(defmethod stop-effector ((r terminator-rule) ctx)
  (let ((tm (gensym "TM")))
    (list :bindings `((,tm ,(rule-code r)))
          :after `(eql ,(getf ctx :element) ,tm) :keep nil)))

(defmethod stop-effector ((r continuation-rule) ctx)
  (list :after `(not (logbitp ,(rule-bit r) ,(getf ctx :element))) :keep t))

(defclass assembler () ())

(defclass vector-assembler (assembler)
  ((etype   :initarg :etype)
   (convert :initarg :convert :initform #'identity) ;; expansion-time fn
   (decoder :initarg :decoder :initform nil)))      ;; form or nil

(defclass leb-assembler (assembler)
  ((signed :initarg :signed :initform nil)))

(defgeneric assemble-effector (assembler ctx))

(defmethod assemble-effector ((a vector-assembler) ctx)
  (with-slots (etype convert decoder) a
    (destructuring-bind (&key element count &allow-other-keys) ctx
      (let ((v (gensym "V")) (j (gensym "J")) (x (funcall convert element)))
        (list :bindings (if count
                            `((,v (make-array ,count :element-type ',etype)) (,j 0))
                            `((,v (make-array 8 :adjustable t :fill-pointer 0))))
              :collect (if count
                           `((setf (aref ,v ,j) ,x) (incf ,j))
                           `((vector-push-extend ,x ,v)))
              :result (let ((out (if count v `(coerce ,v '(simple-array ,etype (*))))))
                        (if decoder `(map 'string ,decoder ,out) out)))))))

(defmethod assemble-effector ((a leb-assembler) ctx)
  (let ((v (gensym "V")) (sh (gensym "SH")) (e (getf ctx :element)))
    (list :bindings `((,v 0) (,sh 0))
          :declare `((type unsigned-byte ,v) (type fixnum ,sh))
          :collect `((setf ,v (logior ,v (ash (logand ,e #x7f) ,sh))) (incf ,sh 7))
          :result (if (slot-value a 'signed)
                      `(if (and (plusp ,sh) (logbitp (1- ,sh) ,v)) (- ,v (ash 1 ,sh)) ,v)
                      v))))

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

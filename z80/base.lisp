;;;; z80.lisp

(in-package #:specops.z80)

;; (defclass z80-register (register)
;;   ())

;; (defclass z80-gpregister (z80-register)
;;   ())

;; (defclass z80-ixregister (z80-register)
;;   ())

;; (defclass z80-spregister (z80-register)
;;   ())

(defclass z80-mas (mas-based mas-displaced)
  () (:documentation "Memory access scheme for z80 processors."))

(defclass assembler-z80 (assembler-encoding)
  ((%storage :accessor   asm-storage
             :allocation :class
             :initform   (make-hash-table :test #'eq)
             :initarg    :storage)
   (%lexicon :accessor   asm-lexicon
             :allocation :class
             :initform   (make-hash-table :test #'eq)
             :initarg    :lexicon)
   (%table   :accessor   asm-enc-table
             :allocation :class
             :initform   (make-hash-table :test #'eq)
             :initarg    :lexicon)))

;; (defvar *z80-storage*)
(defvar *z80-layout*)
(defvar *assembler-prototype-z80*)

(setf *z80-layout*
      (list :gpr #(:a :f :af :b :c :bc :d :e :de :h :l :hl)
            :ixr #(:ixl :iyl :ix :iy)
            :spr #(:sp :r :i)))

;; (setf *z80-storage*
;;       (list :gpr (list :a   (make-instance 'z80-gpregister :name :a   :width  8)
;;                        :f   (make-instance 'z80-gpregister :name :f   :width  8)
;;                        :b   (make-instance 'z80-gpregister :name :b   :width  8)
;;                        :c   (make-instance 'z80-gpregister :name :c   :width  8)
;;                        :d   (make-instance 'z80-gpregister :name :d   :width  8)
;;                        :e   (make-instance 'z80-gpregister :name :e   :width  8)
;;                        :h   (make-instance 'z80-gpregister :name :h   :width  8)
;;                        :l   (make-instance 'z80-gpregister :name :l   :width  8)
;;                        :bc  (make-instance 'z80-gpregister :name :bc  :width 16)
;;                        :de  (make-instance 'z80-gpregister :name :de  :width 16)
;;                        :hl  (make-instance 'z80-gpregister :name :hl  :width 16))
;;             :ixr (list :ixl (make-instance 'z80-ixregister :name :ixl :width  8)
;;                        :iyl (make-instance 'z80-ixregister :name :iyl :width  8)
;;                        :ix  (make-instance 'z80-ixregister :name :ix  :width 16)
;;                        :iy  (make-instance 'z80-ixregister :name :iy  :width 16))
;;             :spr (list :sp  (make-instance 'z80-spregister :name :sp  :width  8)
;;                        :r   (make-instance 'z80-spregister :name :r   :width  8)
;;                        :i   (make-instance 'z80-spregister :name :i   :width 16))))

;; as an op-matrix, the symbol is swapped out for a prefix

;; (defun match-ops (op0 op1 val0 &optional val1)
;;   (print (list op0 op1 val0))
;;   (and (or (and (typep op0 'z80-gpregister)
;;                 (eq val0 (reg-name op0))))
;;        (or (and (not op1) (not val1))
;;            (and (typep op1 'z80-gpregister)
;;                 (eq val1 (reg-name op1))))))

(defun match-ops (op0 op1 val0 &optional val1)
  ;; (print (list op0 op1 val0 val1))
  (and (or (and (keywordp op0) (eq op0 val0)))
       (or (not op1)
           (and (keywordp op1) (eq op1 val1)))))

(defun @ (address &optional offset)
    (make-instance 'z80-mas :base address :displ offset))

(defmethod clause-processor ((assembler assembler-z80))
  (declare (ignore assembler))
  (labels ((operand-test (o1 o2)
             (if (keywordp o1)
                 (eq o1 o2)
                 (if (listp o1)
                     (and (listp o2) (eq (second o2) (second o1)))
                     nil)))
           (clause-extend (clauses clause)
             (destructuring-bind (operands opcode) clause
               (let* ((variant-found) (opcode-out)
                      (variant-operands '(:hl   :h   :l (@ :hl)))
                      (vmaps           '((:ix :ixh :ixl (@ :ix 0))
                                         (:iy :iyh :iyl (@ :iy 0))))
                      (ops-out (loop :for i :in operands
                                     :collect
                                     (funcall (lambda (item)
                                                (when (and (> 256 opcode)
                                                           ;; the HL→IX etc. extensions are only done
                                                           ;; for opcodes in the main table, i.e.
                                                           ;; of values 255 and below
                                                           (member item variant-operands
                                                                   :test #'operand-test))
                                                  (setf variant-found t))
                                                item)
                                              (typecase i (number i)
                                                        (symbol (intern (string i) "KEYWORD"))
                                                        (list (cons '@ (list (intern (string (first i))
                                                                                     "KEYWORD")))))))))
                 
                 (mapcar (lambda (op-spec)
                           (destructuring-bind (operands opcode) op-spec
                             (let ((opcode-out opcode))
                               (loop :for ix :from 0 :for op :in operands
                                     :do (when (or (eq :x  op) (and (listp op) (eq :x  (second op))))
                                           (setf opcode-out `(+ ,(ash opcode  8)
                                                                ,(nth ix '(op0 op1)))))
                                         (when (or (eq :xx op) (and (listp op) (eq :xx (second op))))
                                           (setf opcode-out `(+ ,(ash opcode 16)
                                                                ,(nth ix '(op0 op1))))))
                               (list operands opcode-out))))
                         (cons (list ops-out opcode)
                               (if (not variant-found)
                                   nil (let ((ops-out-dd) (ops-out-fd))
                                         (loop :for op :in ops-out :for ix :from 0
                                               :do (let ((pos (position op variant-operands
                                                                        :test #'operand-test)))
                                                     (push (if (not pos) op (nth pos (first vmaps)))
                                                           ops-out-dd)
                                                     (push (if (not pos) op (nth pos (second vmaps)))
                                                           ops-out-fd)))
                                         (list (list (reverse ops-out-dd) (+ #xDD00 opcode))
                                               (list (reverse ops-out-fd) (+ #xFD00 opcode))))))))))
           (swap-numeric (operands)
             (let ((exchanged)) ;; exchanged is an index reflecting :x or :xx as an immediate value
               (values (loop :for op :in operands :collect (if (keywordp op)
                                                               (let ((pos (position op #(:x :xx))))
                                                                 (if (not pos)
                                                                     op (progn (setf exchanged (1+ pos))
                                                                               `(funcall of-code
                                                                                         ,(1+ pos)))))
                                                               (if (not (listp op))
                                                                   op (multiple-value-bind (new-op ex)
                                                                          (swap-numeric op)
                                                                        (setf exchanged (or exchanged ex))
                                                                        new-op))))
                       exchanged)))
           (encoding-entry (varops operands opcode)
             `(((match-ops ,@varops ,@operands) ,opcode))))
    
    (lambda (clauses asm-sym op-symbols)
      (let ((varops (remove '&optional op-symbols))
            (clauses (cons (first clauses)
                           (loop :for clause :in (rest clauses) :append (clause-extend clauses clause)))))
        ;; (loop :for clause :in (rest clauses) :do (clause-extend clauses clause))
        ;; (print (list :clxx clauses))
        (cons `(of-lexicon
                ,asm-sym ,(first clauses) 
                (lambda ,op-symbols
                  (cond ,@(loop :for clause :in (rest clauses)
                                :append (apply #'encoding-entry varops clause)))))
              (loop :for clause :in (rest clauses)
                    :collect (destructuring-bind (operands opcode) clause
                               ;; (print (list :oa operands))
                               (let ((opcode (if (numberp opcode)
                                                 opcode (if (and (listp opcode) (eql '+ (first opcode)))
                                                            opcode))))
                                 (multiple-value-bind (operands exchanged) (swap-numeric operands)
                                   ;; (print (list :ex operands exchanged))
                                   `(of-encoded ,asm-sym
                                                ,(if (not exchanged)
                                                     opcode (ash (second opcode)
                                                                 (* exchanged -8)))
                                                ,(funcall (lambda (form)
                                                            (if (not exchanged)
                                                                form `(lambda (of-code) ,form)))
                                                          (cons 'list (cons (first clauses)
                                                                            operands)))))))))))))

;; (defmethod locate ((assembler assembler-z80) asm-sym items)
;;   (let ((domains (copy-tree (asm-domains assembler)))
;;         (bound (loop :for item :in items :when (member :bind item :test #'eq) :collect item))
;;         (unbound (loop :for item :in items :unless (member :bind item :test #'eq) :collect item)))
;;     (append (loop :for item :in bound
;;                   :collect (destructuring-bind (symbol type &rest params) item
;;                              (list symbol (let ((out-index (series-index type (getf params :bind))))
;;                                             (setf (rest (assoc type domains))
;;                                                   (remove out-index (rest (assoc type domains))))
;;                                             out-index))))
;;             (loop :for item :in unbound
;;                   :collect (destructuring-bind (symbol type &rest params) item
;;                              (list symbol (let ((random-index
;;                                                   (nth (random (length (rest (assoc type domains))))
;;                                                        (rest (assoc type domains)))))
;;                                             (setf (rest (assoc type domains))
;;                                                   (remove random-index (rest (assoc type domains))))
;;                                             random-index)))))))

;; (defmacro cb-logicop-specs (instructions operands)
;;   (let ((offset #xCB00) (index -1))
;;     (cons 'progn (loop :for op :in instructions
;;                        :collect `(setf (gethash ,op (asm-lexicon *assembler-prototype-z80*))
;;                                        (lambda (op0 &optional op1)
;;                                          (cond ,@(loop :for rg :in operands
;;                                                        :collect (progn (incf index)
;;                                                                        `((match-ops op0 op1 ,rg)
;;                                                                          ,(+ offset index)))))))))))

;; (defmacro cb-bitop-specs (instructions operands)
;;   (let ((offset #xCB40) (index -1))
;;     (cons 'progn
;;           (loop :for op :in instructions
;;                 :collect `(setf (gethash ,op (asm-lexicon *assembler-prototype-z80*))
;;                                 (lambda (op0 &optional op1)
;;                                   (cond ,@(loop :for bit :below 8
;;                                                 :append (loop :for rg :in operands
;;                                                               :collect (progn (incf index)
;;                                                                               `((match-ops op0 op1 ,bit ,rg)
;;                                                                                 ,(+ offset index))))))))))))

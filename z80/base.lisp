;;;; z80.lisp

(in-package #:specops.z80)

(defclass mas-z80 (mas-based mas-displaced)
  () (:documentation "Memory access scheme for z80 processors."))

(defclass assembler-z80 (assembler-encoding)
  ((%storage :accessor   asm-storage 
             :allocation :class
             :initform   '(:gpr #(:a :f :af :b :c :bc :d :e :de :h :l :hl)
                           :ixr #(:ixl :iyl :ix :iy)
                           :spr #(:sp :r :i))
             :initarg    :storage)
   (%lexicon :accessor   asm-lexicon
             :allocation :class
             :initform   (make-hash-table :test #'eq)
             :initarg    :lexicon)
   (%decoder :accessor   asm-enc-decoder
             :allocation :class
             :initform   (make-hash-table :test #'eq)
             :initarg    :decoder)))

;; (defvar *z80-storage*)
;; (defvar *z80-layout*)
(defvar *assembler-prototype-z80*)

;; (setf *z80-layout*
;;       (list :gpr #(:a :f :af :b :c :bc :d :e :de :h :l :hl)
;;             :ixr #(:ixl :iyl :ix :iy)
;;             :spr #(:sp :r :i)))

;; as an op-matrix, the symbol is swapped out for a prefix

(defun match-ops (a0 a1 &optional a2 a3)
  (let ((op0 a0) (op1 (if a2 a1 nil))
        (val0 (or a2 a1)) (val1 a3))
    (flet ((match (op val)
             (cond ((position val #(:a :b :c  :d   :e   :f  :af  :bc  :de
                                    :h :l :hl :ixh :ixl :ix :iyh :iyl :iy :sp))
                    (eq op val))
                   ((eq :x  val)
                    (and (integerp op) (not (minusp op)) (> #x00100 op)))
                   ((eq :xx val)
                    (and (integerp op) (not (minusp op)) (> #x10000 op)))
                   (t (let ((masix (position val #(:@x :@xx :@ix+ :@iy+ :@bc :@de :@hl :@sp))))
                        (and (typep val 'mas-z80)
                             (or (and (position masix #(0 1))
                                      (mas-disp val) (not (mas-base val)))
                                 (and (position masix #(2 3))
                                      (mas-disp val) (eq (mas-base val)
                                                         (aref #(:ix :iy) (- masix 2))))
                                 (eq (mas-base val)
                                     (aref #(:bc :de :hl :sp) (- masix 4))))))))))
      (and (match op0 val0)
           (or (not (or op1 val1)) (match op1 val1))))))

(defun @ (base &optional displacement)
  (let ((base (if (numberp base) nil base))
        (displacement (if (numberp base) base displacement)))
    (make-instance 'mas-z80 :base base :displ displacement)))

(defmethod extend-clauses ((assembler assembler-z80) mnemonic operands params sub-clauses)
  (declare (ignore assembler))
  (loop :for clause :in sub-clauses
        :append (destructuring-bind (operands opcode) clause
                  (let* ((variant-found) (opcode-out)
                         (variant-operands '(:hl   :h   :l :@hl))
                         (vmaps           '((:ix :ixh :ixl :@ix+)
                                            (:iy :iyh :iyl :@iy+)))
                         (ops-out (loop :for i :in operands
                                        :collect
                                        (funcall (lambda (item)
                                                   (when (and (> 256 opcode)
                                                              ;; the HL→IX etc. extensions are only done
                                                              ;; for opcodes in the main table, i.e.
                                                              ;; of values 255 and below
                                                              (member item variant-operands :test #'eq))
                                                     (setf variant-found t))
                                                   item)
                                                 (typecase i (number i)
                                                           (symbol (intern (string i) "KEYWORD")))))))
                    (cons (list ops-out opcode)
                          (if (not variant-found)
                              nil (let ((ops-out-dd) (ops-out-fd))
                                    (loop :for op :in ops-out :for ix :from 0
                                          :do (let ((pos (position op variant-operands :test #'eq)))
                                                (push (if (not pos) op (nth pos (first vmaps)))
                                                      ops-out-dd)
                                                (push (if (not pos) op (nth pos (second vmaps)))
                                                      ops-out-fd)))
                                    (list (list (reverse ops-out-dd) (+ #xDD00 opcode))
                                          (list (reverse ops-out-fd) (+ #xFD00 opcode))))))))))

(defmethod clause-processor ((assembler assembler-z80) action mnemonic operands params body)
  (declare (ignore assembler))
  (labels ((decode-symbol (symbol)
             (cond ((eq symbol :x)
                    `(of-program :next-bytes 1))
                   ((eq symbol :xx)
                    `(of-program :next-bytes 2))
                   ((eq symbol :@x)
                    '(list '@ (of-program :next-bytes 1)))
                   ((eq symbol :@xx)
                    '(list '@ (of-program :next-bytes 2)))
                   ((position symbol #(:a :f :af :b :c :bc :d :e :de :h :l :hl
                                       :ixl :ixh :ix :iyl :iyh :iy :sp)
                              :test #'eq)
                    (intern (string symbol) "KEYWORD"))
                   (t (let ((symx (position symbol #(:@ix+ :@iy+ :@c :@bc :@de :@hl :@sp) :test #'eq)))
                        (if (not symx) nil ;; '(:error)
                            (append `(list '@ ,(aref #(:ix :iy :c :bc :de :hl :sp) symx))
                                    (if (> symx 1) nil '((of-program :next-bytes 1)))))))))
                 
           (clause-extend (clauses clause)
             (destructuring-bind (operands opcode) clause
               (let* ((variant-found) (opcode-out) (ops-out operands))
                 (mapcar (lambda (op-spec)
                           (destructuring-bind (operands opcode) op-spec
                             (let ((opcode-out))
                               (loop :for ix :from 0 :for op :in operands
                                     :do (cond ((position op #(:x :@x) :test #'eq)
                                                (setf opcode-out `(+ ,(ash opcode  8)
                                                                     ,(nth ix '(op0 op1)))))
                                               ((position op #(:xx :@xx) :test #'eq)
                                                (setf opcode-out `(+ ,(ash opcode 16)
                                                                     ,(nth ix '(op0 op1)))))
                                               ((position op #(:@ix+ :@iy+) :test #'eq)
                                                (setf opcode-out `(+ ,(ash opcode 8)
                                                                     (mas-displ ,(nth ix '(op0 op1))))))
                                               (t (setf opcode-out opcode))))
                               (list operands opcode-out))))
                         (list (list ops-out opcode))))))
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
             `(((match-ops ,@(if (second operands) varops (list (first varops)))
                           ,@operands)
                ,opcode))))
    (if (and (listp body) (numberp (first body)) (eql action 'of-lexicon))
        ;; (and (listp body) (symbolp (first body)) (numberp (second body)) (eql action 'of-lexicon))
        (values (first body) mnemonic t)
        (if (and (numberp body) (eql action 'of-decoder))
            (values (list 'list mnemonic) body t)
            (let ((varops (remove '&optional operands)))
              (case action
                (of-lexicon
                 (values `((cond ,@(loop :for clause :in (loop :for c :in body :append (clause-extend body c))
                                         :append (apply #'encoding-entry varops clause))))
                         mnemonic))
                (of-decoder
                 (values `((list ,mnemonic ,@(if (integerp body)
                                                 nil (remove nil (mapcar #'decode-symbol (first body))))))
                         (second body)))))))))

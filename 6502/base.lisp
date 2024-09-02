;;;; 6502.lisp

(in-package #:specops.6502)

(defclass 6502-mas (mas-based mas-displaced)
  ((%offset :accessor z80-mas-offset
            :initform nil
            :initarg  :offset-r))
  (:documentation "Memory access scheme for z80 processors."))

(defclass assembler-6502 (assembler)
  ((%storage :accessor   asm-storage
             :allocation :class
             :initform   (make-hash-table :test #'eq)
             :initarg    :storage)
   (%lexicon :accessor   asm-lexicon
             :allocation :class
             :initform   (make-hash-table :test #'eq)
             :initarg    :lexicon)))

(defvar *assembler-prototype-6502*)

(defun match-ops (op0 op1 val0 &optional val1)
  ;; (print (list op0 op1 val0 val1))
  (and (or (and (keywordp op0) (eq op0 val0)))
       (or (not op1)
           (and (keywordp op1) (eq op1 val1)))))

(defun @ (displacement &optional index)
    (make-instance 'z80-mas :disp displacement :index index))

(defmethod matrix-clause-processor ((assembler assembler-6502))
  (declare (ignore assembler))
  (labels ((operand-test (o1 o2)
             (if (keywordp o1)
                 (eq o1 o2)
                 (if (listp o1)
                     (and (listp o2) (eq (second o2) (second o1)))
                     nil))))
    (lambda (clause op-symbols)
      (let ((varops (remove '&optional op-symbols)))
        (destructuring-bind (operands opcode) clause
          (let ((variant-found) (opcode-out)
                (variant-operands '(:hl :h :l (@ :hl)))
                (vmaps     '((:ix :ixh :ixl (@ :ix 0))
                             (:iy :iyh :iyl (@ :iy 0))))
                (ops-out (loop :for i :in operands
                               :collect (typecase i
                                          (symbol (intern (string i) "KEYWORD"))
                                          (list (cons '@ (list (intern (string (first i))
                                                                       "KEYWORD"))))))))

            (loop :for op :in ops-out :for ix :from 0
                  :do (when (member op variant-operands :test #'operand-test)
                        (setf variant-found t))
                      (when (or (eq :x  op) (and (listp op) (eq :x  (second op))))
                        (setf opcode-out `(+ ,(ash opcode  8)
                                             ,(nth ix '(op0 op1)))))
                      (when (or (eq :xx op) (and (listp op) (eq :xx (second op))))
                        (setf opcode-out `(+ ,(ash opcode 16)
                                             ,(nth ix '(op0 op1))))))
            
            (cons `((match-ops ,@varops ,@ops-out) ,(or opcode-out opcode))
                  (if variant-found
                      (let ((ops-out-dd) (ops-out-fd)
                            (opcode-dd (+ #xDD00 opcode))
                            (opcode-fd (+ #xFD00 opcode)))
                        (loop :for op :in ops-out :for ix :from 0
                              :do (let ((pos (position op variant-operands :test #'operand-test)))
                                    ;; (print (list :pp ops-out pos))
                                    (when (and pos (= 3 pos))
                                      (setf opcode-dd `(+ ,(ash opcode-dd 8) ,(nth ix '(op0 op1)))
                                            opcode-fd `(+ ,(ash opcode-fd 8) ,(nth ix '(op0 op1)))))
                                    (push (if (not pos) op (nth pos (first vmaps)))
                                          ops-out-dd)
                                    (push (if (not pos) op (nth pos (second vmaps)))
                                          ops-out-fd)))
                        
                        (setf ops-out-dd (reverse ops-out-dd)
                              ops-out-fd (reverse ops-out-fd))
                       
                        `(((match-ops ,@varops ,@ops-out-dd) ,opcode-fd)
                          ((match-ops ,@varops ,@ops-out-fd) ,opcode-dd)))))))))))

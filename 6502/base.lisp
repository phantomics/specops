;;;; 6502.lisp

(in-package #:specops.6502)

(defclass 6502-mas (mas-displaced)
  () (:documentation "Memory access scheme for 6502 processors."))

(defclass assembler-6502 (assembler-encoding)
  ((%storage :accessor   asm-storage
             :allocation :class
             :initform   '(:gpr #(:a :x :y) :spr #(:sp :pc :f))
             :initarg    :storage)
   (%lexicon :accessor   asm-lexicon
             :allocation :class
             :initform   (make-hash-table :test #'eq)
             :initarg    :lexicon)
   (%decoder :accessor   asm-enc-decoder
             :allocation :class
             :initform   (make-hash-table :test #'eq)
             :initarg    :decoder))
  (:documentation "Assembler for 6502 CPUs."))

(defun enc-displ (item)
  "Encode a memory address in $LLHH little-endian byte order. This also works in reverse."
  (let ((value (if (typep item 'memory-access-scheme) (mas-displ item) item)))
    (+ (ash              value  -8)
       (ash (logand #xFF value)  8))))
  
(defvar *assembler-prototype-6502*)

(defun match-ops (a0 a1 &optional a2 a3)
  (let ((op0 a0) (op1 (if a2 a1 nil))
        (val0 (or a2 a1)) (val1 a3))
    (flet ((match (op val)
             (cond ((position val #(:a :x :y))
                    (eq op val))
                   ((eq :iv val)
                    (and (integerp op) (not (minusp op)) (> #x00100 op)))
                   ((eq :rl val)
                    (or (symbolp op) (typep op '(signed-byte 8))))
                   ((position val #(:zp :in))
                    (and (typep op '6502-mas) (> #x00100 (mas-displ op))))
                   ((position val #(:ab :inw))
                    (and (typep op '6502-mas) (> #x10000 (mas-displ op)))))))
      (and (match op0 val0)
           (or (not (or op1 val1))
               (match op1 val1))))))

;; (defun match-ops (op0 op1 val0 &optional val1)
;;   ;; (print (list op0 op1 val0 val1))
;;   (and (or (and (keywordp op0) (eq op0 val0)))
;;        (or (not op1)
;;            (and (keywordp op1) (eq op1 val1)))))

(defun @ (displacement)
  (make-instance '6502-mas :displ displacement))

;; (:CL (:SED ((IM) 248)))

;; (:CL
;;  (:POP ((:AF) 241) ((:HL) 225) ((:IX) 56801) ((:IY) 64993) ((:DE) 209)
;;   ((:BC) 193))
;;  (OP0 &OPTIONAL OP1)) 
;; (:CL
;;  (:RET ((:M) 248) ((:P) 240) ((:PE) 232) ((:PO) 224) ((:C) 216) ((:NC) 208)
;;   (NIL 201) ((:Z) 200) ((:NZ) 192))
;;  (OP0 &OPTIONAL OP1)) 

;; (defmethod clause-processor ((assembler assembler-6502) assembler-symbol)
;;   (declare (ignore assembler))
;;   (labels ((operand-test (o1 o2)
;;              (if (keywordp o1)
;;                  (eq o1 o2)
;;                  (if (listp o1)
;;                      (and (listp o2) (eq (second o2) (second o1)))
;;                      nil)))
;;            (decode-symbol (symbol)
;;              (if (position symbol #(a x y))
;;                  (intern (string symbol) "KEYWORD")
;;                  (if (eq 'im symbol) `(funcall of-code 1)
;;                      (if (position symbol #(ab inw))
;;                          `(list '@ (enc-displ (funcall of-code 2)))
;;                          (if (position symbol #(in zp rl))
;;                              `(list '@ (funcall of-code 1)))))))
;;            (encoding-entry (varops legal operands opcode)
;;              (let ((operands (remove 'im operands)))
;;                `(((match-ops ,@(if (second operands)
;;                                    varops (list (first varops)))
;;                              ,@(loop :for op :in operands
;;                                      :collect (case op
;;                                                 (x :x) (y :y) (a :a) (ab :ab) (im :im)
;;                                                 (zp :zp) (in :in) (inw :inw)
;;                                                 (rl :rl) (iv :iv) (t op))))
;;                   ,@(if legal nil `((print "Illegal!")))
;;                   ,(let* ((var-pos (or (position 'ab operands)
;;                                        (position 'rl operands)
;;                                        (position 'zp operands)
;;                                        (position 'im operands)
;;                                        (position 'in operands)
;;                                        (position 'inw operands)))
;;                           (var-sym (and var-pos (nth var-pos operands))))
;;                      (if (not var-pos)
;;                          opcode `(+ ,(ash opcode (if (member var-sym '(ab inw)) 16 8))
;;                                     ;; only the :im immediate operands are not
;;                                     ;; derived from a memory address
;;                                     ,(nth var-pos (cond ((eql var-sym 'rl)
;;                                                          '((of-program :label 8 8 op0)
;;                                                            (of-program :label 8 8 op1)))
;;                                                         ((member var-sym '(ab inw))
;;                                                          '((enc-displ op0) (enc-displ op1)))
;;                                                         ((member var-sym '(in zp))
;;                                                          '((mas-displ op0) (mas-displ op1)))
;;                                                         (t '(op0 op1))))))))))))
    
;;     (lambda (clauses op-symbols)
;;       (let ((varops (remove '&optional op-symbols)))
;;         (if (numberp (second clauses))
;;             ;; `(setf (gethash ,(first c) ,table) ,(second c))
;;             (let ((mnemonic (intern (string (first clauses)) "KEYWORD")))
;;               `((of-lexicon ,assembler-symbol ,mnemonic ,(second clauses))
;;                 (of-decoder ,assembler-symbol ,(second clauses)  (list ,mnemonic))))
;;             (cons `(of-lexicon
;;                     ,assembler-symbol ,(intern (string (first clauses)) "KEYWORD")
;;                     (lambda ,op-symbols
;;                       (declare (ignorable op1))
;;                       (cond ,@(loop :for clause :in (rest clauses)
;;                                     :append (apply #'encoding-entry varops
;;                                                    (keywordp (first clauses))
;;                                                    clause)))))
;;                   (loop :for clause :in (rest clauses)
;;                         :append `((of-decoder ,assembler-symbol
;;                                               ,(second clause)
;;                                               (lambda (of-code)
;;                                                 (list ,(intern (string (first clauses)) "KEYWORD")
;;                                                       ,@(mapcar #'decode-symbol (first clause)))))))
;;                   ))))))

(defmethod clause-processor ((assembler assembler-6502) action mnemonic operands params body)
  (declare (ignore assembler mnemonic params))
  ;; (print (list :ff action mnemonic operands params body))
  (labels ((operand-test (o1 o2)
             (if (keywordp o1)
                 (eq o1 o2)
                 (if (listp o1)
                     (and (listp o2) (eq (second o2) (second o1)))
                     nil)))
           (decode-symbol (symbol)
             (if (position symbol #(a x y))
                 (intern (string symbol) "KEYWORD")
                 (if (eq 'im symbol) `(funcall of-code 1)
                     (if (position symbol #(ab inw))
                         `(list '@ (enc-displ (funcall of-code 2)))
                         (if (position symbol #(in zp rl))
                             `(list '@ (funcall of-code 1)))))))
           (encoding-entry (varops legal operands opcode)
             (let ((operands (remove 'im operands)))
               `(((match-ops ,@(if (second operands)
                                   varops (list (first varops)))
                             ,@(loop :for op :in operands
                                     :collect (case op
                                                (x :x) (y :y) (a :a) (ab :ab) (im :im)
                                                (zp :zp) (in :in) (inw :inw)
                                                (rl :rl) (iv :iv) (t op))))
                  ,@(if legal nil `((print "Illegal!")))
                  ,(let* ((var-pos (or (position 'ab  operands)
                                       (position 'rl  operands)
                                       (position 'zp  operands)
                                       (position 'iv  operands)
                                       (position 'in  operands)
                                       (position 'inw operands)))
                          (var-sym (and var-pos (nth var-pos operands))))
                     (if (not var-pos)
                         opcode `(+ ,(ash opcode (if (member var-sym '(ab inw)) 16 8))
                                    ;; only the :im immediate operands are not
                                    ;; derived from a memory address
                                    ,(nth var-pos (cond ((eql var-sym 'rl)
                                                         '((of-program :label 8 8 op0)
                                                           (of-program :label 8 8 op1)))
                                                        ((member var-sym '(ab inw))
                                                         '((enc-displ op0) (enc-displ op1)))
                                                        ((member var-sym '(in zp))
                                                         '((mas-displ op0) (mas-displ op1)))
                                                        (t '(op0 op1))))))))))))
    (let ((varops (remove '&optional operands)))
      ;; (print (list :ee mnemonic params body))
      ;; (if (eq :sed mnemonic) (print (list :bo body action)))
      (if (and (listp body) (symbolp (first body)) (numberp (second body)) (eql action 'of-lexicon))
          (case action
            (of-lexicon (values (second body)         (second body) t))
            (of-decoder (values (list 'list mnemonic) body t)))
          (if (and (numberp body) (eql action 'of-decoder))
              (values (list 'list mnemonic) t)
              (case action
                (of-lexicon (values `((cond ,@(loop :for clause :in (rest body)
                                                    :append (apply #'encoding-entry
                                                                   varops (keywordp (first body))
                                                                   clause))))
                                    mnemonic))
                (of-decoder (values `((list ,mnemonic ,@(if (integerp body)
                                                            nil (mapcar #'decode-symbol (first body)))))
                                    (if (listp body) (second body) body)))
                
                ))))))

;; (OF-ENCODED *ASSEMBLER-PROTOTYPE-Z80* 205
;;             (LAMBDA (OF-CODE) (LIST :CALL (FUNCALL OF-CODE 2))))

#|

(assemble *assembler-prototype-6502*
  (:with)
  (:adc :x :y)
  (:sre :x 50))

|#

(setf *assembler-prototype-6502* (make-instance 'assembler-6502))

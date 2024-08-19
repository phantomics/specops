;;;; base.lisp

(in-package #:specops.m68k)

(defclass m68k-register (register)
  ())

(defclass m68k-gpregister (m68k-register)
  ())

(defclass m68k-adregister (m68k-register)
  ())

(defclass m68k-saregister (m68k-register)
  ())

(defclass m68k-mem-access ()
  ((%qualifier :accessor m68k-mac-qualifier
               :initform nil
               :initarg  :qualifier)))

(defvar *m68k-storage*)

(setf *m68k-storage*
      (list :gpr (list :d0 (make-instance 'm68k-gpregister :name :d0 :width 32 :index 0)
                       :d1 (make-instance 'm68k-gpregister :name :d1 :width 32 :index 1)
                       :d2 (make-instance 'm68k-gpregister :name :d2 :width 32 :index 2)
                       :d3 (make-instance 'm68k-gpregister :name :d3 :width 32 :index 3)
                       :d4 (make-instance 'm68k-gpregister :name :d4 :width 32 :index 4)
                       :d5 (make-instance 'm68k-gpregister :name :d5 :width 32 :index 5)
                       :d6 (make-instance 'm68k-gpregister :name :d6 :width 32 :index 6)
                       :d7 (make-instance 'm68k-gpregister :name :d7 :width 32 :index 7))
            :adr (list :a0 (make-instance 'm68k-adregister :name :a0 :width 32 :index 0)
                       :a1 (make-instance 'm68k-adregister :name :a1 :width 32 :index 1)
                       :a2 (make-instance 'm68k-adregister :name :a2 :width 32 :index 2)
                       :a3 (make-instance 'm68k-adregister :name :a3 :width 32 :index 3)
                       :a4 (make-instance 'm68k-adregister :name :a4 :width 32 :index 4)
                       :a5 (make-instance 'm68k-adregister :name :a5 :width 32 :index 5)
                       :a6 (make-instance 'm68k-adregister :name :a6 :width 32 :index 6)
                       :a7 (make-instance 'm68k-saregister :name :a7 :width 32 :index 7))))

(defclass assembler-m68k (assembler)
  ((%storage :accessor   asm-storage
             :allocation :class
             :initform   (make-hash-table :test #'eq)
             :initarg    :storage)
   (%lexicon :accessor   asm-lexicon
             :allocation :class
             :initform   (make-hash-table :test #'eq)
             :initarg    :lexicon)))

;; (defmethod initialize-instance :after ((assembler assembler-m68k) &key)
;;   (derive-domains assembler (t (:gpr  8))
;;                   (:x86-64 (:gpr 16))
;;                   (:avx    (:vcr  8))
;;                   (:avx2   (:vcr 16))
;;                   (:avx512 (:vcr 32))))

(defun determine-width (width &optional prefix)
  (if prefix
      (case width (:b #b01) (:w #b11) (:l #b10))
      (case width (:b #b00) (:w #b01) (:l #b10))))

(defun determine-amode (operand)
  (typecase operand
    (m68k-gpregister #b000)
    (m68k-adregister #b001)
    (m68k-mem-access
     (case (m68k-mac-qualifier operand)
       (nil          #b010)
       (:postinc     #b011)
       (:predecr     #b100)
       (:displ       #b101)
       (:index       #b110)))
    (t               #b111)))

(defun compose-op (&rest params)
  (flet ((compose-following (number clauses)
           (if (not clauses)
               number (if (eq :long (first clauses))
                          `(+ (ash ,number 32) ,(second clauses))
                          `(+ (ash ,number 16) ,(first clauses))))))
    (if (numberp (first params))
        (compose-following (first params) (rest params))
        (if (stringp (first params))
            (let ((string (first params)))
              (if (loop :for c :across string :always (position c "01." :test #'char=))
                  (let ((base 0)) ;; period is used as a spacer
                    (loop :for c :across string :for ix :from 0 :when (not (char= c #\.)) 
                          :do (when (char= c #\1) (incf base)) ;; set constant 1 bits
                              ;; shift the number to the left unless this is the last digit
                              (unless (= ix (1- (length string))) (setf base (ash base 1))))
                    (compose-following base (rest params)))
                  (compose-following `(masque ,(first params) ,@(second params)) (cddr params))))))))

(defmethod specify-ops ((assembler assembler-m68k) asm-sym op-symbol operands params)
  (let* ((provisions (rest (assoc :provisions params)))
         (ins-part (rest (assoc :instructions params)))
         (ins-meta (rest (assoc :with ins-part)))
         (ins-main))

    (let ((ins-list ins-part))
      (loop :while (and ins-list (not ins-main))
            :do (if (keywordp (caar ins-list))
                    (setf ins-list (rest ins-list))
                    (setf ins-main ins-list))))
    
    `(setf (gethash ,op-symbol (asm-lexicon ,asm-sym))
           (lambda ,operands ,(compose-op params)))))

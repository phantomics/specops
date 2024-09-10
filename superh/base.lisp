;;;; superh.lisp

(in-package #:superh)

(defclass superh-mas (mas-based mas-indexed mas-displaced)
  ((%qualifier :accessor m68k-mas-qualifier
               :initform nil
               :initarg  :qualifier)))

(defun @+ (base)
  (make-instance 'superh-mas :base base :qualifier :post-incr))

(defun -@ (base)
  (make-instance 'superh-mas :base base :qualifier :pre-decr))

(defun @++ (base index &optional displacement)
  (make-instance 'superh-mas :base base :index (if displacement index nil)
                             :displ (or displacement index)))

(defclass assembler-superh (assembler-masking)
  ((%storage :accessor   asm-storage
             :allocation :class
             :initform   '(:gpr #(:r0 :r1 :r2  :r3  :r4  :r5  :r6  :r7
                                  :r8 :r9 :r10 :r11 :r12 :r13 :r14 :r15)
                           :spr #(:sr :gbr :vbr :mach :macl :pr :pc))
             :initarg    :storage)
   (%lexicon :accessor   asm-lexicon
             :allocation :class
             :initform   (make-hash-table :test #'eq)
             :initarg    :lexicon)
   (%breadth :accessor   asm-msk-segment
             :allocation :class
             :initform   '(2)
             :initarg    :breadth)
   (%battery :accessor   asm-msk-battery
             :allocation :class
             :initform   (make-hash-table :test #'eq)
             :initarg    :battery)
   (%domains :accessor   asm-domains
             :initform   nil
             :initarg    :domains)
   (%joiner  :accessor   asm-joiner
             :allocation :class
             :initform   #'joinw
             :initarg    :joiner)))

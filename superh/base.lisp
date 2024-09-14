;;;; superh.lisp

(in-package #:specops.superh)

(defvar *superh-layout*)

(defclass superh-mas (mas-based mas-indexed mas-displaced)
  ((%qualifier :accessor superh-mas-qualifier
               :initform nil
               :initarg  :qualifier)))

(defclass superh-mas-postinc (superh-mas)
  ())

(defclass superh-mas-predecr (superh-mas)
  ())

(defclass superh-mas-bd (superh-mas mas-displaced)
  ())

(defclass superh-mas-pc (mas-displaced)
  ())

(defun @ (base)
  (make-instance 'superh-mas :base base))

(defun @+ (base)
  (make-instance 'superh-mas :base base :qualifier :postinc))

(defun -@ (base)
  (make-instance 'superh-mas :base base :qualifier :predecr))

(defun @0 (index)
  (make-instance 'superh-mas :base :r0 :index index))

(defun @> (base displacement)
  (make-instance 'superh-mas :base base :displ displacement))

(defun @pc (displacement)
  (make-instance 'superh-mas :base :pc :displ displacement))

(defun @gb (displacement)
  (make-instance 'superh-mas :base :gbr :displ displacement))

(defun @gbr0 ()
  (make-instance 'superh-mas :base :gbr :index :r0))

(defun @@tbr (displacement)
  (make-instance 'superh-mas :base :tbr :displ displacement :qualifier :x4))

(setf *superh-layout*
      (list :gpr #(:r0 :r1 :r2  :r3  :r4  :r5  :r6  :r7
                   :r8 :r9 :r10 :r11 :r12 :r13 :r14 :r15)
            :fpr #(:f0 :f1 :f2  :f3  :f4  :f5  :f6  :f7
                   :f8 :f9 :f10 :f11 :f12 :f13 :f14 :f15)
            :spr #(:sr :gbr :vbr :mach :macl :pr :pc)))

(defclass assembler-superh (assembler-masking)
  ((%storage :accessor   asm-storage
             :allocation :class
             :initform   *superh-layout*
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

(defun gprix (index)
  (aref (getf *superh-layout* :gpr) index))

(defun fprix (index)
  (aref (getf *superh-layout* :fpr) index))

(defun gpr-p (item)
  (and (keywordp item)
       (position item (getf *superh-layout* :gpr))))

(defun fpr-p (item)
  (and (keywordp item)
       (position item (getf *superh-layout* :fpr))))

(deftype gpr () `(satisfies gpr-p))

(deftype fpr () `(satisfies fpr-p))

(defun mas-simple-p  (item)
  (and (typep item 'superh-mas)
       (not (mas-displ item))
       (not (superh-mas-qualifier item))))

(defun mas-predecr-p (item)
  (and (typep item 'superh-mas)
       (eq :predecr (superh-mas-qualifier item))))

(defun mas-postinc-p (item)
  (and (typep item 'superh-mas)
       (eq :postinc (superh-mas-qualifier item))))

(defun mas-b+rzero-p (item)
  (and (typep item 'superh-mas)
       (eq :r0 (mas-base item))
       (typep (mas-index item) 'gpr)))

(defun mas-bs+disp-p (item)
  (and (typep item 'superh-mas)
       (mas-displ item)
       (typep (mas-base item) 'gpr)))

(defun mas-pc+disp-p (item)
  (and (typep item 'superh-mas)
       (mas-displ item)
       (eq :pc (mas-base item))))

(defun mas-gb+disp-p (item)
  (and (typep item 'superh-mas)
       (mas-displ item)
       (eq :gbr (mas-base item))))

(defun mas-gb+rzro-p (item)
  (and (typep item 'superh-mas)
       (eq :gbr (mas-base  item))
       (eq :r0  (mas-index item))))

(defun mas-tb+dis4-p (item)
  (and (typep item 'superh-mas)
       (eq :tbr (mas-base item))
       (eq :x4  (superh-mas-qualifier item))
       (mas-displ item)))

(deftype mas-simple  () `(satisfies mas-simple-p))

(deftype mas-predecr () `(satisfies mas-predecr-p))

(deftype mas-postinc () `(satisfies mas-postinc-p))

(deftype mas-b+rzero () `(satisfies mas-b+rzero-p))

(deftype mas-bs+disp () `(satisfies mas-bs+disp-p))

(deftype mas-pc+disp () `(satisfies mas-pc+disp-p))

(deftype mas-pc+disp () `(satisfies mas-pc+disp-p))

(deftype mas-gb+disp () `(satisfies mas-pc+disp-p))

(deftype mas-gb+rzro () `(satisfies mas-gb+rzro-p))

(deftype mas-tb+dis4 () `(satisfies mas-tb+dis4-p))

(defun drv-gpr (index)
  (aref (getf *superh-layout* :gpr) index))

(defmacro address (symbols addressing &body body)
  `(let ,(loop :for s :in symbols :for a :in addressing
               :collect (list a `(or (position ,s (getf *superh-layout* :gpr))
                                     (position ,s (getf *superh-layout* :fpr)))))
     ,@body))

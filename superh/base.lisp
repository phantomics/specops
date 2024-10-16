;;;; superh.lisp

(in-package #:specops.superh)

(defvar *superh-layout*)

(defclass mas-superh (mas-based mas-indexed mas-displaced)
  ((%qualifier :accessor mas-superh-qualifier
               :initform nil
               :initarg  :qualifier)))

(defclass mas-superh-postinc (mas-superh)
  ())

(defclass mas-superh-predecr (mas-superh)
  ())

(defclass mas-superh-bd (mas-superh mas-displaced)
  ())

(defclass mas-superh-pc (mas-displaced)
  ())

(defun @ (base)
  (make-instance 'mas-superh :base base))

(defun @+ (base)
  (make-instance 'mas-superh :base base :qualifier :postinc))

(defun -@ (base)
  (make-instance 'mas-superh :base base :qualifier :predecr))

(defun @0 (index)
  (make-instance 'mas-superh :base :r0 :index index))

(defun @> (base displacement)
  (make-instance 'mas-superh :base base :displ displacement))

(defun @pc (displacement)
  (make-instance 'mas-superh :base :pc :displ displacement))

(defun @gb (displacement)
  (make-instance 'mas-superh :base :gbr :displ displacement))

(defun @gbr0 ()
  (make-instance 'mas-superh :base :gbr :index :r0))

(defun @@tbr (displacement)
  (make-instance 'mas-superh :base :tbr :displ displacement :qualifier :x4))

(setf *superh-layout*
      (list :gpr  '(:r0  :r1  :r2  :r3  :r4  :r5  :r6  :r7 :r8  :r9  :r10 :r11 :r12 :r13 :r14 :r15)
            :fpr  '(:f0  :f1  :f2  :f3  :f4  :f5  :f6  :f7 :f8  :f9  :f10 :f11 :f12 :f13 :f14 :f15)
            :dpr  '(:dr0  :dr2  :dr4  :dr6  :dr8  :dr10 :dr12 :dr14)
            :fvr  '(:fv0 :fv4 :fv8 :fv12)
            :xfr  '(:xf0  :xf1  :xf2  :xf3  :xf4  :xf5  :xf6  :xf7
                    :xf8  :xf9  :xf10 :xf11 :xf12 :xf13 :xf14 :xf15)
            :xdr  '(:xd0  :xd2  :xd4  :xd6  :xd8  :xd10 :xd12 :xd14)
            :gprb '(:rb0 :rb1 :rb2 :rb3 :rb4 :rb5 :rb6 :rb7)
            :spr  '(:sr :gbr :vbr :mach :macl :pr :pc)
            :dspr '(:dsr :a0 :x0 :x1 :y0 :y1)))

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
  (position index (getf *superh-layout* :gpr)))

(defun gprbix (index)
  (position index (getf *superh-layout* :gprb)))

(defun fprix (index)
  (position index (getf *superh-layout* :fpr)))

(defun dprix (index)
  (position index (getf *superh-layout* :dpr)))

(defun xdrix (index)
  (position index (getf *superh-layout* :xdr)))

(defun gpr-p (item)
  (and (keywordp item) (gprix item)))

(defun gprb-p (item)
  (and (keywordp item) (gprbix item)))

(defun fpr-p (item)
  (and (keywordp item) (fprix item)))

(defun dpr-p (item)
  (and (keywordp item) (dprix item)))

(defun xdr-p (item)
  (and (keywordp item) (xdrix item)))

(deftype gpr  () `(satisfies gpr-p))

(deftype gprb () `(satisfies gprb-p))

(deftype fpr  () `(satisfies fpr-p))

(deftype dpr  () `(satisfies dpr-p))

(deftype xdr  () `(satisfies xdr-p))

(defun mas-simple-p  (item)
  (and (typep item 'mas-superh)
       (not (mas-displ item))
       (not (mas-superh-qualifier item))))

(defun mas-predecr-p (item)
  (and (typep item 'mas-superh)
       (eq :predecr (mas-superh-qualifier item))))

(defun mas-postinc-p (item)
  (and (typep item 'mas-superh)
       (eq :postinc (mas-superh-qualifier item))))

(defun mas-b+rzero-p (item)
  (and (typep item 'mas-superh)
       (eq :r0 (mas-base item))
       (typep (mas-index item) 'gpr)))

(defun mas-bs+disp-p (item)
  (and (typep item 'mas-superh)
       (mas-displ item)
       (typep (mas-base item) 'gpr)))

(defun mas-pc+disp-p (item)
  (and (typep item 'mas-superh)
       (mas-displ item)
       (eq :pc (mas-base item))))

(defun mas-gb+disp-p (item)
  (and (typep item 'mas-superh)
       (mas-displ item)
       (eq :gbr (mas-base item))))

(defun mas-gb+rzro-p (item)
  (and (typep item 'mas-superh)
       (eq :gbr (mas-base  item))
       (eq :r0  (mas-index item))))

(defun mas-tb+dis4-p (item)
  (and (typep item 'mas-superh)
       (eq :tbr (mas-base item))
       (eq :x4  (mas-superh-qualifier item))
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
  (nth index (getf *superh-layout* :gpr)))

(defun drv-gprb (index)
  (nth index (getf *superh-layout* :gprb)))

(defun drv-fpr (index)
  (nth index (getf *superh-layout* :fpr)))

(defun rs16 (number)
  (ash number -16))

(defun lo16 (number)
  (logand number #xFFFF))

(defmacro address (symbols addressing &body body)
  `(let ,(loop :for s :in symbols :for a :in addressing
               :collect (list a `(or (position ,s (getf *superh-layout* :gpr))
                                     (position ,s (getf *superh-layout* :fpr)))))
     ,@body))

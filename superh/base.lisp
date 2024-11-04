;;;; superh.lisp

(in-package #:specops.superh)

(defvar *super-h-layout*)
(defvar *assembler-prototype-super-h*)

(setf *super-h-layout*
      (list :gp  '(:r0  :r1  :r2  :r3  :r4  :r5  :r6  :r7 :r8  :r9  :r10 :r11 :r12 :r13 :r14 :r15)
            :fp  '(:f0  :f1  :f2  :f3  :f4  :f5  :f6  :f7 :f8  :f9  :f10 :f11 :f12 :f13 :f14 :f15)
            :dp  '(:dr0  :dr2  :dr4  :dr6  :dr8  :dr10 :dr12 :dr14)
            :fv  '(:fv0  :fv4  :fv8  :fv12)
            :xfr '(:xf0  :xf1  :xf2  :xf3  :xf4  :xf5  :xf6  :xf7
                   :xf8  :xf9  :xf10 :xf11 :xf12 :xf13 :xf14 :xf15)
            :xd  '(:xd0  :xd2  :xd4  :xd6  :xd8  :xd10 :xd12 :xd14)
            :gpb '(:rb0 :rb1 :rb2 :rb3 :rb4 :rb5 :rb6 :rb7)
            :spr  '(:sr :gbr :vbr :mach :macl :pr :pc
                    :fpul :xmtrx)
            :dspr '(:dsr :a0 :x0 :x1 :y0 :y1)))

(defun rix (register &optional type)
  (typecase register
    (register (of-register-type-index register type))
    (keyword  (if type (values (position register (getf *super-h-layout* type))
                               type)
                  (let ((position) (type-found))
                    (loop :for (type-key names) :on *super-h-layout* :by #'cddr :until position
                          :do (setf position   (position register names)
                                    type-found type-key))
                    (values position type-found))))))

(defun gprix (index)
  (position index (getf *super-h-layout* :gp)))

(defun gprbix (index)
  (position index (getf *super-h-layout* :gpb)))

(defun fprix (index)
  (position index (getf *super-h-layout* :fp)))

(defun fvrix (index)
  (position index (getf *super-h-layout* :fv)))

(defun dprix (index)
  (position index (getf *super-h-layout* :dp)))

(defun xdrix (index)
  (position index (getf *super-h-layout* :xd)))

(defun gpr-p (item)
  (and (keywordp item) (gprix item)))

(defun gprb-p (item)
  (and (keywordp item) (gprbix item)))

(defun fpr-p (item)
  (and (keywordp item) (fprix item)))

(defun fvr-p (item)
  (and (keywordp item) (fvrix item)))

(defun dpr-p (item)
  (and (keywordp item) (dprix item)))

(defun xdr-p (item)
  (and (keywordp item) (xdrix item)))

(deftype gpr  () `(satisfies gpr-p))

(deftype gprb () `(satisfies gprb-p))

(deftype fpr  () `(satisfies fpr-p))

(deftype fvr  () `(satisfies fvr-p))

(deftype dpr  () `(satisfies dpr-p))

(deftype xdr  () `(satisfies xdr-p))

(defclass mas-super-h (mas-based mas-indexed mas-displaced)
  ((%qualifier :accessor mas-super-h-qualifier
               :initform nil
               :initarg  :qualifier)))

(defclass mas-super-h-postinc (mas-super-h)
  ())

(defclass mas-super-h-predecr (mas-super-h)
  ())

(defclass mas-super-h-bd (mas-super-h mas-displaced)
  ())

(defclass mas-super-h-pc (mas-displaced)
  ())

(defun @ (base)
  (make-instance 'mas-super-h :base base))

(defun @+ (base)
  (make-instance 'mas-super-h :base base :qualifier :postinc))

(defun -@ (base)
  (make-instance 'mas-super-h :base base :qualifier :predecr))

(defun @0 (index)
  (make-instance 'mas-super-h :base :r0 :index index))

(defun @> (base displacement)
  (make-instance 'mas-super-h :base base :displ displacement))

(defun @pc (displacement)
  (make-instance 'mas-super-h :base :pc :displ displacement))

(defun @gb (displacement)
  (make-instance 'mas-super-h :base :gbr :displ displacement))

(defun @gbr0 ()
  (make-instance 'mas-super-h :base :gbr :index :r0))

(defun @@tbr (displacement)
  (make-instance 'mas-super-h :base :tbr :displ displacement :qualifier :x4))

(defun mas-simple-p  (item)
  (and (typep item 'mas-super-h)
       (not (mas-displ item))
       (not (mas-super-h-qualifier item))))

(defun mas-predecr-p (item)
  (and (typep item 'mas-super-h)
       (eq :predecr (mas-super-h-qualifier item))))

(defun mas-postinc-p (item)
  (and (typep item 'mas-super-h)
       (eq :postinc (mas-super-h-qualifier item))))

(defun mas-b+rzero-p (item)
  (and (typep item 'mas-super-h)
       (eq :r0 (mas-base item))
       (typep (mas-index item) 'gpr)))

(defun mas-bs+disp-p (item)
  (and (typep item 'mas-super-h)
       (mas-displ item)
       (typep (mas-base item) 'gpr)))

(defun mas-pc+disp-p (item)
  (and (typep item 'mas-super-h)
       (mas-displ item)
       (eq :pc (mas-base item))))

(defun mas-gb+disp-p (item)
  (and (typep item 'mas-super-h)
       (mas-displ item)
       (eq :gbr (mas-base item))))

(defun mas-gb+rzro-p (item)
  (and (typep item 'mas-super-h)
       (eq :gbr (mas-base  item))
       (eq :r0  (mas-index item))))

(defun mas-tb+dis4-p (item)
  (and (typep item 'mas-super-h)
       (eq :tbr (mas-base item))
       (eq :x4  (mas-super-h-qualifier item))
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
  (nth index (getf *super-h-layout* :gp)))

(defun drv-gprb (index)
  (nth index (getf *super-h-layout* :gpb)))

(defun drv-fpr (index)
  (nth index (getf *super-h-layout* :fp)))

(defun drv-fvr (index)
  (nth index (getf *super-h-layout* :fv)))

(defun drv-dpr (index)
  (nth index (getf *super-h-layout* :dp)))

(defun drv-xdr (index)
  (nth index (getf *super-h-layout* :xd)))

(defun rs16 (number)
  (ash number -16))

(defun lo16 (number)
  (logand number #xFFFF))

(defclass assembler-super-h (assembler-encoding assembler-masking)
  ((%storage :accessor   asm-storage
             :allocation :class
             :initform   *super-h-layout*
             :initarg    :storage)
   (%lexicon :accessor   asm-lexicon
             :allocation :class
             :initform   (make-hash-table :test #'eq)
             :initarg    :lexicon)
   (%breadth :accessor   asm-msk-segment
             :allocation :class
             :initform   '(2)
             :initarg    :breadth)
   (%decoder :accessor   asm-enc-decoder
             :allocation :class
             :initform   (make-hash-table :test #'eq)
             :initarg    :decoder)
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

(setf *assembler-prototype-super-h* (make-instance 'assembler-super-h))

(defmacro specops-sh (symbol operands &body params)
  (cons 'specops (append (list symbol operands '*assembler-prototype-super-h*) params)))

(defmacro readops-sh (symbol operands &body params)
  (cons 'readops (append (list symbol operands '*assembler-prototype-super-h*) params)))

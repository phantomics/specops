;;;; superh.lisp

(in-package #:specops.superh)

(defvar *super-h-layout*)
(defvar *assembler-prototype-super-h*)

(setf *super-h-layout*
      (list :gp  '(:r0  :r1  :r2  :r3  :r4  :r5  :r6  :r7 :r8  :r9  :r10 :r11 :r12 :r13 :r14 :r15)
            :fp  '(:f0  :f1  :f2  :f3  :f4  :f5  :f6  :f7 :f8  :f9  :f10 :f11 :f12 :f13 :f14 :f15)
            :dp  '(:dr0  :dr2  :dr4  :dr6  :dr8  :dr10 :dr12 :dr14)
            :fv  '(:fv0  :fv4  :fv8  :fv12)
            :xf  '(:xf0  :xf1  :xf2  :xf3  :xf4  :xf5  :xf6  :xf7
                   :xf8  :xf9  :xf10 :xf11 :xf12 :xf13 :xf14 :xf15)
            :xd  '(:xd0  :xd2  :xd4  :xd6  :xd8  :xd10 :xd12 :xd14)
            :gb '(:rb0 :rb1 :rb2 :rb3 :rb4 :rb5 :rb6 :rb7)
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

(defun gpr-p (item)
  (and (keywordp item) (rix item :gp)))

(defun gprb-p (item)
  (and (keywordp item) (rix item :gb)))

(defun fpr-p (item)
  (and (keywordp item) (rix item :fp)))

(defun fvr-p (item)
  (and (keywordp item) (rix item :fv)))

(defun dpr-p (item)
  (and (keywordp item) (rix item :dp)))

(defun xdr-p (item)
  (and (keywordp item) (rix item :xd)))

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
  (make-instance 'mas-super-h-postinc :base base))

(defun -@ (base)
  (make-instance 'mas-super-h-predecr :base base))

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
  (typep item 'mas-super-h-predecr))

(defun mas-postinc-p (item)
  (typep item 'mas-super-h-postinc))

(defun mas-b+rzero-p (item)
  (and (typep item 'mas-super-h)
       (eq :r0 (mas-base item))
       (typep (mas-index item) 'gpr)))

(defun mas-disp-p (item)
  (and (typep item 'mas-super-h)
       (mas-displ item)
       (typep (mas-base item) 'gpr)))

(defun mas-disp4-p (item)
  (and (typep item 'mas-super-h)
       (mas-displ item)
       (zerop (ash (mas-displ item) -4))
       (gpr-p (mas-base item))))

(defun mas-disp8-p (item)
  (and (typep item 'mas-super-h)
       (mas-displ item)
       (zerop (ash (mas-displ item) -8))
       (gpr-p (mas-base item))))

(defun mas-disp12-p (item)
  (and (typep item 'mas-super-h)
       (mas-displ item)
       (zerop (ash (mas-displ item) -12))
       (gpr-p (mas-base item))))

(defun mas-disp20-p (item)
  (and (typep item 'mas-super-h)
       (mas-displ item)
       (zerop (ash (mas-displ item) -20))
       (gpr-p (mas-base item))))

(defun mas-pc+disp-p (item)
  (and (typep item 'mas-super-h)
       (mas-displ item)
       (eq :pc (mas-base item))))

(defun mas-gb+disp-p (item)
  (and (typep item 'mas-super-h)
       (mas-displ item)
       (zerop (ash (mas-displ item) -8))
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

(deftype mas-disp    () `(satisfies mas-disp-p))

(deftype mas-disp4   () `(satisfies mas-disp4-p))

(deftype mas-disp8   () `(satisfies mas-disp8-p))

(deftype mas-disp12  () `(satisfies mas-disp12-p))

(deftype mas-disp20  () `(satisfies mas-disp20-p))

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

(setf *assembler-prototype-super-h* (make-instance 'assembler-super-h :type '(:sh1)))

(defun qualify-operand (operand type)
  (typecase type
    (symbol (case type
              (width `(member (wspec-name ,operand) '(:b :w :l)))
              (gpr `(gpr-p ,operand))
              (fpr `(fpr-p ,operand))
              (label `(or (gpr-p ,operand) (symbolp ,operand)))))
    (list (destructuring-bind (type &rest qualities) type
            (case type
              (width `(member ,operand ',qualities))
              (imm `(and (imm-value ,operand)
                         (< (imm-value ,operand) ,(expt 2 (first qualities)))))
              (mas `(and (typep ,operand 'mas-m68k)
                         (or ,@(loop :for q :in qualities :collect `(typep ,operand ',q)))))
              (reg-fixed `(eq (reg-name ,operand) ,(first qualities))))))))

(defun verbalize-operand (spec)
  (flet ((mas-express (mas-type)
           (case mas-type
             (mas-simple "(Rx)") (mas-postinc "(Rx)+") (mas-predecr "-(Rx)")
             (mas-postinc-r15 "R15+") (mas-predecr-r15 "-R15")
             (mas-disp4 "(disp4,Rx)") (mas-disp8 "(disp8,Rx)") (mas-disp12 "(disp12,Rx)")
             (mas-b+rzero "(R0,Rx)") (mas-gb+rzro "(R0,GBR)")
             (mas-pc+disp "(disp,PC)")(mas-gb+disp "(disp8,GBR)")
             )))
    (format nil "~a~%" (typecase spec
                         (symbol (case spec
                                   (width "width : any")
                                   (gpr "general purpose register : any")
                                   (fpr "floating point register : any")
                                   (label "label")))
                         (list (destructuring-bind (type &rest qualities) spec
                                 (case type
                                   (width (format nil "width : ~{~a~^ ~^/ ~}" qualities))
                                   (imm (format nil "immediate value : ~d bits" (first qualities)))
                                   (mas (format nil "memory access : ~{~a ~}"
                                                (mapcar #'mas-express qualities)))
                                   (reg-fixed (format nil "~a (fixed register)"
                                                      (first qualities))))))))))

(defun derive-operand (spec)
  (destructuring-bind (operand &rest types) spec
    (cons 'cond (loop :for type :in types
                    :collect
                    (list (qualify-operand operand type)
                          (typecase type
                            (symbol (case type
                                      (width `(determine-width (wspec-name ,operand)))
                                      (gpr `(position (reg-name ,operand)
                                                      (getf *super-h-layout* :gp)))
                                      (fpr `(position (reg-name ,operand)
                                                      (getf *super-h-layout* :fp)))
                                      (label operand)))
                            (list (destructuring-bind (type &rest qualities) type
                                    (declare (ignore qualities))
                                    (case type
                                      (width `(determine-width ,operand))
                                      (imm (list 'imm-value operand))
                                      (mas `(encode-location ,operand))
                                      (reg-fixed `(reg-name ,operand)))))))))))

(defmacro determine (specs &optional bindings &body body)
  "This placeholder macro is not meant to be evaluated but to inform indentation algorithms; (determine) forms will be converted to determine-in-context forms as (specops) forms are expanded."
  (list specs bindings body))

(defun form-process (symbol options form)
  (declare (ignore options))
  (if (eql 'determine (first form))
      (append (list 'determine-in-context
                    (list :qualify #'qualify-operand :verbalize #'verbalize-operand
                          :derive #'derive-operand)
                    symbol)
              (rest form))))

(defmacro specops-sh (symbol operands &body params)
  (let* ((options (and (listp (first params))
                       (listp (caar params))
                       (keywordp (caaar params))
                       (first params)))
         (operations (if options (rest params) params)))
    (cons 'specops (append (list symbol operands '*assembler-prototype-super-h*)
                           (cons (cons (cons :process-forms 'form-process)
                                       options)
                                 operations)))))

;; (defmacro specops-sh (symbol operands &body params)
;;   (cons 'specops (append (list symbol operands '*assembler-prototype-super-h*) params)))

(defmacro readops-sh (symbol operands &body params)
  (cons 'readops (append (list symbol operands '*assembler-prototype-super-h*) params)))

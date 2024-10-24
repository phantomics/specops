;;;; base.lisp

(in-package #:specops.m68k)

(defvar *m68k-layout*)

(setf *m68k-layout*
      (list :gpr '(:d0 :d1 :d2 :d3 :d4 :d5 :d6 :d7)
            :adr '(:a0 :a1 :a2 :a3 :a4 :a5 :a6 :a7)
            :spr '(:usp :sr :ccr)))

(defun rix (register &optional type)
  (typecase register
    (register (of-register-type-index register type))
    (keyword  (if type (values (position register (getf *m68k-layout* type))
                               type)
                  (let ((position) (type-found))
                    (loop :for (type-key names) :on *m68k-layout* :by #'cddr :until position
                          :do (setf position   (position register names)
                                    type-found type-key))
                    (values position type-found))))))

(defclass m68k-mas (mas-based mas-indexed mas-displaced)
  ((%qualifier :accessor m68k-mas-qualifier
               :initform nil
               :initarg  :qualifier)))

(defun @+ (base)
  (if (not (position base (getf *m68k-layout* :adr)))
      (error "Memory can only be addressed using an address register.")
      (make-instance 'm68k-mas :base base :qualifier :post-incr)))

(defun -@ (base)
  (if (not (position base (getf *m68k-layout* :adr)))
      (error "Memory can only be addressed using an address register.")
      (make-instance 'm68k-mas :base base :qualifier :pre-decr)))

(defun @~ (base index &optional displacement)
  (if (not (position base (getf *m68k-layout* :adr)))
      (error "Memory can only be addressed using an address register.")
      (make-instance 'm68k-mas :base base :index (if displacement index nil)
                               :displ (or displacement index))))

(defvar *assembler-prototype-m68k*)

(defclass assembler-m68k (assembler-encoding assembler-masking)
  ((%storage :accessor   asm-storage
             :allocation :class
             :initform   *m68k-layout*
             :initarg    :storage)
   (%lexicon :accessor   asm-lexicon
             :allocation :class
             :initform   (make-hash-table :test #'eq)
             :initarg    :lexicon)
   (%segment :accessor   asm-msk-segment
             :allocation :class
             :initform   '(1)
             :initarg    :segment)
   (%decoder :accessor   asm-enc-decoder
             :allocation :class
             :initform   (make-hash-table :test #'eq)
             :initarg    :decoder)
   (%battery :accessor   asm-msk-battery
             :allocation :class
             :initform   (make-hash-table :test #'eq)
             :initarg    :battery)
   (%domains :accessor   asm-domains
             :initform   *m68k-layout*
             :initarg    :domains)
   (%breadth :accessor   asm-breadth
             :allocation :class
             :initform   16
             :initarg    :breadth)
   (%joiner  :accessor   asm-joiner
             :allocation :class
             :initform   #'joinw
             :initarg    :joiner)))

;; (defmethod initialize-instance :after ((assembler assembler-m68k) &key)
;;   (derive-domains assembler (t (:gpr 8) (:adr 8))))

(defun determine-width (width &optional prefix)
  (if prefix
      (case width (:b #b01) (:w #b11) (:l #b10))
      (case width (:b #b00) (:w #b01) (:l #b10))))

(defun derive-width (width &optional prefix)
  (if prefix
      (case width (#b01 :b) (#b11 :w) (#b10 :l))
      (case width (#b00 :b) (#b01 :w) (#b10 :l))))

(defun determine-width-bit (width)
  (case width (:w 0) (:l 1)))

(defun derive-width-bit (width)
  (if (zerop width) :w :l))
  
(defun base-or-reg-index (item)
  (if (typep item 'm68k-mas)
      (reg-index (mas-base item))
      (reg-index item)))

;; (defun special-reg-p (item name)
;;   (and (typep item 'm68k-spregister)
;;        (eq name (reg-name op0))))

;; (defun determine-amode (operand)
;;   (typecase operand
;;     (m68k-gpregister #b000)
;;     (m68k-adregister #b001)
;;     (m68k-mas
;;      (case (m68k-mas-qualifier operand)
;;        (nil          #b010)
;;        (:postinc     #b011)
;;        (:predecr     #b100)
;;        (:displ       #b101)
;;        (:index       #b110)))
;;     (t               #b111)))

(defun gprix (index)
  (position index (getf *m68k-layout* :gpr)))

(defun adrix (index)
  (position index (getf *m68k-layout* :adr)))

(defun gpr-p (item)
  (and (keywordp item) (gprix item)))

(defun adr-p (item)
  (and (keywordp item) (adrix item)))

(deftype gpr () `(satisfies gpr-p))

(deftype adr () `(satisfies adr-p))

(defun mas-simple-p  (item)
  (and (typep item 'm68k-mas)
       (not (mas-displ item))
       (not (m68k-mas-qualifier item))))

(defun mas-predecr-p (item)
  (and (typep item 'm68k-mas)
       (eq :pre-decr (m68k-mas-qualifier item))))

(defun mas-postinc-p (item)
  (and (typep item 'm68k-mas)
       (eq :post-incr (m68k-mas-qualifier item))))

(defun location-p (item)
  (or (gpr-p item) (adr-p item) (typep item 'm68k-mas)))

(deftype mas-simple  () `(satisfies gpr-p))

(deftype mas-predecr () `(satisfies adr-p))

(deftype mas-postinc () `(satisfies adr-p))

(deftype location    () `(satisfies location-p))

(defun derive-location (addressing-mode index &key base displacement)
  (case addressing-mode
    (#b000 (nth index (getf *m68k-layout* :gpr)))
    (#b001 (nth index (getf *m68k-layout* :adr)))
    (#b011 (list '@+ (nth index (getf *m68k-layout* :adr))))
    (#b100 (list '-@ (nth index (getf *m68k-layout* :adr))))
    (#b101 (list '@~ (nth base (getf *m68k-layout* :adr)) nil displacement))))

(defmethod of-storage ((assembler assembler-m68k) key)
  (if (typep key 'm68k-mas)
      (values (rix (mas-base key) :adr)
              ;; (position (mas-base key) (getf (asm-storage assembler) :adr))
              (case (m68k-mas-qualifier key)
                (:postinc #b011) (:predecr #b100)
                (t (if (not (mas-displ key))
                       #b010 (if (mas-index key) #b110 #b101)))))
      ;; (multiple-value-bind (index type) (call-next-method)
      ;;   (values index (case type (:gpr #b000) (:adr #b001))))
      (multiple-value-bind (index type) (rix key)
        (values index (case type (:gpr #b000) (:adr #b001))))
      ))

;; (defmethod locate ((assembler assembler-m68k) asm-sym items)
;;   (let ((domains (copy-tree (asm-domains assembler)))
;;         (bound (loop :for item :in items :when (member :bind item :test #'eq) :collect item))
;;         (unbound (loop :for item :in items :unless (member :bind item :test #'eq) :collect item)))
;;     ;; (print (list :bu bound unbound domains))
    
;;     (append (loop :for item :in bound
;;                   :collect (destructuring-bind (symbol type &rest params) item
;;                              ;; (print (list :dom domains))
;;                              ;; (print (list :par params type (getf *m68k-storage* type)))
;;                              (let ((bind-position (getf params :bind)))
;;                                (setf (rest (assoc type domains))
;;                                      (remove bind-position (rest (assoc type domains))))
;;                                (list symbol `(reserve ,asm-sym ,type ,bind-position)))))
;;             (loop :for item :in unbound
;;                   :collect (destructuring-bind (symbol type &rest params) item
;;                              (declare (ignore params))
;;                              (list symbol (let ((random-index
;;                                                   (nth (random (length (rest (assoc type domains))))
;;                                                        (rest (assoc type domains)))))
;;                                             (setf (rest (assoc type domains))
;;                                                   (remove random-index (rest (assoc type domains))))
;;                                             `(reserve ,asm-sym ,type ,random-index))))))))

;; (defmethod reserve ((assembler assembler-m68k) &rest params)
;;   (destructuring-bind (type index &rest _) params
;;     (declare (ignore _))
;;     (aref (getf (asm-storage assembler) type) index)))

#|

(assemble *assembler-prototype-m68k*
  ((:store (abc :gpr) (def :gpr)))
  (:addi :b 10 abc)
  (:addi :b 330 def))

(assemble *assembler-prototype-m68k*
        ((:store (abc :gpr) (def :gpr)))
        (:addi :b 10 abc)
        :hello
        (:nop)
        (:ori :b 330 def)
        (:bra :hello)
        (:ori :b 330 def)
        :gg (:addi :b 10 abc))

(@+ abc)

(-@ abc)

(@ abc 50)


|#





;; (defclass m68k-register (register)
;;   ())

;; (defclass m68k-gpregister (m68k-register)
;;   ((%width :accessor   reg-width
;;            :allocation :class
;;            :initform   32
;;            :initarg    :width
;;            :documentation "The register's width.")))

;; (defclass m68k-adregister (m68k-register)
;;   ())

;; (defclass m68k-saregister (m68k-register)
;;   ())

;; (defclass m68k-spregister (m68k-register)
;;   ())

;; (setf *m68k-storage*
;;       (list :gpr (vector (make-instance 'm68k-gpregister :name :d0 :index 0)
;;                          (make-instance 'm68k-gpregister :name :d1 :index 1)
;;                          (make-instance 'm68k-gpregister :name :d2 :index 2)
;;                          (make-instance 'm68k-gpregister :name :d3 :index 3)
;;                          (make-instance 'm68k-gpregister :name :d4 :index 4)
;;                          (make-instance 'm68k-gpregister :name :d5 :index 5)
;;                          (make-instance 'm68k-gpregister :name :d6 :index 6)
;;                          (make-instance 'm68k-gpregister :name :d7 :index 7))
;;             :adr (vector (make-instance 'm68k-adregister :name :a0 :index 0)
;;                          (make-instance 'm68k-adregister :name :a1 :index 1)
;;                          (make-instance 'm68k-adregister :name :a2 :index 2)
;;                          (make-instance 'm68k-adregister :name :a3 :index 3)
;;                          (make-instance 'm68k-adregister :name :a4 :index 4)
;;                          (make-instance 'm68k-adregister :name :a5 :index 5)
;;                          (make-instance 'm68k-adregister :name :a6 :index 6)
;;                          (make-instance 'm68k-saregister :name :a7 :index 7))
            
;;             :usp (make-instance 'm68k-spregister :name :usp :width 32 :index 0)
;;             :sr  (make-instance 'm68k-spregister :name :sr  :width 16 :index 0)
;;             :ccr (make-instance 'm68k-spregister :name :ccr :width 8  :index 0)))

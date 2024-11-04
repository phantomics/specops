;;;; base.lisp

(in-package #:specops.m68k)

(defvar *m68k-layout*)

(setf *m68k-layout*
      (list :gp '(:d0 :d1 :d2 :d3 :d4 :d5 :d6 :d7)
            :ad '(:a0 :a1 :a2 :a3 :a4 :a5 :a6 :a7)
            :sp '(:usp :sr :ccr)))

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

(defun gpr-p (item)
  (and (keywordp item) (rix item :gp)))

(defun adr-p (item)
  (and (keywordp item) (rix item :ad)))

(deftype gpr () `(satisfies gpr-p))

(deftype adr () `(satisfies adr-p))

(defclass mas-m68k (mas-based mas-indexed mas-displaced)
  ())

(defclass mas-m68k-postinc (mas-m68k)
  ())

(defclass mas-m68k-predecr (mas-m68k)
  ())

(defclass mas-m68k-absolute (mas-absolute)
  ())

(defun mas-simple-p  (item)
  (and (typep item 'mas-m68k)
       (not (or (typep item 'mas-m68k-predecr)
                (typep item 'mas-m68k-postinc)))))

(defun mas-postinc-p (item)
  (typep item 'mas-m68k-postinc))

(defun mas-predecr-p (item)
  (typep item 'mas-m68k-predecr))

(defun mas-disp-p (item)
  (and (typep item 'mas-m68k)
       (mas-displ item) (adr-p (mas-base item))))

(defun mas-i+disp-p (item)
  (and (typep item 'mas-m68k)
       (mas-displ item) (adr-p (mas-base item)) (adr-p (mas-index item))))

(defun mas-pc+disp-p (item)
  (and (typep item 'mas-m68k)
       (mas-displ item) (eq :pc (mas-base item))))

(defun mas-pci+disp-p (item)
  (and (typep item 'mas-m68k)
       (mas-displ item) (eq :pc (mas-base item)) (adr-p (mas-index item))))

(defun mas-abs-w-p (item)
  (and (typep item 'mas-m68k-absolute)
       (< (mas-addr item) (expt 2 16)))

(defun mas-abs-l-p (item)
  (and (typep item 'mas-m68k-absolute)
       (< (mas-addr item) (expt 2 32)))))

(defun location-p (item)
  (or (gpr-p item) (adr-p item) (typep item 'mas-m68k)))

(deftype mas-simple   () `(satisfies mas-simple-p))

(deftype mas-postinc  () `(satisfies mas-postinc-p))

(deftype mas-predecr  () `(satisfies mas-predecr-p))

(deftype mas-disp     () `(satisfies mas-disp-p))

(deftype mas-i+disp   () `(satisfies mas-i+disp-p))

(deftype mas-pc+disp  () `(satisfies mas-pc+disp-p))

(deftype mas-pci+disp () `(satisfies mas-pci+disp-p))

(deftype mas-abs-w    () `(satisfies mas-abs-w-p))

(deftype mas-abs-l    () `(satisfies mas-abs-l-p))

(deftype location     () `(satisfies location-p))

(defun @ (base &optional index displacement)
  (if (not (position base (getf *m68k-layout* :ad)))
      (error "Memory can only be addressed using an address register.")
      (make-instance 'mas-m68k :base base :index (if displacement index nil)
                               :displ (or displacement index))))

(defun @+ (base)
  (if (not (position base (getf *m68k-layout* :ad)))
      (error "Memory can only be addressed using an address register.")
      (make-instance 'mas-m68k-postinc :base base)))

(defun -@ (base)
  (if (not (position base (getf *m68k-layout* :ad)))
      (error "Memory can only be addressed using an address register.")
      (make-instance 'mas-m68k-predecr :base base)))

(defun @= (addr)
  (make-instance 'mas-m68k-addr :addr addr))

(defun encode-extension-word (width access)
  (if (or (typep access 'mas-i+disp) (typep access 'mas-pci+disp))
      (masque "MXXXS000.DDDDDDDD"
              (m 0) (x (rix (mas-index access) :ad))
              (s (case width (:w 0) (:l 1) (t 0))) (d (mas-displ access)))))

(defun encode-location (location)
  (typecase location
    (gpr          (values #b000 (rix            location  :gp)))
    (adr          (values #b001 (rix            location  :ad)))
    (mas-simple   (values #b010 (rix  (mas-base location) :ad)))
    (mas-postinc  (values #b011 (rix  (mas-base location) :ad)))
    (mas-predecr  (values #b100 (rix  (mas-base location) :ad)))
    (mas-disp     (values #b101 (rix  (mas-base location) :ad) (mas-displ location)))
    (mas-i+disp   (values #b110 (rix  (mas-base location) :ad)))
    (mas-pc+disp  (values #b111 #b010 (mas-displ location)))
    (mas-pci+disp (values #b111 #b011))
    (mas-abs-w    (values #b111 #b000 (mas-addr location)))
    (mas-abs-l    (values #b111 #b001 (mas-addr location)))
    (integer      (values #b111 #b100 location))))

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
;;   (derive-domains assembler (t (:gpr 8) (:ad 8))))

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
  (if (typep item 'mas-m68k)
      (reg-index (mas-base item))
      (reg-index item)))

;; (defun special-reg-p (item name)
;;   (and (typep item 'm68k-spregister)
;;        (eq name (reg-name op0))))

;; (defun determine-amode (operand)
;;   (typecase operand
;;     (m68k-gpregister #b000)
;;     (m68k-adregister #b001)
;;     (mas-m68k
;;      (case (mas-m68k-qualifier operand)
;;        (nil          #b010)
;;        (:postinc     #b011)
;;        (:predecr     #b100)
;;        (:displ       #b101)
;;        (:index       #b110)))
;;     (t               #b111)))

(defun qualify-operand (operand type)
  (typecase type
    (symbol (case type
              (width `(member (wspec-name ,operand) '(:b :w :l)))
              (gpr `(gpr-p ,operand))
              (adr `(adr-p ,operand))
              (mas `(typep ,operand 'mas-m68k))))
    (list (destructuring-bind (type &rest qualities) type
            (case type
              (width `(member ,operand ,qualities))
              (imm `(and (integerp ,operand)
                         (< ,operand ,(expt 2 (reduce #'max qualities)))))
              (mas `(and (typep ,operand 'mas-m68k)
                         (or ,@(loop :for q :in qualities :collect `(typep ,operand ',q))))))))))

(defun verbalize-operand (spec)
  (flet ((mas-express (mas-type)
           (case mas-type
             (mas-simple "(An)") (mas-postinc "(An)+") (mas-predecr "-(An)") (mas-b+disp "(d16,An)")
             (mas-bi+disp "(d8,An,Xn)") (mas-pc+disp "(d16,PC)") (mas-pci+disp "(d8,PC,Xn)")
             (mas-abs-w "ABS.W") (mas-abs-l "ABS.L"))))
    (format nil "~a~%" (typecase spec
                         (symbol (case spec
                                   (width "width: any")
                                   (gpr "general purpose register: any")
                                   (adr "address register: any")))
                         (list (destructuring-bind (type &rest qualities) spec
                                 (case type
                                   (width (format nil "width: ~{~a~^, ~}" qualities))
                                   (imm (format nil "immediate value : ~{~a~^ ~^/ ~} bits" qualities))
                                   (mas (format nil "memory access: ~{~a ~}"
                                                (mapcar #'mas-express qualities))))))))))

(defun derive-operand (operand type)
  (typecase type
    (symbol (case type
              (width `(determine-width (wspec-name ,operand)))
              (gpr `(encode-location ,operand))
              (adr `(encode-location ,operand))
              ((mas-simple mas-postinc mas-predecr mas-b+disp mas-bi+disp mas-pc+disp mas-pci+disp mas-abs)
               `(typep ,operand ,type))))
    (list (destructuring-bind (type &rest qualities) type
            (declare (ignore qualities))
            (case type
              (width `(determine-width ,operand))
              (imm (list 'imm operand)))))))

(defmacro determine (mnemonic specs &optional bindings &body body)
  `(determine-in-context ,(list :qualify #'qualify-operand :verbalize #'verbalize-operand
                                :derive #'derive-operand)
                         ,mnemonic ,specs ,bindings ,@body))

(defun derive-location (addressing-mode index &key base displacement)
  (case addressing-mode
    (#b000 (nth index (getf *m68k-layout* :gp)))
    (#b001 (nth index (getf *m68k-layout* :ad)))
    (#b011 (list '@+ (nth index (getf *m68k-layout* :ad))))
    (#b100 (list '-@ (nth index (getf *m68k-layout* :ad))))
    (#b101 (list '@~ (nth base (getf *m68k-layout* :ad)) nil displacement))))

(defmethod of-storage ((assembler assembler-m68k) key)
  (if (typep key 'mas-m68k)
      (values (rix (mas-base key) :ad)
              ;; (position (mas-base key) (getf (asm-storage assembler) :ad))
              (case (mas-m68k-qualifier key)
                (:postinc #b011) (:predecr #b100)
                (t (if (not (mas-displ key))
                       #b010 (if (mas-index key) #b110 #b101)))))
      ;; (multiple-value-bind (index type) (call-next-method)
      ;;   (values index (case type (:gp #b000) (:ad #b001))))
      (multiple-value-bind (index type) (rix key)
        (values index (case type (:gp #b000) (:ad #b001))))
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

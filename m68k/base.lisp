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

(defun mas-bi+disp-p (item)
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

(deftype mas-bi+disp  () `(satisfies mas-bi+disp-p))

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

(defun encode-location (location)
  (typecase location
    (gpr          (values (rix            location  :gp) #b000))
    (adr          (values (rix            location  :ad) #b001))
    (mas-simple   (values (rix  (mas-base location) :ad) #b010))
    (mas-postinc  (values (rix  (mas-base location) :ad) #b011))
    (mas-predecr  (values (rix  (mas-base location) :ad) #b100))
    (mas-disp     (values (rix  (mas-base location) :ad) #b101 (mas-displ location)))
    (mas-bi+disp  (values (rix  (mas-base location) :ad) #b110))
    (mas-abs-w    (values #b000                          #b111 (mas-addr  location)))
    (mas-abs-l    (values #b001                          #b111 (mas-addr  location)))
    (mas-pc+disp  (values #b010                          #b111 (mas-displ location)))
    (mas-pci+disp (values #b011                          #b111))
    (immediate    (values #b100                          #b111 (imm-value location)))))

(defun encode-extension-word (width access)
  (if (or (typep access 'mas-bi+disp) (typep access 'mas-pci+disp))
      (masque "MXXXS000.DDDDDDDD"
              (m 0) (x (rix (mas-index access) :ad))
              (s (case width (:w 0) (:l 1) (t 0))) (d (mas-displ access)))))

(defun derive-location (mode index &key base displacement (pa (lambda (a b) (declare (ignore a b)) 0)))
  (case mode
    (#b000           (nth index (getf *m68k-layout* :gp)))  ;; Dn
    (#b001           (nth index (getf *m68k-layout* :ad)))  ;; An
    (#b010 (list '@  (nth index (getf *m68k-layout* :ad)))) ;; (An)
    (#b011 (list '@+ (nth index (getf *m68k-layout* :ad)))) ;; (An)+
    (#b100 (list '-@ (nth index (getf *m68k-layout* :ad)))) ;; -(An)
    (#b101 (list '@~ (nth index (getf *m68k-layout* :ad)) nil (funcall pa :read 1))) ;; (d16,An)
    (#b110 (unmasque "MXXXS000.DDDDDDDD" (funcall pa :read 1) (m x s d) ;; (d8,An,Xn)
             (list '@~ (nth index (getf *m68k-layout* :ad))
                   (nth x (getf *m68k-layout* :ad)) d)))
    (#b111 (case index
             (#b000 (list '@= (funcall pa :read 1)))         ;; (xxx).W
             (#b001 (list '@= (funcall pa :read 2)))         ;; (xxx).L
             (#b010 (list '@~ :pc nil (funcall pa :read 1))) ;; (d16,PC)
             (#b011 (unmasque "MXXXS000.DDDDDDDD" (funcall pa :read 1) (m x s d) ;; (d8,PC,Xn)
                      (list '@~ :pc (nth x (getf *m68k-layout* :ad)))))
             (#b100 (funcall pa :read 1)))))) ;; #imm

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

;; (defun determine-width (width &optional prefix)
;;   (if prefix
;;       (case width (:b #b01) (:w #b11) (:l #b10))
;;       (case width (:b #b00) (:w #b01) (:l #b10))))

(defun derive-width (width &optional prefix)
  (if prefix
      (case width (#b01 :b) (#b11 :w) (#b10 :l))
      (case width (#b00 :b) (#b01 :w) (#b10 :l))))

;; (defun determine-width-bit (width)
;;   (case width (:w 0) (:l 1)))

(defun derive-width-bit (width)
  (if (zerop width) :w :l))
  
(defun match-imm-width (addressing-mode width immediate)
  (if (not (and addressing-mode (= #b111 addressing-mode)))
      t (zerop (ash (imm-value immediate) (case width (:b -8) (:w -16) (:l -32))))))

(defun qualify-operand (operand type)
  (typecase type
    (symbol (case type
              (nil `(null ,operand))
              (width `(member (wspec-name ,operand) '(:b :w :l)))
              (width-prefix `(member (wspec-name ,operand) '(:b :w :l)))
              (width-bit `(member (wspec-name ,operand) '(:w :l)))
              (gpr `(gpr-p ,operand))
              (adr `(adr-p ,operand))
              (label `(or (gpr-p ,operand) (symbolp ,operand)))
              (mas-all `(typep ,operand 'mas-m68k))
              (mas-all-but-pc `(and (typep ,operand 'mas-m68k)
                                    (not (or (typep ,operand 'mas-bi+disp)
                                             (typep ,operand 'mas-pci+disp)))))
              (vector `(and (integerp (imm-value ,operand))
                            (<= 0 (imm-value ,operand) 15)))
              (reg-list `(listp ,operand)))) ;; this should be more detailed
    (list (destructuring-bind (type &rest qualities) type
            (case type
              (width `(member ,operand ',qualities))
              (imm (case (first qualities)
                     (:width `(and (imm-value ,operand)
                                   (< (imm-value ,operand) ,(expt 2 (second qualities)))))
                     (:range `(and (imm-value ,operand)
                                   (<= ,(second qualities) (imm-value ,operand) ,(third qualities))))))
              (idata `(and (imm-value ,operand)
                           (< (imm-value ,operand) ,(expt 2 (first qualities)))))
              (mas `(and (typep ,operand 'mas-m68k)
                         (or ,@(loop :for q :in qualities :collect `(typep ,operand ',q)))))
              (reg-fixed `(eq (reg-name ,operand) ,(first qualities))))))))

(defun verbalize-operand (spec)
  (flet ((mas-express (mas-type)
           (case mas-type
             (mas-simple "(An)") (mas-postinc "(An)+") (mas-predecr "-(An)") (mas-disp "(d16,An)")
             (mas-bi+disp "(d8,An,Xn)") (mas-abs-w "ABS.W") (mas-abs-l "ABS.L")
             (mas-pc+disp "(d16,PC)") (mas-pci+disp "(d8,PC,Xn)"))))
    (let ((mas-all-list '(mas-simple mas-postinc mas-predecr mas-disp mas-bi+disp
                          mas-abs-w mas-abs-l mas-pc+disp mas-pci+disp))
          (mas-no-pc-list '(mas-simple mas-postinc mas-predecr mas-disp mas-bi+disp mas-abs-w mas-abs-l)))
      (format nil "~a~%" (typecase spec
                           (symbol (case spec
                                     (nil "null")
                                     (width "width : any")
                                     (width-bit "width : B / W")
                                     (width-prefix "width : any")
                                     (gpr "general purpose register : any")
                                     (adr "address register : any")
                                     (label "label")
                                     (mas-all (format nil "memory access : ~{~a ~}"
                                                      (mapcar #'mas-express mas-all-list)))
                                     (mas-all-but-pc (format nil "memory access : ~{~a ~}"
                                                             (mapcar #'mas-express mas-no-pc-list)))
                                     (vector "immediate vector index: 0-15")
                                     (reg-list "register list")))
                           (list (destructuring-bind (type &rest qualities) spec
                                   (case type
                                     (width (format nil "width : ~{~a~^ ~^/ ~}" qualities))
                                     (imm (case (first qualities)
                                            (:width (format nil "immediate value : ~d bits"
                                                            (second qualities)))
                                            (:range (format nil "immediate value: ~d to ~d"
                                                            (second qualities) (third qualities)))))
                                     (idata (format nil "immediate data : ~a bits" (first qualities)))
                                     (mas (format nil "memory access : ~{~a ~}"
                                                  (mapcar #'mas-express qualities)))
                                     (reg-fixed (format nil "~a (fixed register)"
                                                        (first qualities)))))))))))

(defun derive-operand (spec)
  (destructuring-bind (operand &rest types) spec
    (cons 'cond (loop :for type :in types
                    :collect
                    (list (qualify-operand operand type)
                          (typecase type
                            (symbol (case type
                                      (nil nil)
                                      (width `(determine-width (wspec-name ,operand)))
                                      (width-prefix `(determine-width (wspec-name ,operand) t))
                                      (width-bit `(determine-width-bit (wspec-name ,operand)))
                                      ((gpr adr mas-all mas-all-but-pc mas-simple mas-postinc mas-predecr
                                            mas-disp mas-bi+disp mas-pc+disp mas-pci+disp mas-abs)
                                       `(encode-location ,operand))
                                      (label operand)
                                      (vector `(imm-value ,operand))
                                      (reg-list operand)))
                            (list (destructuring-bind (type &rest qualities) type
                                    (declare (ignore qualities))
                                    (case type
                                      (width `(determine-width ,operand))
                                      (imm (list 'imm-value operand))
                                      (idata (list 'encode-location operand))
                                      (mas `(encode-location ,operand))
                                      (reg-fixed `(reg-name ,operand)))))))))))

(defmacro determine (specs &optional bindings &body body)
  "This placeholder macro is not meant to be evaluated but to inform indentation algorithms; (determine) forms will be converted to determine-in-context forms as (specops) forms are expanded."
  (list specs bindings body))

;; (defmethod of-storage ((assembler assembler-m68k) key)
;;   (if (typep key 'mas-m68k)
;;       (values (rix (mas-base key) :ad)
;;               (typecase key (mas-m68k-postinc #b011) (mas-m68k-predecr #b100)
;;                         (t (if (not (mas-displ key))
;;                                #b010 (if (mas-index key) #b110 #b101)))))
;;       ;; (multiple-value-bind (index type) (call-next-method)
;;       ;;   (values index (case type (:gp #b000) (:ad #b001))))
;;       (multiple-value-bind (index type) (rix key)
;;         (values index (case type (:gp #b000) (:ad #b001))))
;;       ))

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

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

(defclass m68k-spregister (m68k-register)
  ())

(defclass m68k-mas (memory-access-scheme)
  ((%qualifier :accessor m68k-mas-qualifier
               :initform nil
               :initarg  :qualifier)
   (%offset-r  :accessor m68k-mas-offset-r
               :initform nil
               :initarg  :offset-r)
   (%offset-i  :accessor m68k-mas-offset-i
               :initform nil
               :initarg  :offset-i)))

(defvar *m68k-storage*)
(defvar *assembler-prototype-m68k*)

(setf *m68k-storage*
      (list :gpr (vector (make-instance 'm68k-gpregister :name :d0 :width 32 :index 0)
                         (make-instance 'm68k-gpregister :name :d1 :width 32 :index 1)
                         (make-instance 'm68k-gpregister :name :d2 :width 32 :index 2)
                         (make-instance 'm68k-gpregister :name :d3 :width 32 :index 3)
                         (make-instance 'm68k-gpregister :name :d4 :width 32 :index 4)
                         (make-instance 'm68k-gpregister :name :d5 :width 32 :index 5)
                         (make-instance 'm68k-gpregister :name :d6 :width 32 :index 6)
                         (make-instance 'm68k-gpregister :name :d7 :width 32 :index 7))
            :adr (vector (make-instance 'm68k-adregister :name :a0 :width 32 :index 0)
                         (make-instance 'm68k-adregister :name :a1 :width 32 :index 1)
                         (make-instance 'm68k-adregister :name :a2 :width 32 :index 2)
                         (make-instance 'm68k-adregister :name :a3 :width 32 :index 3)
                         (make-instance 'm68k-adregister :name :a4 :width 32 :index 4)
                         (make-instance 'm68k-adregister :name :a5 :width 32 :index 5)
                         (make-instance 'm68k-adregister :name :a6 :width 32 :index 6)
                         (make-instance 'm68k-saregister :name :a7 :width 32 :index 7))
            
            :usp (make-instance 'm68k-spregister :name :usp :width 32 :index 0)
            :sr  (make-instance 'm68k-spregister :name :sr  :width 16 :index 0)
            :ccr (make-instance 'm68k-spregister :name :ccr :width 8  :index 0)))

(defclass assembler-m68k (assembler)
  ((%storage :accessor   asm-storage
             :allocation :class
             :initform   (make-hash-table :test #'eq)
             :initarg    :storage)
   (%lexicon :accessor   asm-lexicon
             :allocation :class
             :initform   (make-hash-table :test #'eq)
             :initarg    :lexicon)
   (%domains :accessor   asm-domains
             :initform   nil
             :initarg    :domains)
   (%joiner  :accessor   asm-joiner
             :allocation :class
             :initform   #'join-16
             :initarg    :joiner)))

(defmethod initialize-instance :after ((assembler assembler-m68k) &key)
  (derive-domains assembler (t (:gpr  8) (:adr  8))))

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

;; (let ((this-join (join-spec 16)))
;;   (defun join (&rest items)
;;     (apply this-join items)))

(defmethod locate ((assembler assembler-m68k) asm-sym items)
  (let ((domains (copy-tree (asm-domains assembler)))
        (bound (loop :for item :in items :when (member :bind item :test #'eq) :collect item))
        (unbound (loop :for item :in items :unless (member :bind item :test #'eq) :collect item)))
    ;; (print (list :bu bound unbound domains))
    
    (append (loop :for item :in bound
                  :collect (destructuring-bind (symbol type &rest params) item
                             ;; (print (list :dom domains))
                             ;; (print (list :par params type (getf *m68k-storage* type)))
                             (let ((bind-position (getf params :bind)))
                               (setf (rest (assoc type domains))
                                     (remove bind-position (rest (assoc type domains))))
                               (list symbol `(reserve ,asm-sym ,type ,bind-position)))))
            (loop :for item :in unbound
                  :collect (destructuring-bind (symbol type &rest params) item
                             (list symbol (let ((random-index
                                                  (nth (random (length (rest (assoc type domains))))
                                                       (rest (assoc type domains)))))
                                            (setf (rest (assoc type domains))
                                                  (remove random-index (rest (assoc type domains))))
                                            `(reserve ,asm-sym ,type ,(1+ (ash random-index 1))))))))))

(defmethod reserve ((assembler assembler-m68k) &rest params)
  (destructuring-bind (type index &rest _) params
    (aref (getf *m68k-storage* type) index)))

(defun @+ (operand)
  (if (not (typep operand 'm68k-adregister))
      (error "Memory can only be addressed using an address register.")
      (make-instance 'm68k-mas :base operand :qualifier :post-incr)))

(defun -@ (operand)
  (if (not (typep operand 'm68k-adregister))
      (error "Memory can only be addressed using an address register.")
      (make-instance 'm68k-mas :base operand :qualifier :pre-decr)))

(defun @++ (operand base &optional offset-arg)
  (if (not (typep operand 'm68k-adregister))
      (error "Memory can only be addressed using an address register.")
      (make-instance 'm68k-mas :base operand :offset-r (if offset-arg base nil)
                               :offset-i (or offset-arg base))))

;; (defmethod compose ((assembler assembler-m68k) params expression)
;;   (destructuring-bind (ins &rest ops) expression
;;     (let ((width (if (not (keywordp (first ops)))
;;                      nil (position (first ops) #(:b :w :l) :test #'eq)))
;;           (bindings (rest (assoc :store params))))
;;       (print (list :bi bindings width params ins ops))
;;       (apply (gethash ins (asm-lexicon assembler))
;;              (process-operands bindings ops)))))

;; (print (loop :for o :in ops
;;              :collect (typecase o
;;                         (keyword o)
;;                         (symbol (second (assoc o bindings)))
;;                         (t o))))

#|

(assemble *assembler-prototype-m68k*
  (:with (:store (abc :gpr) (def :gpr :bind :d2)))
  (:addi :b abc 10)
  (:addi :b def 33))

(@+ abc)

(-@ abc)

(@ abc 50)


|#


;; (defun compose-op (&rest params)
;;   (flet ((compose-following (number clauses)
;;            (if (not clauses)
;;                number (if (eq :long (first clauses))
;;                           `(+ (ash ,number 32) ,(second clauses))
;;                           `(+ (ash ,number 16) ,(first clauses))))))
;;     (if (numberp (first params))
;;         (compose-following (first params) (rest params))
;;         (if (stringp (first params))
;;             (let ((string (first params)))
;;               (if (loop :for c :across string :always (position c "01." :test #'char=))
;;                   (let ((base 0)) ;; period is used as a spacer
;;                     (loop :for c :across string :for ix :from 0 :when (not (char= c #\.)) 
;;                           :do (when (char= c #\1) (incf base)) ;; set constant 1 bits
;;                               ;; shift the number to the left unless this is the last digit
;;                               (unless (= ix (1- (length string))) (setf base (ash base 1))))
;;                     (compose-following base (rest params)))
;;                   (compose-following `(masque ,(first params) ,@(second params)) (cddr params))))))))

;; (defmethod specify-ops ((assembler assembler-m68k) asm-sym op-symbol operands params)
;;   (let* ((provisions (rest (assoc :provisions params)))
;;          (ins-part (rest (assoc :instructions params)))
;;          (ins-meta (rest (assoc :with ins-part)))
;;          (ins-main))

;;     (let ((ins-list ins-part))
;;       (loop :while (and ins-list (not ins-main))
;;             :do (if (keywordp (caar ins-list))
;;                     (setf ins-list (rest ins-list))
;;                     (setf ins-main ins-list))))
    
;;     `(setf (gethash ,(intern (string op-symbol) "KEYWORD")
;;                     (asm-lexicon ,asm-sym))
;;            (lambda ,operands ,@params))))

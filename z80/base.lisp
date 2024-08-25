;;;; z80.lisp

(in-package #:specops.z80)

(defclass z80-register (register)
  ())

(defclass z80-gpregister (z80-register)
  ())

(defclass z80-ixregister (z80-register)
  ())

(defclass z80-spregister (z80-register)
  ())

(defclass z80-mem-access ()
  ((%qualifier :accessor m68k-mac-qualifier
               :initform nil
               :initarg  :qualifier)))

(defclass assembler-z80 (assembler)
  ((%storage :accessor   asm-storage
             :allocation :class
             :initform   (make-hash-table :test #'eq)
             :initarg    :storage)
   (%lexicon :accessor   asm-lexicon
             :allocation :class
             :initform   (make-hash-table :test #'eq)
             :initarg    :lexicon)))

(defvar *z80-storage*)

(setf *z80-storage*
      (list :gpr (list :a   (make-instance 'z80-gpregister :name :a   :width  8)
                       :f   (make-instance 'z80-gpregister :name :f   :width  8)
                       :b   (make-instance 'z80-gpregister :name :b   :width  8)
                       :c   (make-instance 'z80-gpregister :name :c   :width  8)
                       :d   (make-instance 'z80-gpregister :name :d   :width  8)
                       :e   (make-instance 'z80-gpregister :name :e   :width  8)
                       :h   (make-instance 'z80-gpregister :name :h   :width  8)
                       :l   (make-instance 'z80-gpregister :name :l   :width  8)
                       :bc  (make-instance 'z80-gpregister :name :bc  :width 16)
                       :de  (make-instance 'z80-gpregister :name :de  :width 16)
                       :hl  (make-instance 'z80-gpregister :name :hl  :width 16))
            :ixr (list :ixl (make-instance 'z80-ixregister :name :ixl :width  8)
                       :iyl (make-instance 'z80-ixregister :name :iyl :width  8)
                       :ix  (make-instance 'z80-ixregister :name :ix  :width 16)
                       :iy  (make-instance 'z80-ixregister :name :iy  :width 16))
            :spr (list :sp  (make-instance 'z80-spregister :name :sp  :width  8)
                       :r   (make-instance 'z80-spregister :name :r   :width  8)
                       :i   (make-instance 'z80-spregister :name :i   :width 16))))

;; (defun process-clause-matrix (table operands matrix)
;;   (let ((clauses) (varops (remove '&optional operands)))
;;     (flet ((matcher (item)
;;              `(match-ops ,@varops ,@(loop :for i :in item
;;                                           :collect (if (not (symbolp i))
;;                                                        i (intern (string i) "KEYWORD"))))))
;;       (symbol-macrolet ((opcode (+ col-index (ash row-index 4))))
;;         (loop :for row :in (rest matrix) :for row-index :from 0
;;               :do (loop :for cell :in (rest row) :for col-index :from 0
;;                         :do (destructuring-bind (&optional ins &rest opr) cell
;;                               (when ins (if (assoc ins clauses)
;;                                             (push (list opr opcode) (rest (assoc ins clauses)))
;;                                             (push (if opr (list ins (list opr opcode))
;;                                                       (list ins opcode))
;;                                                   clauses))))))
;;         (loop :for c :in clauses
;;               :collect (if (numberp (second c))
;;                            `(setf (gethash ,(first c) ,table) ,(second c))
;;                            `(setf (gethash ,(first c) ,table)
;;                                   (lambda ,operands
;;                                     (cond ,@(mapcar (lambda (item)
;;                                                       (list (matcher (first item))
;;                                                             (second item)))
;;                                                     (rest c)))))))))))

;; as an op-matrix, the symbol is swapped out for a prefix

(defun match-ops (op0 op1 val0 &optional val1)
  (print (list op0 op1 val0))
  (and (or (and (typep op0 'z80-gpregister)
                (eq val0 (reg-name op0))))
       (or (and (not op1) (not val1))
           (and (typep op1 'z80-gpregister)
                (eq val1 (reg-name op1))))))

;; (defmethod specify-ops ((assembler assembler-z80) asm-sym prefix operands params)
;;   (let* ((provisions (rest (assoc :provisions params)))
;;          (ins-part (rest (assoc :instructions params)))
;;          (ins-meta (rest (assoc :with ins-part)))
;;          (ins-main))

;;     (let ((ins-list ins-part))
;;       (loop :while (and ins-list (not ins-main))
;;             :do (if (keywordp (caar ins-list))
;;                     (setf ins-list (rest ins-list))
;;                     (setf ins-main ins-list))))

;;     (cons 'progn (process-clause-matrix `(asm-lexicon ,asm-sym)
;;                                         operands (first params)))))

;; `(setf (gethash ,(intern (string op-symbol) "KEYWORD")
;;                 (asm-lexicon ,asm-sym))
;;        (lambda ,operands ;; ,(compose-op params)
;;          ;; ,@params
;;          ,(process-clause-matrix `(asm-lexicon ,asm-sym) (first params))
;;          ))))

(defmethod locate ((assembler assembler-z80) asm-sym items)
  (let ((domains (copy-tree (asm-domains assembler)))
        (bound (loop :for item :in items :when (member :bind item :test #'eq) :collect item))
        (unbound (loop :for item :in items :unless (member :bind item :test #'eq) :collect item)))
    (append (loop :for item :in bound
                  :collect (destructuring-bind (symbol type &rest params) item
                             ;; (print (list :dom domains))
                             (list symbol (let ((out-index (series-index type (getf params :bind))))
                                            (setf (rest (assoc type domains))
                                                  (remove out-index (rest (assoc type domains))))
                                            out-index))))
            (loop :for item :in unbound
                  :collect (destructuring-bind (symbol type &rest params) item
                             (list symbol (let ((random-index
                                                  (nth (random (length (rest (assoc type domains))))
                                                       (rest (assoc type domains)))))
                                            (setf (rest (assoc type domains))
                                                  (remove random-index (rest (assoc type domains))))
                                            random-index)))))))

;; (defmethod compose ((assembler assembler-z80) params expression)
;;   (destructuring-bind (ins &rest ops) expression
;;     (if (listp ins)
;;         (loop :for item :in expression :collect (compose assembler params item))
;;         (if (not (keywordp ins))
;;             (compose assembler params (macroexpand ins))
;;             (let ((bindings (rest (assoc :bindings params))))
;;               ;; (print (list :bi bindings width))
;;               (apply (gethash ins (asm-lexicon assembler))
;;                      (loop :for p :in ops
;;                            :collect (typecase p
;;                                       (symbol
;;                                        ;; (of-storage assembler
;;                                        ;;             :gpr :width width
;;                                        ;;                  :series-index (second (assoc p bindings)))
;;                                        (getf (getf *z80-storage* :gpr)
;;                                              (intern (string p) "KEYWORD"))
;;                                        )
;;                                       (t p)))))))))

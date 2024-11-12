;;;; templates.lisp

(in-package #:specops.m68k)

(setf *assembler-prototype-m68k* (make-instance 'assembler-m68k))

(defun form-process (symbol options form)
  (declare (ignore options))
  (if (eql 'determine (first form))
      (append (list 'determine-in-context
                    (list :qualify #'qualify-operand :verbalize #'verbalize-operand
                          :derive #'derive-operand)
                    symbol)
              (rest form))))

(defmacro specop-68k (symbol operands &body params)
  (let* ((options (and (listp (first params))
                       (listp (caar params))
                       (keywordp (caaar params))
                       (first params)))
         (operations (if options (rest params) params)))
    (cons 'specops (append (list symbol operands '*assembler-prototype-m68k*)
                           (cons (cons (cons :process-forms 'form-process)
                                       options)
                                 operations)))))

;; (defmacro specop (symbol operands &body params)
;;   (specify-ops *assembler-prototype-m68k* '*assembler-prototype-m68k* symbol operands params))

(defmacro readop-68k (symbol args &body body)
  (let ((symbol (macroexpand symbol))
        (function `(lambda ,args (declare (ignorable ,@args)) ,@body)))
    (if (numberp symbol)
        `(of-decoder *assembler-prototype-m68k* ,symbol ,function)
        `(of-battery *assembler-prototype-m68k* ,(intern (string symbol) "KEYWORD") ,function))))

;; (defmacro address (operands bindings &body body)
;;   (labels ((process-level (ops bns)
;;              (if (not ops)
;;                  body `((multiple-value-bind ,(first bns)
;;                             (if (numberp ,(first ops))
;;                                 (list ,(first ops))
;;                                 (of-storage *assembler-prototype-m68k* ,(first ops)))
;;                           ,@(process-level (rest ops) (rest bns)))))))
;;     (first (process-level operands bindings))))

;;;; templates.lisp

(in-package #:specops.m68k)

(setf *assembler-prototype-m68k* (make-instance 'assembler-m68k))

;; (defmacro specop (symbol operands &body params)
;;   (cons 'specops (append (list (intern (string symbol) "KEYWORD")
;;                                operands '*assembler-prototype-m68k*)
;;                          params)))

(defmacro specop (symbol operands &body params)
  (cons 'specops (append (list symbol operands '*assembler-prototype-m68k*) params)))

;; (defmacro specop (symbol operands &body params)
;;   (specify-ops *assembler-prototype-m68k* '*assembler-prototype-m68k* symbol operands params))

(defmacro readop (symbol args &body body)
  (let ((symbol (macroexpand symbol))
        (function `(lambda ,args (declare (ignorable ,@args)) ,@body)))
    (if (numberp symbol)
        `(of-decoder *assembler-prototype-m68k* ,symbol ,function)
        `(of-battery *assembler-prototype-m68k* ,(intern (string symbol) "KEYWORD") ,function))))

(defmacro address (operands bindings &body body)
  (labels ((process-level (ops bns)
             (if (not ops)
                 body `((multiple-value-bind ,(first bns)
                            (if (numberp ,(first ops))
                                (list ,(first ops))
                                (of-storage *assembler-prototype-m68k* ,(first ops)))
                          ,@(process-level (rest ops) (rest bns)))))))
    (first (process-level operands bindings))))

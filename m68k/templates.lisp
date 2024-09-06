;;;; templates.lisp

(in-package #:specops.m68k)

(setf *assembler-prototype-m68k* (make-instance 'assembler-m68k))

(defmacro specop (symbol operands &body params)
  (specify-ops *assembler-prototype-m68k* '*assembler-prototype-m68k* symbol operands params))

(defmacro address (operands bindings &body body)
  (labels ((process-level (ops bns)
             (if (not ops)
                 body `((multiple-value-bind ,(first bns)
                            (of-storage *assembler-prototype-m68k* ,(first ops))
                          ,@(process-level (rest ops) (rest bns)))))))
    (first (process-level operands bindings))))

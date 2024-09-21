;;;; wasm.lisp

(in-package #:specops.wasm)

(defclass assembler-wasm (assembler-encoding)
  ((%lexicon :accessor   asm-lexicon
             :allocation :class
             :initform   (make-hash-table :test #'eq)
             :initarg    :lexicon)
   (%decoder :accessor   asm-enc-decoder
             :allocation :class
             :initform   (make-hash-table :test #'eq)
             :initarg    :decoder)))

;;;; scripts/bench-manifests.lisp — Measurements for doc/Eval.ManifestBenchmarks.md: the cost of
;;;; writing one record with marshal, against packing it by hand and against binary-structures,
;;;; and the gain each remedy would bring, estimated by transforming marshal's expansion
;;;;
;;; Run with: sbcl --noinform --non-interactive --load scripts/bench-manifests.lisp
;;;
;;; Needs Quicklisp, for binary-structures. Each case writes one record a million times, in each
;;; of five runs, and the fastest run is reported, with the bytes allocated per record. The
;;; transformed expansions T1 to T4 are estimates, made only to measure what a change to the
;;; generated code would gain; they are not code the manifest layer generates, and they hold
;;; only for the plain cell, a record of fixed size with no slots, written to a simple vector of
;;; octets.

(ql:quickload "binary-structures" :silent t)
(asdf:load-system :specops/test)

(in-package #:specops-tests)

;;; ------------------------------------------------------------
;;; The records
;;; ------------------------------------------------------------

;; The plain cell of test/test-manifest-arrays.lisp, as written in its worked records: g
;; underlined in the text's colour, the glyph, ink slot and palette slot known only when the
;; code runs.
(defmacro write-plain-cell (buf glyph ink palette)
  `(marshal plain-cell ,buf :glyph-count 2 :type-1 1 :swatch #x1234 :palette ,palette
                            :g0 (:glyph ,glyph :ink ,ink) :g1 (:glyph 9 :operation :union)))

;; The same cell as whole bytes and words, with its bit fields packed by the caller, so that
;; binary-structures, which requires octet-aligned data, and a manifest write the same thing.
(defmanifest cell-bytes (:unit 8 :endian :little)
  (:format :u8 :default 0) (:structure :u8 :default 0) (:flags :u8 :default 0)
  (:parameters :u8 :default 0) (:swatch :u16 :default 0) (:palette :u8 :default 0)
  (:header :u8 :default 0)
  (:g0 :u32 :default 0) (:g1 :u32 :default 0) (:g2 :u32 :default 0)
  (:g3 :u32 :default 0) (:g4 :u32 :default 0) (:g5 :u32 :default 0))

(org.shirakumo.binary-structures:define-io-structure bs-cell
  (format org.shirakumo.binary-structures.types:uint8)
  (structure org.shirakumo.binary-structures.types:uint8)
  (flags org.shirakumo.binary-structures.types:uint8)
  (parameters org.shirakumo.binary-structures.types:uint8)
  (swatch org.shirakumo.binary-structures.types:uint16)
  (palette org.shirakumo.binary-structures.types:uint8)
  (header org.shirakumo.binary-structures.types:uint8)
  (g0 org.shirakumo.binary-structures.types:uint32)
  (g1 org.shirakumo.binary-structures.types:uint32)
  (g2 org.shirakumo.binary-structures.types:uint32)
  (g3 org.shirakumo.binary-structures.types:uint32)
  (g4 org.shirakumo.binary-structures.types:uint32)
  (g5 org.shirakumo.binary-structures.types:uint32))

;;; ------------------------------------------------------------
;;; The expansion and its transformations
;;; ------------------------------------------------------------

(defun proper-list-p (x)
  (and (listp x) (handler-case (list-length x) (error () nil))))

(defun map-tree (function f)
  "FUNCTION applied to each element of F, when F is a proper list; otherwise F."
  (if (proper-list-p f) (mapcar function f) f))

(defun expansion ()
  (macroexpand-1 (macroexpand-1 '(write-plain-cell buf glyph ink palette))))

(defun expansion-parts (e)
  "The variables of marshal's expansion E: the destination, the position, the serializers,
the span map, and the function that enters a unit."
  (let* ((bindings (second e))
         (var (lambda (test) (first (find-if test bindings))))
         (sr (funcall var (lambda (b) (and (consp (second b)) (eq 'make-array (first (second b)))))))
         (ac (funcall var (lambda (b) (eq 'buf (second b)))))
         (in (first (second bindings)))
         (flet-form (find 'flet (cddr e) :key (lambda (f) (and (consp f) (first f)))))
         (en (second (third (first (second flet-form))))))   ; (in-place (limit) (setf en ...))
    (list :sr sr :ac ac :in in :en en)))

(defun t1 (e)
  "The serializers made once, at load time, instead of on each call."
  (destructuring-bind (&key sr &allow-other-keys) (expansion-parts e)
    (let ((setters (remove-if-not (lambda (f) (and (consp f) (eq 'setf (first f))
                                                   (consp (second f)) (eq 'aref (first (second f)))
                                                   (eq sr (second (second f)))))
                                  (cddr e))))
      `(let* ,(mapcar (lambda (b)
                        (if (eq (first b) sr)
                            `(,sr (load-time-value
                                   (let ((,sr ,(second b))) ,@setters ,sr)))
                            b))
                      (second e))
         ,@(remove-if (lambda (f) (member f setters)) (cddr e))))))

(defun t2 (e)
  "As T1, without the map of each field's span, which a manifest with no slots never reads."
  (labels ((map-form-p (f)
             (and (consp f)
                  (or (and (eq 'push (first f)) (consp (second f)) (eq 'list (first (second f))))
                      (and (eq 'setf (first f)) (consp (second f)) (eq 'third (first (second f)))))))
           (strip (f)
             (if (proper-list-p f) (mapcar #'strip (remove-if #'map-form-p f)) f)))
    (strip (t1 e))))

(defun t3 (e)
  "As T2, each field written by code of its own width and byte order, not by the generic
serializer from a fresh (width . value) pair."
  (destructuring-bind (&key sr en &allow-other-keys) (expansion-parts e)
    (labels ((rewrite (f)
               (cond ((and (consp f) (eq 'funcall (first f)) (consp (second f))
                           (eq 'aref (first (second f))) (eq sr (second (second f)))
                           (consp (third f)) (eq 'cons (first (third f))))
                      (destructuring-bind (width value) (rest (third f))
                        (let ((x (gensym "X")))
                          `(let ((,x ,value))
                             (unless (and (typep ,x 'fixnum) (<= 0 ,x ,(1- (ash 1 (* 8 width)))))
                               (error "Value ~a does not fit within ~a unit(s)." ,x ,width))
                             (locally (declare (type fixnum ,x))
                               ,@(loop :for j :below width
                                       :collect `(funcall ,en (ldb (byte 8 ,(* 8 j)) ,x))))))))
                     (t (map-tree #'rewrite f)))))
      (rewrite (t2 e)))))

(defun t4 (e &optional (declare t))
  "As T3, the record's 32 units checked against the vector once, and each unit stored
directly, with the vector and the position declared unless DECLARE is false."
  (destructuring-bind (&key ac in en &allow-other-keys) (expansion-parts e)
    (labels ((rewrite (f)
               (cond ((and (consp f) (eq 'funcall (first f)) (eq en (second f)))
                      `(progn (setf (aref ,ac ,in) ,(third f)) (incf ,in)))
                     (t (map-tree #'rewrite f)))))
      (let ((e3 (t3 e)))
        `(let* ,(second e3)
           ,@(and declare
                  `((declare (type (simple-array (unsigned-byte 8) (*)) ,ac) (type fixnum ,in))))
           (when (> (+ ,in 32) (length ,ac))
             (specops::signal-overrun 'plain-cell (length ,ac) (length ,ac)))
           ,@(rewrite (remove-if (lambda (f) (and (consp f) (eq 'flet (first f)))) (cddr e3))))))))

(defmacro define-writer (name transform)
  `(defun ,name (buf glyph ink palette n)
     (dotimes (i n) ,(funcall transform (expansion)))))

(define-writer write-t1 t1)
(define-writer write-t2 t2)
(define-writer write-t3 t3)
(define-writer write-t4 t4)
(defun t4-undeclared (e) (t4 e nil))
(define-writer write-t4-undeclared t4-undeclared)

;;; ------------------------------------------------------------
;;; The cases
;;; ------------------------------------------------------------

(defun write-by-marshal (buf glyph ink palette n)
  (dotimes (i n) (write-plain-cell buf glyph ink palette)))

(defun write-constants (buf n)
  (dotimes (i n)
    (marshal plain-cell buf :glyph-count 2 :type-1 1 :swatch #x1234 :palette 7
                            :g0 (:glyph 103 :ink 1) :g1 (:glyph 9 :operation :union))))

(defun write-by-hand (buf glyph ink palette n)
  (declare (type (simple-array (unsigned-byte 8) (*)) buf) (fixnum n glyph ink palette)
           (optimize speed))
  (dotimes (i n)
    (when (> 32 (length buf)) (error "Too small."))
    (fill buf 0 :end 32)
    (setf (aref buf 1) #x0A (aref buf 4) #x34 (aref buf 5) #x12 (aref buf 6) palette
          (aref buf 8) (ldb (byte 8 0) glyph) (aref buf 9) (ldb (byte 8 8) glyph)
          (aref buf 10) (ldb (byte 8 16) glyph) (aref buf 11) (ash ink 1)
          (aref buf 12) 9)))

(defun write-bytes-by-marshal (buf glyph ink palette n)
  (dotimes (i n)
    (marshal cell-bytes buf :structure #x0A :swatch #x1234 :palette palette
                            :g0 (logior glyph (ash (ash ink 1) 24)) :g1 9)))

(defun write-bytes-by-binary-structures (buf glyph ink palette n)
  (let ((cell (make-bs-cell :format 0 :structure #x0A :flags 0 :parameters 0 :swatch #x1234
                            :palette 0 :header 0 :g0 0 :g1 9 :g2 0 :g3 0 :g4 0 :g5 0)))
    (dotimes (i n)
      (setf (bs-cell-palette cell) palette
            (bs-cell-g0 cell) (logior glyph (ash (ash ink 1) 24)))
      (write-bs-cell cell buf))))

;;; ------------------------------------------------------------
;;; The runs
;;; ------------------------------------------------------------

(defun measure (thunk n)
  "The fastest of five runs of THUNK over N records, in ns and bytes allocated per record."
  (let ((best nil) (bytes nil))
    (dotimes (run 5)
      (let ((t0 (get-internal-real-time)) (b0 (sb-ext:get-bytes-consed)))
        (funcall thunk)
        (let ((ns (/ (* 1e9 (/ (- (get-internal-real-time) t0) internal-time-units-per-second)) n))
              (b (/ (- (sb-ext:get-bytes-consed) b0) n)))
          (when (or (null best) (< ns best)) (setf best ns bytes b)))))
    (values best bytes)))

(let* ((n 1000000)
       (expected (let ((v (ub8 32))) (write-by-marshal v 103 1 7 1) v))
       (cases
         (list (list "marshal, the plain cell" (lambda (b) (write-by-marshal b 103 1 7 n)))
               (list "marshal, the plain cell, every value a constant" (lambda (b) (write-constants b n)))
               (list "T1: serializers made once" (lambda (b) (write-t1 b 103 1 7 n)))
               (list "T2: and no span map" (lambda (b) (write-t2 b 103 1 7 n)))
               (list "T3: and each field written by its own code" (lambda (b) (write-t3 b 103 1 7 n)))
               (list "T4, without declarations" (lambda (b) (write-t4-undeclared b 103 1 7 n)))
               (list "T4: and one bounds check, direct stores" (lambda (b) (write-t4 b 103 1 7 n)))
               (list "by hand" (lambda (b) (write-by-hand b 103 1 7 n)))
               (list "marshal, the cell as bytes and words" (lambda (b) (write-bytes-by-marshal b 103 1 7 n)))
               (list "binary-structures, the cell as bytes and words"
                     (lambda (b) (write-bytes-by-binary-structures b 103 1 7 n))))))
  (format t "~&~a, ~a~%" (lisp-implementation-type) (lisp-implementation-version))
  (dolist (c cases)
    (destructuring-bind (label fn) c
      (let ((buf (ub8 32)))
        (multiple-value-bind (ns bytes) (measure (lambda () (funcall fn buf)) n)
          (format t "~&~48a ~8,1f ns ~8,1f bytes ~:[(different bytes)~;~]~%"
                  label ns bytes (equalp buf expected)))))))

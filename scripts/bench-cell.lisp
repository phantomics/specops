;;;; scripts/bench-cell.lisp — The cost of writing and reading one of Lexter's cells through a
;;;; manifest, against packing and reading its bytes by hand
;;;
;;; Run with: sbcl --noinform --non-interactive --load scripts/bench-cell.lisp
;;;
;;; The cell is test/test-manifest-arrays.lisp's plain cell holding g underlined in the text's
;;; colour, written with its glyph, ink slot and palette slot known only when the code runs.
;;; Each measurement is a million calls; the times are wall-clock and vary with the machine.

(asdf:load-system :specops/test)

(in-package #:specops-tests)

(defun bench-by-manifest (buf glyph ink palette n)
  (dotimes (i n)
    (marshal plain-cell buf :glyph-count 2 :type-1 1 :swatch #x1234 :palette palette
                            :g0 (:glyph glyph :ink ink) :g1 (:glyph 9 :operation :union))))

(defun bench-by-hand (buf glyph ink palette n)
  (declare (type (simple-array (unsigned-byte 8) (32)) buf) (fixnum n glyph ink palette)
           (optimize speed))
  (dotimes (i n)
    (fill buf 0)
    (setf (aref buf 1) #x0A (aref buf 4) #x34 (aref buf 5) #x12 (aref buf 6) palette
          (aref buf 8) (ldb (byte 8 0) glyph) (aref buf 9) (ldb (byte 8 8) glyph)
          (aref buf 10) (ldb (byte 8 16) glyph) (aref buf 11) (ash ink 1)
          (aref buf 12) 9)))

(defun bench-read-by-manifest (buf n)
  (let ((s 0))
    (dotimes (i n) (incf s (unmarshal plain-cell buf nil :blink)))
    s))

(defun bench-read-by-hand (buf n)
  (declare (type (simple-array (unsigned-byte 8) (32)) buf) (fixnum n) (optimize speed))
  (let ((s 0))
    (declare (fixnum s))
    (dotimes (i n) (incf s (ldb (byte 1 2) (aref buf 2))))
    s))

(let ((a (ub8 32)) (b (ub8 32)) (n 1000000))
  (bench-by-manifest a 103 1 7 1)
  (bench-by-hand b 103 1 7 1)
  (format t "~&The two writes give the same bytes: ~:[no~;yes~]~%" (equalp a b))
  (dolist (f (list (cons "marshal, the full cell" (lambda () (bench-by-manifest a 103 1 7 n)))
                   (cons "by hand, the full cell" (lambda () (bench-by-hand b 103 1 7 n)))
                   (cons "unmarshal, :blink alone" (lambda () (bench-read-by-manifest a n)))
                   (cons "by hand, the blink bit" (lambda () (bench-read-by-hand a n)))))
    (let ((t0 (get-internal-real-time))
          (bytes0 #+sbcl (sb-ext:get-bytes-consed) #-sbcl 0))
      (funcall (cdr f))
      (format t "~&~a: ~,1f ns per cell~@[, ~,1f bytes allocated per cell~]~%" (car f)
              (/ (* 1e9 (/ (- (get-internal-real-time) t0) internal-time-units-per-second)) n)
              #+sbcl (/ (- (sb-ext:get-bytes-consed) bytes0) n) #-sbcl nil))))

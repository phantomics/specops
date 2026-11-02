;;;; test/test-manifest-destinations.lisp — Marshalling to functions, streams and vectors:
;;;; buffers, reuse and overruns. File-stream tests write temporary files, deleted afterwards.

(in-package #:specops-tests)

(in-suite manifest-destinations)

;;; ============================================================
;;; Probe manifests and helpers
;;; ============================================================

(defun file-units (path)
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((v (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence v in)
      v)))

(defun file-units-written (header thunk)
  "Write HEADER to a fresh temporary file, call THUNK with the output stream positioned after
it, and return the file's units. The file is deleted afterwards."
  (uiop:with-temporary-file (:pathname path :type "bin")
    (with-open-file (out path :direction :output :if-exists :supersede
                              :element-type '(unsigned-byte 8))
      (write-sequence header out)
      (funcall thunk out))
    (file-units path)))

;;; ============================================================
;;; Tests
;;; ============================================================

(def-test png-file-to-a-function-the-bytes-written-to-a-vector-and-the-end ()
  "png-file to a function: the bytes written to a vector, and the end position."
  (let* ((png (make-1x1-png))
         (plist (unmarshal png-file png)))
    (is (equalp (list png 72)
                (multiple-value-list (collect-units (lambda (d) (marshal png-file d (:from plist)))))))))

(def-test png-any-to-a-function-the-png-with-a-text-chunk-and-two-idats ()
  "png-any to a function: the PNG with a tEXt chunk and two IDATs."
  (let* ((ancillary (png-with-ancillary-chunks))
         (chunks (unmarshal png-any ancillary)))
    (is (equalp ancillary
                (collect-units (lambda (d) (marshal png-any d (:from chunks))))))))

(def-test count-of-items-probe-and-checked-offset-probe-to-a-function ()
  "count-of-items-probe and checked-offset-probe to a function: count and offset slots."
  (is (equalp '(2 1 0 5 2 0 6 9)
              (coerce (collect-units (lambda (d) (marshal count-of-items-probe d
                                                       :items (list (list :tag 1 :value 5)
                                                                    (list :tag 2 :value 6))
                                                       :after 9)))
                      'list)))
  (is (equalp '(2 7 0 5)
              (coerce (collect-units (lambda (d) (marshal checked-offset-probe d :x 7 :body 5)))
                      'list))))

(def-test png-idat-chunk-to-a-function-312-bytes-the-buffer-grows-past ()
  "png-idat-chunk to a function, 312 bytes: the buffer grows past its first 256."
  (let ((data (->bytes (loop :for i :below 300 :collect (mod i 251)))))
    (is-true (let ((buf (ub8 312)))
               (marshal png-idat-chunk buf :data data)
               (equalp buf (collect-units (lambda (d) (marshal png-idat-chunk d :data data))))))))

(def-test png-file-to-a-file-stream-opened-for-output-only ()
  "png-file to a file stream opened for output only."
  (let* ((png (make-1x1-png))
         (plist (unmarshal png-file png)))
    (is (equalp png
                (file-units-written #() (lambda (s) (marshal png-file s (:from plist))))))))

(def-test png-file-to-a-file-stream-after-a-16-byte-header-slots-land ()
  "png-file to a file stream after a 16-byte header: slots land in the record."
  (let* ((png (make-1x1-png))
         (plist (unmarshal png-file png)))
    (is (equalp (concatenate '(vector (unsigned-byte 8)) (make-array 16 :initial-element #xAA) png)
                (file-units-written (->bytes (make-array 16 :initial-element #xAA))
                         (lambda (s) (marshal png-file s (:from plist))))))))

(def-test png-any-to-a-file-stream-after-a-header-read-back-with-no-failed ()
  "png-any to a file stream after a header, read back with no failed check."
  (let* ((ancillary (png-with-ancillary-chunks))
         (chunks (unmarshal png-any ancillary)))
    (is (equalp '(t nil)
                (let ((file (file-units-written (->bytes #(1 2 3)) (lambda (s) (marshal png-any s (:from chunks))))))
                  (list (equalp (subseq file 3) ancillary)
                        (failed-checks (lambda () (unmarshal png-any (subseq file 3))))))))))

(def-test sized-items-probe-a-length-slot-and-no-checksum-to-a-stream ()
  "sized-items-probe, a length slot and no checksum, to a stream after a header."
  (is (equalp (->bytes #(7 7 3 1 0 5 9))
              (file-units-written (->bytes #(7 7)) (lambda (s) (marshal sized-items-probe s
                                                      :items (list (list :tag 1 :value 5)) :after 9))))))

(def-test item-probe-with-no-slots-to-a-stream-after-a-header-written ()
  "item-probe, with no slots, to a stream after a header: written directly."
  (is (equalp (->bytes #(9 1 0 5))
              (file-units-written (->bytes #(9)) (lambda (s) (marshal item-probe s :tag 1 :value 5))))))

(def-test only-a-manifest-with-a-slot-here-or-nested-is-written-through ()
  "Only a manifest with a slot, here or nested, is written through a buffer."
  (is (equalp '(t t nil nil)
              (mapcar (lambda (form) (tree-mentions-p 'grow-buffer (macroexpand-1 form)))
                      '((marshal png-file d (:from r)) (marshal png-any d (:from r)) (marshal item-probe d)
                        (marshal counted-items-probe d))))))

(def-test a-manifest-buffer-keeps-its-array-across-calls-and-the-bytes ()
  "A manifest-buffer keeps its array across calls, and the bytes are the same."
  (let* ((png (make-1x1-png))
         (plist (unmarshal png-file png))
         (ancillary (png-with-ancillary-chunks))
         (chunks (unmarshal png-any ancillary)))
    (let ((holder (make-manifest-buffer)))
      (is (equalp '(t t t)
                  (let* ((first (collect-units (lambda (d) (marshal png-file d (:from plist :buffer holder)))))
                         (array (manifest-buffer-array holder))
                         (second (collect-units (lambda (d) (marshal png-any d (:from chunks :buffer holder))))))
                    (list (equalp first png) (equalp second ancillary)
                          (eq array (manifest-buffer-array holder)))))))))

(def-test a-manifest-buffer-keeps-the-grown-array-and-a-vector-destination ()
  "A manifest-buffer keeps the grown array, and a vector destination leaves it alone."
  (let ((holder (make-manifest-buffer)))
    (is (equalp '(t t)
                (let ((data (->bytes (make-array 300 :initial-element 7))))
                  (collect-units (lambda (d) (marshal png-idat-chunk d (:buffer holder) :data data)))
                  (let ((grown (manifest-buffer-array holder)))
                    (marshal png-idat-chunk (ub8 312) (:buffer holder) :data data)
                    (list (>= (length grown) 312) (eq grown (manifest-buffer-array holder)))))))))

(def-test png-file-into-50-bytes-signals-manifest-overrun-with-the-size ()
  "png-file into 50 bytes signals manifest-overrun with the size and position."
  (let* ((png (make-1x1-png))
         (plist (unmarshal png-file png)))
    (is (equalp '(t png-file 50 50 t)
                (handler-case (progn (marshal png-file (ub8 50) (:from plist)) :no-error)
                  (manifest-overrun (c) (list (typep c 'error) (overrun-manifest c) (overrun-size c)
                                              (overrun-position c)
                                              (and (search "runs past the end of its destination"
                                                           (format nil "~a" c))
                                                   t))))))))

(def-test item-probe-with-no-slots-into-2-bytes-overruns-too-3-bytes ()
  "item-probe, with no slots, into 2 bytes overruns too; 3 bytes is enough."
  (is (equalp 2
              (handler-case (marshal item-probe (ub8 2) :tag 1 :value 5)
                (manifest-overrun (c) (overrun-position c)))))
  (is (equalp 3
              (marshal item-probe (ub8 3) :tag 1 :value 5))))

(def-test a-vector-with-a-fill-pointer-and-one-of-general-elements-receive ()
  "A vector with a fill pointer, and one of general elements, receive the PNG through a buffer."
  (let* ((png (make-1x1-png))
         (plist (unmarshal png-file png)))
    (is (equalp '(72 0 t 72 t)
                (let ((v (make-array 72 :element-type '(unsigned-byte 8) :fill-pointer 0 :adjustable t))
                      (g (make-array 72)))
                  (list (marshal png-file v (:from plist)) (fill-pointer v)
                        (equalp (loop :for i :below 72 :collect (aref v i)) (coerce png 'list))
                        (marshal png-file g (:from plist)) (equalp g png)))))))

(def-test a-vector-with-a-fill-pointer-is-bounded-by-its-size-an-overrun ()
  "A vector with a fill pointer is bounded by its size: an overrun writes nothing."
  (let* ((png (make-1x1-png))
         (plist (unmarshal png-file png)))
    (is (equalp '((50 50) t)
                (let ((v (make-array 50 :element-type '(unsigned-byte 8) :fill-pointer 0
                                        :initial-element 0)))
                  (list (handler-case (marshal png-file v (:from plist))
                          (manifest-overrun (c) (list (overrun-size c) (overrun-position c))))
                        (every #'zerop (loop :for i :below 50 :collect (aref v i)))))))))

(def-test item-probe-with-no-slots-into-a-vector-with-a-fill-pointer ()
  "item-probe, with no slots, into a vector with a fill pointer: in place, fill pointer kept."
  (is (equalp '(1 (1 0 5))
              (let ((v (make-array 3 :element-type '(unsigned-byte 8) :fill-pointer 1)))
                (marshal item-probe v :tag 1 :value 5)
                (list (fill-pointer v) (loop :for i :below 3 :collect (aref v i)))))))

(def-test a-destination-that-is-not-a-vector-a-stream-or-a-function ()
  "A destination that is not a vector, a stream or a function signals an error."
  (let* ((png (make-1x1-png))
         (plist (unmarshal png-file png)))
    (signals error
      (marshal item-probe 'nowhere :tag 1))
    (signals error
      (marshal png-file 'nowhere (:from plist)))))

(def-test an-unknown-option-of-marshal-is-an-expansion-error ()
  "An unknown option of marshal is an expansion error."
  (signals error
    (macroexpand-1 '(marshal png-file d (:buffers b)))))

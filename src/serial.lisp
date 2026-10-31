;;;; serial.lisp

(in-package #:specops)

(defun find-width (number unit)
  "Return the number of UNIT-bit units needed to represent NUMBER. Non-negative
values are sized by magnitude; negative values are sized for their two's-complement
representation, including the sign bit. Zero has width 0."
  (if (zerop number)
      0 (ceiling (if (minusp number)
                     (1+ (integer-length number))
                     (integer-length number))
                 unit)))

(defun serialize (item unit collector)
  "Serialize a series of integers, integer vectors and/or serial integer specifications into a vector of integers of a given width. A serial integer specification takes the form of a pair of a value and the number of elements it is intended to serialize to."
  (let ((mask (1- (ash 1 unit)))) ;; TODO: find a way to create the mask just once?
    (flet ((decompose (number starting-width)
             (let ((shift (- (* unit (1- starting-width)))))
               (loop :for i :below starting-width
                     :do (funcall collector (logand mask (ash number shift)))
                         (incf shift unit)))))
      (if (integerp item) ;; values that fit within a byte are just pushed on
          (if (zerop (ash item (- unit)))
              (funcall collector item)
              (decompose item (find-width item unit)))
          (if (and (consp item) (not (listp (rest item))))
              ;; handle cons cells encoding width and value, like (3 . 5) → #x000005
              (destructuring-bind (width &rest value) item
                (decompose value width))
              (if (vectorp item)
                  (loop :for i :across item :do (funcall collector i))
                  (error "Attempted to serialize incompatible value - must be an integer, a vector or a pair indicating integer value and encoding width.")))))))

(defun swap-segments (value width granularity)
  "Swap segments of a number for cross-endian encoding. This function supports
swapping at multiple levels of granularity to support systems like the PDP-11,
where words are entered in little-endian order with big-endian byte order
within words."
  (if (zerop granularity)
      value (let* ((span (ash 1 (+ 2 granularity)))
                   (mask (1- (ash 1 span)))
                   (shift 0) (output 0) (count (ash width (- (1- granularity)))))
              (dotimes (i count)
                (incf output (logand mask (ash value shift)))
                (unless (= i (1- count))
                  (setf output (ash output span)))
                (decf shift span))
              output)))

(defun serializer-for (&optional (unit-power 0) (swap-by 0))
  "Serialize a series of integers, integer vectors and/or serial integer specifications
into a vector of integers of a given width. A serial integer specification takes the
form of a pair of a value and the number of elements it is intended to serialize to. This
function can output at multiple unit widths, as determined by the unit-power argument
expressing the (power+3) of 2 corresponding to the width at which output will be generated."
  (let* ((unit (ash 1 (+ 3 unit-power)))
         (mask (1- (ash 1 unit))))

    (flet ((decompose (collector value starting-width)
             (let ((shift (- (- (* unit starting-width) unit))))
               (loop :repeat starting-width
                     :do (funcall collector (logand mask (ash value shift)))
                         (incf shift unit)))))
   
      (lambda (item collector)
        (typecase item
          (integer (if (zerop (ash item (- unit)))
                       ;; values that fit within a unit are sent directly
                       (funcall collector (swap-segments item (ash 1 unit-power) swap-by))
                       (let ((width (find-width item unit)))
                         (decompose collector (swap-segments item (ash width unit-power) swap-by)
                                    width))))
          (cons (if (null (rest item)) ;; nil cdr values cause zero-padding
                    (loop :repeat (first item) :do (funcall collector 0))
                    (if (listp (rest item))
                        (error "Incompatible cons entry.")
                        ;; handle cons cells encoding width and value, like (3 . 5) → #x000005
                        (destructuring-bind (width &rest value) item
                          (when (> (find-width value unit) width)
                            (error "Value ~a does not fit within ~a unit(s) of ~a bits." value width unit))
                          (decompose collector (swap-segments value (ash width unit-power) swap-by)
                                     width)))))
          (vector (let* ((type (array-element-type item))
                         (el-width (and (listp type) (eql 'unsigned-byte (first type))
                                        (second type)))
                         (dc-width (if (= el-width unit)
                                       0 (ash el-width (- (+ 3 unit-power))))))
                    ;; (print (list :ty unit el-width dc-width))
                    (loop :for i :across item
                          :do (if (zerop dc-width) (funcall collector i)
                                  (decompose collector
                                             (swap-segments i (ash dc-width unit-power) swap-by)
                                             dc-width)))))
          (t (error "Attempted to serialize incompatible value - must be an integer, a vector or a pair indicating integer value and encoding width.")))))))

;; (defun make-array-writer (array &key num-width)
;;   (lambda (offset &rest numbers)
;;     (let ((width) (mask) (count 0) (eltype (array-element-type array)))
;;       (if (and (listp eltype) (eql 'unsigned-byte (first eltype))
;;                (member (second eltype) '(8 16 32 64)))
;;           (setf width (second eltype)
;;                 mask  (1- (ash 1 width)))
;;           (error "Invalid collection array; only unsigned arrays of 8, 16, 32 or 64-bit integers may be used."))
;;       (loop :for number :in numbers
;;             :do (let ((num-width (or num-width (find-width number width)))
;;                       (base count))
;;                   (loop :for n :below num-width
;;                         :do (setf (row-major-aref array (+ offset base (- num-width 1 n)))
;;                                   (logand mask number))
;;                             (setf number (ash number (- width)))
;;                             (incf count))))
;;       count)))

;; (defun serialize (item unit collector)
;;   (let ((mask (1- (ash 1 unit)))) ;; TODO: find a way to create the mask just once?
;;     (flet ((decompose (number starting-width)
;;              (print (list :sw starting-width unit))
;;              (let ((shift (- (* unit starting-width))))
;;                (loop :for i :below starting-width
;;                      :do (funcall collector (logand mask (ash number shift)))
;;                          (incf shift unit)))))
;;       (print (list :it item))
;;       (if (integerp item) ;; values that fit within a byte are just pushed on
;;           (if (zerop (ash item (- unit)))
;;               (funcall collector item)
;;               (decompose item (1- (find-width item unit))))
;;           (if (and (consp item) (not (listp (rest item))))
;;               ;; handle cons cells encoding width and value, like (3 . 5) → #x000005
;;               (destructuring-bind (width &rest value) item
;;                 (decompose value (1- width)))
;;               (if (vectorp item)
;;                   (loop :for i :across item :do (funcall collector i))
;;                   (error "Attempted to serialize incompatible value - must be an integer, a vector or a pair indicating integer value and encoding width.")))))))

(defun join-spec (unit items)
  (let ((shift (- unit)) (mask (1- (ash 1 unit))))
      (let ((collected))
        (loop :for item :in items :when item ;; ignore null items
              :do (if (numberp item) ;; values that fit within a byte are just pushed on
                      (if (zerop (ash item shift))
                          (push item collected)
                          (let ((sub-collected))
                            (loop :until (zerop item)
                                  :do (push (logand item mask) sub-collected)
                                      (setf item (ash item shift)))
                            (setf collected (append (reverse sub-collected) collected))))
                      (if (and (consp item) (not (listp (rest item))))
                          ;; handle cons cells encoding width and value, like (3 . 5) → #x000005
                          (destructuring-bind (width &rest value) item
                            (let ((sub-collected))
                              (loop :for i :below width :do (push (logand value mask) sub-collected)
                                                            (setf value (ash value shift)))
                              (setf collected (append (reverse sub-collected) collected))))
                          (when (vectorp item)
                            (loop :for i :across item :do (push i collected))))))
        (make-array (length collected) :element-type (list 'unsigned-byte unit)
                                       :initial-contents (reverse collected)))))

(defun joinb (&rest items)
  (join-spec  8 items))

(defun joinw (&rest items)
  (join-spec 16 items)) 

(defun flipbits (b &optional (n 32))
  (let ((r 0))
    (dotimes (x n r)
      (setq r (logior (ash r 1) (logand b 1))
            b (ash b -1)))))

(defmacro defcodetable (symbol options &rest rows)
  (let ((assignments)
        (blank-symbol (caar rows))
        (table (gensym)) (xtable (gensym)) (char (gensym)) (code (gensym)) (v (gensym))
        (store (rest (assoc :store options))))
    (dolist (row (rest rows))
      (let ((base (first row)))
        (loop :for ix :from 1 :for item :in (rest row) :for increment :in (cdar rows)
              :do (typecase item
                    (symbol)
                    (character
                     (push (+ base increment) assignments)
                     (push (case store
                             (:htable  `(gethash (char-code ,item) ,table))
                             (:vector `(aref ,table (char-code ,item))))
                           assignments))))))
    `(let ((,table ,(case store
                      (:vector `(make-array 256 :initial-element -1 :element-type '(signed-byte 16)))
                      (:htable `(make-hash-table))))
           (,xtable (make-array 256 :initial-element nil)))
       (setf ,@assignments)
       ,(case store
          (:vector `(loop :for x :across ,table :for ix :from 0
                          :unless (= x -1) :do (setf (aref ,xtable x) (code-char ix))))
          (:htable `(loop :for x :being :the :hash-keys :of ,table :for ix :from 0
                          :do (setf (aref ,xtable (gethash x ,table)) (code-char x)))))
       (setf (symbol-function ',symbol)
             (lambda (,char)
               (typecase ,char
                 (character ,(case store
                               (:vector `(let ((,code (char-code ,char)))
                                           (and (< ,code (length ,table))
                                                (let ((,v (aref ,table ,code)))
                                                  (and (/= -1 ,v) ,v)))))
                               (:htable `(gethash (char-code ,char) ,table))))
                 (integer (and (< -1 ,char (length ,xtable))
                               (aref ,xtable ,char)))))))))

(defun quantify-mask-string (string params)
  (let ((segments) (symbols) (factor) (base 0) (bits 0) (width 1) (sx 0) (enumask "01"))
    (when (and (< 1 (length string)) (char= #\: (aref string 1)))
      (incf sx 2) ;; skip reading the type prefix
      ;; bodh for binary, octal, decimal, hexadecimal;
      ;; b is the default case so it's unhandled here
      (case (position (char-upcase (aref string 0)) "BODH" :test #'char=)
        (1 (setf width   3
                 enumask "01234567"))
        (2 (setf factor  10 ;; decimal uses the factor multiplication mode
                 enumask "0123456789"))
        ;; for hexadecimal, uppercase A-F register as digits while lowercase represent symbols
        (3 (setf width   4
                 enumask "0123456789ABCDEF"))))
    (loop :repeat (- (length string) sx)
          :do (let ((char (aref string sx)))
                (if (char= #\. char)
                  (incf sx) ;; period is an ignored spacing character
                  (let ((index (position char enumask :test #'char=)))
                    (if index (progn (when (and symbols (not (null (first symbols))))
                                       (push nil symbols)
                                       (push bits segments))
                                     (incf base index))
                        (when (or (not symbols) (not (eq (intern (string-upcase char) "KEYWORD")
                                                         (first symbols))))
                          (push (intern (string-upcase char) "KEYWORD") symbols)
                          (push bits segments)))
                    (unless (= sx (1- (length string)))
                      (setf base (if factor (* base factor)
                                     (ash base width))))
                    ;; (print (list :bt bits ))
                    (incf bits width)
                    (incf sx)))))
    ;; (print (list :ss segments))
    (let ((segments (if factor (cons 1 (reverse (loop :for s :in segments :collect (expt factor (1- s)))))
                        (cons 0 (loop :for s :in segments :collect (abs (- s bits)))))))
      ;; (print (list :seg segments symbols bits))
      (values (loop :for pair :in (rest (assoc :static params)) ;; values
                    :do (destructuring-bind (sym value) pair
                          (let* ((insym (intern (string-upcase sym) "KEYWORD"))
                                 (index (loop :for s :in symbols :for ix :from 0
                                              :when (eq s insym) :return ix)))
                            ;; (when reverse-match (push insym static-segments))
                            (if index (incf base (ash value (nth index segments)))
                                (error "Invalid key for static base value increment."))))
                    :finally (return base))
              ;; (cons 0 (loop :for s :in segments :collect (abs (- s bits))))
              segments symbols bits))))

(defmacro masque (string &rest assignments)
  (let* ((base-sym (gensym))
         (params (if (listp (caar assignments)) (first assignments) nil))
         (assignments (if (not params) assignments (rest assignments)))
         (processor (rest (assoc :with-processor params))))
    (multiple-value-bind (base segments symbols) (quantify-mask-string string params)
      ;; (print (list :ss segments symbols params))
      ;; (format t "~v,'0B~%" 16 (ash (1- (ash 1 length)) (nth index segments)))

      ;; (print segments)
      (if processor
          (let* ((rev-sym (reverse symbols)) (rev-seg (reverse segments))
                 (big-endian (eq :big (rest (assoc :endian params))))
                 (width (rest (assoc :width params))) (mask (1- (ash 1 width))))
            (cons 'progn
                  (loop :for assignment :in assignments :for sym :in rev-sym
                        :for seg :in rev-seg :for n :from 0
                        :append (destructuring-bind (key value) assignment
                                  (let ((index (position (intern (string-upcase key) "KEYWORD")
                                                         symbols))
                                        (seg-length (if (= n (1- (length assignments)))
                                                        (nth n rev-seg)
                                                        (- (nth n rev-seg) (nth (1+ n) rev-seg)))))
                                    (when (not index)
                                      (error "Symbol ~a not found in mask ~a." key string))
                                    (let ((divs (/ seg-length width)))
                                      ;; (print (list :ee divs length))
                                      (if (= 1 divs)
                                          `((funcall ,processor ,value))
                                          (loop :for d :below divs
                                                :collect `(funcall ,processor
                                                                   (logand (ash ,value
                                                                                ,(- (* 8 (if big-endian
                                                                                             d (- divs 1 d)))))
                                                                           ,mask))))))))))
          (let* ((irregular (= 1 (first segments)))
                 (steps (loop :for assignment :in assignments
                              :collect (destructuring-bind (key value) assignment
                                         (let ((index (position (intern (string-upcase key) "KEYWORD")
                                                                symbols)))
                                           (when (not index)
                                             (error "Symbol ~a not found in mask ~a." key string))
                                           (let ((length (- (nth (1+ index) segments) (nth index segments))))
                                             (if irregular `(incf ,base-sym (* ,value ,(nth index segments)))
                                                 `(setf ,base-sym
                                                        (logior ,base-sym
                                                                (ash (logand ,value ,(1- (ash 1 length)))
                                                                     ,(nth index segments)))))
                                             ))))))
            (if (not steps)
                base `(let ((,base-sym ,base)) ,@steps ,base-sym)))))))

(defmacro unmasque (string test-value params-or-keys &body body)
  (let* ((params (if (listp (first params-or-keys)) params-or-keys nil))
         (keys   (if (not params) params-or-keys (first body)))
         (body   (if (not params) body (rest body))))
    (multiple-value-bind (base segments symbols bits) (quantify-mask-string string params)
      (let* ((variant-mask (1- (ash 1 bits)))
             (pairs (loop :for key :in keys
                          :collect (let* ((index (position (intern (string-upcase key) "KEYWORD")
                                                           symbols))
                                          (length (- (nth (1+ index) segments) (nth index segments))))
                                     (decf variant-mask (ash (1- (ash 1 length)) (nth index segments)))
                                     `(,key (logand ,(1- (ash 1 length))
                                                    (ash ,test-value ,(- (nth index segments)))))))))
        ;; (print (list :vm (format nil "~v,'0B" 16 variant-mask)
        ;;              (format nil "~v,'0B" 16 base)))
        `(if (/= ,base (logand ,variant-mask ,test-value))
             nil (let ,pairs ,@body))))))

(defun deserializer-for (&optional (unit-power 0) (swap-by 0))
  (let ((unit (ash 1 (+ 3 unit-power))))
    (labels ((finish (raw length signed)
               (let ((v    (swap-segments raw (ash length unit-power) swap-by))
                     (bits (* unit length)))
                 (if (and signed (plusp bits) (logbitp (1- bits) v))
                     (- v (ash 1 bits))
                     v)))
             (extract (value units index length signed)
               ;; VALUE holds UNITS units in stream order (most significant first).
               ;; INDEX counts units from the most-significant end; the field spans
               ;; units INDEX .. INDEX+LENGTH-1. START is the field's lowest unit
               ;; counted from the least-significant end.
               (let ((start (- units index length)))
                 (when (or (minusp index) (minusp start))
                   (error "Field at unit ~a (length ~a) exceeds the ~a-unit source."
                          index length units))
                 ;; the extracted bits are the MSB-first composition serializer-for
                 ;; emitted, i.e. (swap-segments value ...); applying it again restores it
                 ;; (funcall (lambda (v)
                 ;;            (if (and signed (logbitp (1- (* unit length)) v))
                 ;;                (- v (ash 1 (* unit length)))
                 ;;                v))
                 (finish (ldb (byte (* unit length) (* unit start)) value) length signed)
                 ;; (swap-segments 
                 ;;                ;; (ldb (byte (* unit length) (* unit start)) value)
                 ;;                (ash length unit-power)
                 ;;                swap-by)
                 )))
      (lambda (index length signed collected)
        (typecase collected
          ;; (units . integer): explicit unit count, so leading zero units are kept
          (cons (destructuring-bind (units . value) collected
                  (unless (integerp value)
                    (error "Source ~a must be (units . integer)." collected))
                  (when (> (find-width value unit) units)
                    (error "Source value ~a exceeds its declared ~a unit(s)." value units))
                  (extract value units index length signed)))
          ;; bare integer: unit count inferred, so leading zero units are lost —
          ;; only reliable when the first unit is nonzero
          (integer (extract collected (find-width collected unit) index length signed))
          ;; vector: length is intrinsic; compose elements MSB-first, then unswap
          (vector
           (let* ((raw 0)
                  (vtype (array-element-type collected))
                  (vunit (if (listp vtype) (second vtype)
                             (error "Incompatible type for collection array."))))
             (when (/= unit vunit)
               (error "Mismatched unit widths.")) ;; add handler later
             (loop :for i :below length
                   :do (setf raw (logior (ash raw unit) (aref collected (+ index i)))))
             ;; (swap-segments raw (ash length unit-power) swap-by)
             (finish raw length signed)
             )))))))

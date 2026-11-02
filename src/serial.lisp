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
    ;; each group's lowest position from the right end, and the top of the last: in bits, or in
    ;; digits for a decimal mask, whose positions are then powers of its factor
    (let ((segments (cons 0 (loop :for s :in segments :collect (abs (- s bits))))))
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
              factor segments symbols bits))))

(defmacro masque (string &rest assignments)
  "Compose a number as represented by an alphanumeric string laying out fields and static sections.
A first argument of parameters may give (:overflow . mode) for a value too wide for its group:
:mask, the default, reduces it to the group's width (two's complement for a negative value
in a binary or hexadecimal mask); :error signals an error, unless the value fits the group
as an unsigned or a two's complement number, which is then masked; NIL leaves it as it is,
for code known to give values that fit."
  (let* ((base-sym (gensym))
         (params (if (listp (caar assignments)) (first assignments) nil))
         (assignments (if (not params) assignments (rest assignments)))
         (processor (rest (assoc :with-processor params)))
         (overflow (let ((entry (assoc :overflow params))) (if entry (rest entry) :mask))))
    (unless (member overflow '(:mask :error nil))
      (error "masque: :overflow must be :mask, :error or NIL, not ~s." overflow))
    (multiple-value-bind (base factor segments symbols) (quantify-mask-string string params)
      ;; (format t "~v,'0B~%" 16 (ash (1- (ash 1 length)) (nth index segments)))

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
          (let ((steps
                  (loop :for assignment :in assignments
                        :collect
                        (destructuring-bind (key value) assignment
                          (let ((index (position (intern (string-upcase key) "KEYWORD") symbols)))
                            (when (not index)
                              (error "Symbol ~a not found in mask ~a." key string))
                            (let* ((low (nth index segments))
                                   (length (- (nth (1+ index) segments) low))
                                   (v (gensym "V"))
                                   ;; the least and greatest values the group holds
                                   (least (if factor 0 (- (ash 1 (1- length)))))
                                   (most (if factor (1- (expt factor length)) (1- (ash 1 length))))
                                   (check `(unless (and (integerp ,v) (<= ,least ,v ,most))
                                             (error "masque: ~s does not fit group ~a of ~s, which holds ~
                                                     ~a to ~a." ,v ',key ,string ,least ,most))))
                              (when (and (eq overflow :error) (integerp value) (not (<= least value most)))
                                (error "masque: ~s does not fit group ~a of ~s, which holds ~a to ~a."
                                       value key string least most))
                              (if factor
                                  ;; a decimal group: a power of the factor scales the value
                                  (let ((scale (expt factor low)) (span (expt factor length)))
                                    (ecase overflow
                                      (:mask `(incf ,base-sym (* (mod ,value ,span) ,scale)))
                                      ((nil) `(incf ,base-sym (* ,value ,scale)))
                                      (:error `(let ((,v ,value))
                                                 ,check
                                                 (incf ,base-sym (* ,v ,scale))))))
                                  (let ((mask (1- (ash 1 length))))
                                    (ecase overflow
                                      (:mask `(setf ,base-sym (logior ,base-sym (ash (logand ,value ,mask) ,low))))
                                      ((nil) `(setf ,base-sym (logior ,base-sym (ash ,value ,low))))
                                      (:error `(let ((,v ,value))
                                                 ,check
                                                 (setf ,base-sym (logior ,base-sym
                                                                         (ash (logand ,v ,mask) ,low))))))))))))))
            (if (not steps)
                base `(let ((,base-sym ,base)) ,@steps ,base-sym)))))))

(defmacro unmasque (string test-value params-or-keys &body body)
  "Decompose a number according to the same type of alphanumeric string informing the (masque) macro."
  (let* ((params (if (listp (first params-or-keys)) params-or-keys nil))
         (keys   (if (not params) params-or-keys (first body)))
         (body   (if (not params) body (rest body)))
         (value  (gensym)))
    (multiple-value-bind (base factor segments symbols bits) (quantify-mask-string string params)
      (let* ((variant-mask (1- (ash 1 bits)))
             (pairs (loop :for key :in keys
                          :collect (let ((index (position (intern (string-upcase key) "KEYWORD")
                                                          symbols)))
                                     (if factor
                                         ;; a decimal group: the digits from its position to the next's
                                         (list key `(floor (mod ,value ,(expt factor (nth (1+ index) segments)))
                                                           ,(expt factor (nth index segments))))
                                         (let ((length (- (nth (1+ index) segments) (nth index segments))))
                                           (decf variant-mask (ash (1- (ash 1 length)) (nth index segments)))
                                           (list key `(logand ,(1- (ash 1 length))
                                                              (ash ,value ,(- (nth index segments)))))))))))
        ;; (print (list :vm (format nil "~v,'0B" 16 variant-mask)
        ;;              (format nil "~v,'0B" 16 base)))
        `(let ((,value ,test-value))
           ,(if factor
                ;; a decimal mask matches when the value less its groups is its constant digits
                `(if (/= ,base (- ,value ,@(loop :for s :in symbols :for i :from 0 :when s
                                                 :collect (let ((low (expt factor (nth i segments)))
                                                                (high (expt factor (nth (1+ i) segments))))
                                                            `(* (floor (mod ,value ,high) ,low) ,low)))))
                     nil (let ,pairs ,@body))
                `(if (/= ,base (logand ,variant-mask ,test-value))
                     nil (let ,pairs ,@body))))))))

(defun grow-buffer (buffer position)
  "A simple array of BUFFER's element type, holding BUFFER's elements, at least twice as
long and long enough to hold the unit at POSITION."
  (replace (make-array (max (* 2 (length buffer)) (1+ position) 16)
                       :element-type (array-element-type buffer))
           buffer))

(defstruct (stream-source (:constructor make-stream-source (stream array)) (:copier nil))
  "A stream read as a source by deserializer-for. The first COUNT elements of ARRAY, a
simple array of units, hold the units read from STREAM so far, numbered from 0 where
reading began. The stream is read only as far as reads need, and never sought."
  stream array (count 0 :type fixnum))

(defun fill-stream-source (source end &optional (eof-error-p t))
  "Read from SOURCE's stream until SOURCE holds END units, and return T. When the stream
ends first, signal an error, or return NIL when EOF-ERROR-P is NIL."
  (let ((count (stream-source-count source)))
    (or (>= count end)
        (let ((array (stream-source-array source)))
          (when (> end (length array))
            (setf array (grow-buffer array (1- end))
                  (stream-source-array source) array))
          (let ((got (read-sequence array (stream-source-stream source) :start count :end end)))
            (setf (stream-source-count source) got)
            (cond ((>= got end) t)
                  (eof-error-p
                   (error "unmarshal: the source stream ends after ~a units, where unit ~a is needed."
                          got (1- end)))
                  (t nil)))))))

(defun source-end-p (source position)
  "True when SOURCE, a vector or a stream source, holds no unit at POSITION."
  (typecase source
    (stream-source (not (fill-stream-source source (1+ position) nil)))
    (t (>= position (length source)))))

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
                 ))
             (from-vector (collected index length signed)
               ;; length is intrinsic; compose elements MSB-first, then unswap
               (let* ((raw 0)
                      (vtype (array-element-type collected))
                      (vunit (if (listp vtype) (second vtype)
                                 (error "Incompatible type for collection array."))))
                 (when (/= unit vunit)
                   (error "Mismatched unit widths.")) ;; add handler later
                 (loop :for i :below length
                       :do (setf raw (logior (ash raw unit) (aref collected (+ index i)))))
                 (finish raw length signed))))
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
          (vector (from-vector collected index length signed))
          ;; a stream: its units are read as far as this read needs, and kept
          (stream-source (fill-stream-source collected (+ index length))
                         (from-vector (stream-source-array collected) index length signed)))))))

;;; IEEE 754 binary floats as bit patterns: half (16), single (32), double (64) and quad
;;; (128) precision. A portable arithmetic path serves every width; SBCL's accessors serve
;;; single and double precision there, keeping every bit pattern, NaN payloads included.

(defun float-format (bits)
  "The exponent and fraction widths of the IEEE binary format of BITS bits, as two values."
  (case bits
    (16  (values 5 10))
    (32  (values 8 23))
    (64  (values 11 52))
    (128 (values 15 112))
    (t (error "No IEEE binary float format is ~a bits wide; there are 16, 32, 64 and 128." bits))))

(defun float-special-p (value)
  "True when VALUE is an infinity or a NaN of the implementation's floats."
  (declare (ignorable value))
  #+sbcl (and (floatp value) (or (sb-ext:float-infinity-p value) (sb-ext:float-nan-p value)))
  #-sbcl nil)

(defun portable-encode-float (value bits &optional name)
  "The bit pattern of VALUE in the IEEE binary format of BITS bits, computed arithmetically:
VALUE is a real, rounded to the nearest representable value, ties to even, or :INFINITY,
:-INFINITY or :NAN. A finite value too large for the format is an error, naming NAME."
  (multiple-value-bind (ew fw) (float-format bits)
    (let* ((bias (1- (ash 1 (1- ew))))
           (top (1- (ash 1 ew)))          ; the exponent field of infinities and NaNs
           (sign-bit (ash 1 (1- bits))))
      (flet ((pattern (sign exponent fraction)
               (logior (if sign sign-bit 0) (ash exponent fw) fraction)))
        (case value
          (:infinity (pattern nil top 0))
          (:-infinity (pattern t top 0))
          (:nan (pattern nil top (ash 1 (1- fw))))
          (t
           (unless (realp value)
             (error "Field ~s: ~s is not a number, :INFINITY, :-INFINITY or :NAN." name value))
           (when (float-special-p value)
             (return-from portable-encode-float
               (portable-encode-float (cond ((not (= value value)) :nan) ; not reached for NaN on SBCL
                                            ((plusp value) :infinity)
                                            (t :-infinity))
                                      bits name)))
           (let* ((sign (if (floatp value) (minusp (float-sign value)) (minusp value)))
                  (r (abs (rational value))))
             (if (zerop r)
                 (pattern sign 0 0)
                 ;; E is the exponent of R's leading bit: 2^E <= R < 2^(E+1)
                 (let ((e (- (integer-length (numerator r)) (integer-length (denominator r)))))
                   (when (< r (expt 2 e)) (decf e))
                   (let* ((normal (>= e (- 1 bias)))
                          (scale (if normal e (- 1 bias)))
                          ;; the significand, its leading bit included, rounded to FW bits
                          (full (round (* r (expt 2 (- fw scale))))))
                     (when (and normal (= full (ash 1 (1+ fw))))   ; rounding carried a bit up
                       (setf full (ash full -1)) (incf e))
                     (let ((exponent (if normal (+ e bias) 0)))
                       (when (>= exponent top)
                         (error "Field ~s: ~s is too large for a ~a-bit float." name value bits))
                       ;; a subnormal that rounded up to 2^FW is the least normal, which
                       ;; the sum gives
                       (+ (pattern sign exponent 0)
                          (if normal (- full (ash 1 fw)) full)))))))))))))

(defun portable-decode-float (pattern bits)
  "The value of PATTERN in the IEEE binary format of BITS bits, computed arithmetically: a
single float for 16 and 32 bits, a double float for 64, and an exact rational for 128; or
:INFINITY, :-INFINITY or :NAN. A negative zero of 128 bits reads as 0."
  (multiple-value-bind (ew fw) (float-format bits)
    (let* ((bias (1- (ash 1 (1- ew))))
           (top (1- (ash 1 ew)))
           (sign (logbitp (1- bits) pattern))
           (exponent (ldb (byte ew fw) pattern))
           (fraction (ldb (byte fw 0) pattern)))
      (if (= exponent top)
          (cond ((plusp fraction) :nan) (sign :-infinity) (t :infinity))
          (let* ((magnitude (if (zerop exponent)
                                (* fraction (expt 2 (- 1 bias fw)))
                                (* (+ (ash 1 fw) fraction) (expt 2 (- exponent bias fw)))))
                 (r (if sign (- magnitude) magnitude)))
            (case bits
              ((16 32) (if (and sign (zerop magnitude)) (- 0f0) (coerce r 'single-float)))
              (64 (if (and sign (zerop magnitude)) (- 0d0) (coerce r 'double-float)))
              (t r)))))))

(defun encode-float-bits (value bits &optional name)
  "The bit pattern of VALUE in the IEEE binary format of BITS bits, for writing; see
portable-encode-float. On SBCL, a float given for single or double precision goes through
its own floats, which keep a NaN's payload. A rational always takes the portable path,
which rounds exactly: SBCL's conversion of a rational to a single float can truncate."
  #+sbcl
  (when (and (member bits '(32 64)) (floatp value))
    (let ((x (handler-case (coerce value (if (= bits 32) 'single-float 'double-float))
               (arithmetic-error ()
                 (error "Field ~s: ~s is too large for a ~a-bit float." name value bits)))))
      (when (and (sb-ext:float-infinity-p x) (not (float-special-p value)))
        (error "Field ~s: ~s is too large for a ~a-bit float." name value bits))
      (return-from encode-float-bits
        (if (= bits 32)
            (ldb (byte 32 0) (sb-kernel:single-float-bits x))
            (logior (ash (ldb (byte 32 0) (sb-kernel:double-float-high-bits x)) 32)
                    (sb-kernel:double-float-low-bits x))))))
  (portable-encode-float value bits name))

(defun decode-float-bits (pattern bits)
  "The value of PATTERN in the IEEE binary format of BITS bits, for reading; see
portable-decode-float. On SBCL, single and double precision give its own floats, so an
infinity or a NaN, its payload included, reads as a float rather than a keyword."
  #+sbcl
  (case bits
    (32 (return-from decode-float-bits
          (sb-kernel:make-single-float (if (logbitp 31 pattern) (- pattern (ash 1 32)) pattern))))
    (64 (return-from decode-float-bits
          (let ((high (ldb (byte 32 32) pattern)))
            (sb-kernel:make-double-float (if (logbitp 31 high) (- high (ash 1 32)) high)
                                         (ldb (byte 32 0) pattern))))))
  (portable-decode-float pattern bits))

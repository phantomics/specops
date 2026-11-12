;;;; src/units.lisp — A code builder for binary values: the forms that store a value's units at
;;;; a target and load them from a source, in the value's order of units, chosen at expansion.
;;;; The manifest layer places these forms in its own code; nothing here knows of manifests.
;;;; Internal until the manifest layer has proven it (SPECOPS-DRAFT-over-arrays-D13).

(in-package #:specops)

;;; Orders. A value's units are numbered by significance, 0 the least significant; an order
;;; gives the significance of each unit in the order the units are stored.

(defun unit-order (endian)
  "The order of units ENDIAN names: :BIG, the most significant unit first; :LITTLE, the least
significant first; or (OUTER INNER GROUP), the units in groups of GROUP, the groups in the
OUTER order and the units of each group in the INNER one. NIL is :BIG, and :PDP the PDP-11's
order, (:BIG :LITTLE 2)."
  (cond ((member endian '(nil :big)) :big)
        ((eq endian :little) :little)
        ((eq endian :pdp) '(:big :little 2))
        ((eq endian :middle)
         (error "The byte order :middle is retired, since \"middle-endian\" names different ~
                 orders: write (:big :little 2), also :pdp, for the PDP-11's order, BADC, or ~
                 (:little :big 2) for the word-swapped order, CDAB."))
        ((and (consp endian) (= 3 (length endian))
              (member (first endian) '(:big :little)) (member (second endian) '(:big :little))
              (integerp (third endian)) (> (third endian) 1))
         endian)
        (t (error "Unknown byte order ~s: it is :big, :little, :pdp, or (outer inner group), ~
                   such as (:big :little 2)." endian))))

(defun unit-significances (width order)
  "The significance of each of the WIDTH units of a value stored in ORDER, in the order they
are stored. A value of no more units than ORDER's group is ordered by the group's order."
  (flet ((run (n direction)
           (if (eq direction :big)
               (loop :for s :from (1- n) :downto 0 :collect s)
               (loop :for s :below n :collect s))))
    (if (keywordp order)
        (run width order)
        (destructuring-bind (outer inner group) order
          (cond ((<= width group) (run width inner))
                ((zerop (mod width group))
                 (loop :for g :in (run (/ width group) outer)
                       :append (mapcar (lambda (s) (+ s (* g group))) (run group inner))))
                (t (error "A value of ~a units cannot be stored in groups of ~a, as the order ~s ~
                           stores it." width group order)))))))

;;; Encodings

(defstruct (encoding (:constructor %make-encoding (width bits order signed)))
  "A binary value of WIDTH units of BITS bits each, stored in ORDER, signed or not."
  width bits order signed)

(defun make-encoding (width bits &key (order :big) signed)
  "An encoding of WIDTH units of BITS bits, in ORDER, any order UNIT-ORDER names."
  (%make-encoding width bits (unit-order order) signed))

(defun encoding-range (encoding)
  "The least and the greatest integer ENCODING stores: two's complement and unsigned values
alike, as the generic serializer accepts."
  (let ((n (* (encoding-width encoding) (encoding-bits encoding))))
    (values (if (zerop n) 0 (- (ash 1 (1- n)))) (1- (ash 1 n)))))

(defun range-error-form (value-form encoding)
  (list 'error "Value ~a does not fit within ~a unit(s) of ~a bits."
        value-form (encoding-width encoding) (encoding-bits encoding)))

;;; Targets: where units are stored. A target's position advances by the units it stores.

(defclass unit-target () ())

(defclass entry-target (unit-target)
  ((enter :initarg :enter :reader target-enter))
  (:documentation "Units entered one at a time through a function of one unit, held by the
variable ENTER, which stores the unit and advances its own position."))

(defclass vector-target (unit-target)
  ((vector :initarg :vector :reader target-vector)
   (index  :initarg :index  :reader target-index))
  (:documentation "Units stored directly into the vector held by the variable VECTOR, at the
position held by the variable INDEX, which the stores advance. Nothing is checked against the
vector's size; an extent check before the stores is the caller's."))

(defgeneric units-store-form (target unit-forms)
  (:documentation "A form storing UNIT-FORMS, one unit each, in turn at TARGET's position."))

(defmethod units-store-form ((target entry-target) unit-forms)
  `(progn ,@(mapcar (lambda (u) `(funcall ,(target-enter target) ,u)) unit-forms)))

(defmethod units-store-form ((target vector-target) unit-forms)
  (let ((v (target-vector target)) (i (target-index target)))
    `(setf ,@(loop :for u :in unit-forms :for k :from 0
                   :append `((aref ,v ,(if (zerop k) i `(+ ,i ,k))) ,u))
           ,i (+ ,i ,(length unit-forms)))))

(defun store-form (target encoding value-form &key (check t))
  "A form storing the value of VALUE-FORM at TARGET as ENCODING gives, its units split out by
significance; an integer is split at expansion. With CHECK, a value that does not fit is an
error, signalled before any unit is stored."
  (let* ((bits (encoding-bits encoding))
         (sigs (unit-significances (encoding-width encoding) (encoding-order encoding))))
    (multiple-value-bind (lo hi) (encoding-range encoding)
      (if (integerp value-form)
          (if (<= lo value-form hi)
              (units-store-form target (mapcar (lambda (s) (ldb (byte bits (* bits s)) value-form))
                                               sigs))
              (range-error-form value-form encoding))
          (let ((x (gensym "X")))
            `(let ((,x ,value-form))
               ,@(and check `((unless (and (integerp ,x) (<= ,lo ,x ,hi))
                                ,(range-error-form x encoding))))
               (locally (declare (type (integer ,lo ,hi) ,x))
                 ,(units-store-form target (mapcar (lambda (s) `(ldb (byte ,bits ,(* bits s)) ,x))
                                                   sigs)))))))))

(defun store-code-form (target bits code-form)
  "A form storing the code CODE-FORM gives, of a character or a terminator: in one unit of BITS
bits when it fits, otherwise in as many as it needs, most significant first, as the generic
serializer stores an integer given no width."
  (let ((c (gensym "C")) (k (gensym "K")))
    `(let ((,c ,code-form))
       (if (and (integerp ,c) (<= 0 ,c ,(1- (ash 1 bits))))
           ,(units-store-form target (list c))
           (loop :for ,k :from (1- (find-width ,c ,bits)) :downto 0
                 :do ,(units-store-form target (list `(ldb (byte ,bits (* ,k ,bits)) ,c))))))))

(defgeneric extent-check-form (target count overrun)
  (:documentation "A form checking that COUNT units fit at TARGET's position, which calls the
function OVERRUN with a form of the target's size, to make the form that reports it."))

(defmethod extent-check-form ((target vector-target) count overrun)
  (let ((v (target-vector target)) (i (target-index target)))
    `(when (> (+ ,i ,count) (length ,v))
       ,(funcall overrun `(length ,v)))))

;;; Sources: where units are loaded from.

(defclass unit-source () ())

(defclass general-source (unit-source)
  ((source     :initarg :source     :reader source-form)
   (unit-power :initarg :unit-power :reader source-unit-power))
  (:documentation "Units loaded from whatever the variable SOURCE holds when the code runs: a
vector, an integer, a (units . integer) pair or a stream source, through the deserializer of
units of 2^(UNIT-POWER+3) bits, in stream order."))

(defclass vector-source (unit-source)
  ((vector :initarg :vector :reader source-vector))
  (:documentation "Units loaded directly from the vector held by the variable VECTOR."))

(defun signed-form (form encoding)
  "FORM, an unsigned value of ENCODING's bits, as a two's complement value when ENCODING is
signed."
  (let ((n (* (encoding-width encoding) (encoding-bits encoding))))
    (if (and (encoding-signed encoding) (plusp n))
        (let ((v (gensym "V")))
          `(let ((,v ,form)) (if (logbitp ,(1- n) ,v) (- ,v ,(ash 1 n)) ,v)))
        form)))

(defgeneric load-form (source encoding position-form)
  (:documentation "A form giving the value ENCODING describes, loaded from SOURCE at the
position POSITION-FORM gives."))

(defmethod load-form ((source general-source) encoding position-form)
  (let* ((w (encoding-width encoding)) (bits (encoding-bits encoding))
         (sigs (unit-significances w (encoding-order encoding)))
         (raw `(funcall (load-time-value (deserializer-for ,(source-unit-power source) 0) t)
                        ,position-form ,w nil ,(source-form source))))
    (signed-form
     (if (equal sigs (loop :for s :from (1- w) :downto 0 :collect s))
         raw
         ;; the units come most significant first, in stream order; each is moved to its place
         (let ((r (gensym "R")))
           `(let ((,r ,raw))
              (logior ,@(loop :for s :in sigs :for k :from 0
                              :collect `(ash (ldb (byte ,bits ,(* bits (- w 1 k))) ,r)
                                             ,(* bits s)))))))
     encoding)))

(defmethod load-form ((source vector-source) encoding position-form)
  (let* ((bits (encoding-bits encoding))
         (sigs (unit-significances (encoding-width encoding) (encoding-order encoding)))
         (p (gensym "P")))
    `(let ((,p ,position-form))
       ,(signed-form
         `(logior ,@(loop :for s :in sigs :for k :from 0
                          :collect `(ash (aref ,(source-vector source) ,(if (zerop k) p `(+ ,p ,k)))
                                         ,(* bits s))))
         encoding))))

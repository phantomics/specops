;;;; specops.lisp

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

;; (defun serialize (item unit collector &optional (swap-granularity 0))
;;   "Serialize a series of integers, integer vectors and/or serial integer specifications into a vector of integers of a given width. A serial integer specification takes the form of a pair of a value and the number of elements it is intended to serialize to."
;;   (when (zerop unit)
;;     (error "Unit cannot be zero."))
;;   (let ((mask (1- (ash 1 unit))) ;; TODO: find a way to create the mask just once?
;;         (increment (* unit (if (zerop swap-granularity) 1 -1)))
;;         (shift-factor (if (zerop swap-granularity) -1 0))
;;         (vshift (let ((u unit))
;;                   (loop :for i :from 1 :do (setf u (ash u -1)) :when (= 1 (logand 1 u)) :return i))))
;;     (flet ((decompose (number starting-width)
;;              (let ((shift (* shift-factor (ash (1- starting-width) vshift))))
;;                (loop :for i :below starting-width
;;                      :do (funcall collector (logand mask (ash number shift)))
;;                          (incf shift increment)))))
;;       (if (integerp item) ;; values that fit within a byte are just pushed on
;;           (if (zerop (ash item (- unit)))
;;               (funcall collector item)
;;               (decompose item (find-width item unit)))
;;           (if (and (consp item) (not (listp (rest item))))
;;               ;; handle cons cells encoding width and value, like (3 . 5) → #x000005
;;               (destructuring-bind (width &rest value) item
;;                 (decompose value width))
;;               (if (vectorp item)
;;                   (let* ((type (array-element-type item))
;;                          ;; number of units to decompose; not needed if the vector's element width
;;                          ;; is the same as the unit width to serialize
;;                          (el-width (and (listp type) (eql 'unsigned-byte (first type))
;;                                         (second type)))
;;                          (dc-width (if (= el-width unit)
;;                                        0 (ash el-width (- vshift)))))
;;                     (loop :for i :across item :do (if (zerop dc-width) (funcall collector i)
;;                                                       (decompose i dc-width))))
;;                   (error "Attempted to serialize incompatible value - must be an integer, a vector or a pair indicating integer value and encoding width.")))))))

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
        (table (gensym)) (xtable (gensym)) (char (gensym))
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
                      (:vector `(make-array 256 :initial-element #x00 :element-type '(unsigned-byte 8)))
                      (:htable `(make-hash-table))))
           (,xtable (make-array 256 :element-type 'character)))
       (setf ,@assignments)
       ,(case store
          (:vector `(loop :for x :across ,table :for ix :from 0 :do (setf (aref ,xtable x) (code-char ix))))
          (:htable `(loop :for x :being :the :hash-keys :of ,table :for ix :from 0
                          :do (when t ; (gethash x ,table)
                                ;; (print (list :aa x))
                                (setf (aref ,xtable (gethash x ,table)) (code-char x))))))
       (setf (symbol-function ',symbol)
             (lambda (,char)
               (typecase ,char
                 (character ,(case store
                               (:vector `(aref ,table (char-code ,char)))
                               (:htable `(gethash (char-code ,char) ,table))))
                 (integer (aref ,xtable ,char))))))))

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

;; (defun quantify-mask-string2 (string params)
;;   (let ((segments) (symbols) (base 0) (bits 0))
;;     (if (char= #\# (aref string 0)) ;; initial # means the string is hexadecimal
;;         (loop :for c :across string :for ix :from 0 :when (not (or (zerop ix)
;;                                                                    (char= c #\.)))
;;               :do (let ((index (position c "0123456789ABCDEF" :test #'char=)))
;;                     ;; symbol-denoting characters must be lowercase when entering hexadecimal strings
;;                     (if index
;;                         (progn (when (and symbols (not (null (first symbols))))
;;                                  (push nil symbols)
;;                                  (push bits segments))
;;                                (incf base index))
;;                         (when (or (not symbols) (not (eq (intern (string-upcase c) "KEYWORD")
;;                                                          (first symbols))))
;;                           (push (intern (string-upcase c) "KEYWORD") symbols)
;;                           (push bits segments)))
;;                     (unless (= ix (1- (length string))) (setf base (ash base 4)))
;;                     (incf bits 4)))
;;         (loop :for c :across string :for ix :from 0 :when (not (char= c #\.)) ;; period is used as a spacer
;;               :do (if (position c "01" :test #'char=)
;;                       (progn (when (and symbols (not (null (first symbols))))
;;                                (push nil symbols)
;;                                (push bits segments))
;;                              (when (char= c #\1) (incf base))) ;; set constant 1 bits
;;                       (when (or (not symbols) (not (eq (intern (string-upcase c) "KEYWORD")
;;                                                        (first symbols))))
;;                         (push (intern (string-upcase c) "KEYWORD") symbols)
;;                         (push bits segments)))
;;                   ;; shift the number to the left unless this is the last digit
;;                   (unless (= ix (1- (length string))) (setf base (ash base 1)))
;;                   (incf bits)))
;;     (let ((segments (cons 0 (loop :for s :in segments :collect (abs (- s bits))))))
;;       ;; (print (list :seg segments symbols bits))
;;       (values (loop :for pair :in (rest (assoc :static params)) ;; values
;;                     :do (destructuring-bind (sym value) pair
;;                           (let* ((insym (intern (string-upcase sym) "KEYWORD"))
;;                                  (index (loop :for s :in symbols :for ix :from 0
;;                                               :when (eq s insym) :return ix)))
;;                             ;; (when reverse-match (push insym static-segments))
;;                             (if index (incf base (ash value (nth index segments)))
;;                                 (error "Invalid key for static base value increment."))))
;;                     :finally (return base))
;;               ;; (cons 0 (loop :for s :in segments :collect (abs (- s bits))))
;;               segments symbols bits))))

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

(defun txcodec-ebcdic (a) (print a))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *manifests* (make-hash-table :test 'eq))
  (defvar *enums*     (make-hash-table :test 'eq)))

(defmacro defenum (name params &body map)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (gethash ',name *enums*)
           ,(append (list 'list (cons 'list params))
                    map))))

;; ===============================================================
;; LEB128 encoding utilities
;; ===============================================================

(defun encode-leb128-unsigned (value writer)
  "Encode a non-negative integer as an unsigned LEB128 byte vector."
  (declare (type (integer 0) value))
  (if (zerop value)
      (funcall writer 0)
      (loop :while (plusp value)
            :do (let ((byte (logand value #x7F)))
                  (setf value (ash value -7))
                  (when (plusp value)
                    (setf byte (logior byte #x80)))
                  (funcall writer byte)))))

(defun encode-leb128-signed (value writer)
  "Encode an integer as a signed LEB128 byte vector."
  (let ((more t))
    (loop :while more
          :do (let ((byte (logand value #x7F)))
                (setf value (ash value -7))
                (if (or (and (zerop value) (zerop (logand byte #x40)))
                        (and (= value -1)  (not (zerop (logand byte #x40)))))
                    (setf more nil)
                    (setf byte (logior byte #x80)))
                (funcall writer byte)))))

(defmacro defmanifest (name params &body fields)
  (destructuring-bind (&key unit endian index vextend-by count-from length pad offset) params
    (let* ((unit-spec (typecase unit
                        (integer (if (< unit 4)
                                     ;; the unit may be an exponent of 2 + 3
                                     unit (multiple-value-bind (root remainder) (floor (log unit 2))
                                            (if (zerop remainder) (- root 3)
                                                (error "Invalid unit.")))))
                        (symbol (case unit (:byte 0) (:word 1) (:doubleword 2) (:quadword 3)))))
           (swap-by (case endian (:big 0) (:little 1) (:middle 2) (:pdp 2)))
           (field-specs (list (list 'list :type :config :unit-spec unit-spec
                                    :pad pad :length length)))
           (index 0))

      (labels ((format-slot-spec (spec)
                 ;; quote functions mentioned in slot specs for proper marshal macroexpansion
                 (cons 'list (loop :for item :in spec
                                   :collect (if (not (and (listp item) (eql 'function (first item))))
                                                item (list 'quote item)))))
               (process-field (field-spec &optional unincrementing)
                 (destructuring-bind (symbol predicate &key (endian 0) length enumerate-by end-by
                                                         count default slot &allow-other-keys)
                     field-spec
                   (let ((width) (signed) (subtype-list) (type-indicator) (type-conditions)
                         (this-swap-by (case endian
                                         (:big 0) (:little 1) (:middle 2) (:pdp 2)
                                         (t endian))))

                     ;; (when slot (setf (getf (cdar field-specs) :has-slots) t))

                     (typecase predicate
                       (null (setf width 1))
                       (keyword (if (zerop unit-spec)
                                    (case predicate
                                      (:u8  (setf width 1 signed nil))
                                      (:s8  (setf width 1 signed t))
                                      (:u16 (setf width 2 signed nil))
                                      (:s16 (setf width 2 signed t))
                                      (:u32 (setf width 4 signed nil))
                                      (:s32 (setf width 4 signed t))
                                      (:u64 (setf width 8 signed nil))
                                      (:s64 (setf width 8 signed t))
                                      (:str (setf width 1)))
                                    (error "Type symbols are only valid for 8-bit output.")))
                       (list (destructuring-bind (is-signed count &rest subtypes) predicate
                               (print (list :cc is-signed))
                               (if (eq :case is-signed)
                                   (setf type-indicator  count
                                         type-conditions
                                         (cons 'list (mapcar (lambda (i) (cons 'list i)) subtypes))
                                         width           1) ;; how to handle right?
                                   (setf width        count
                                         subtype-list subtypes))
                               ;; (print (list :sty subtypes))
                               (when (eq :s is-signed) (setf signed t)))))

                     (unless unincrementing (incf index (ash width (- unit-spec))))
                     (list 'list
                           :name (and (not (string= "_" (symbol-name symbol))) symbol)
                           :signed signed :length (ash width (- unit-spec))
                           :count count :enumerate-by (list 'quote enumerate-by)
                           :slot (format-slot-spec slot)
                           :type-indicator type-indicator :type-conditions type-conditions
                           :subtypes (and subtype-list (cons 'list subtype-list))
                           :end-by end-by :swap-by (or this-swap-by swap-by)
                           :default default))))
               (process-entry (f accumulator)
                 (typecase (first f)
                   (keyword (push (process-field f) accumulator))
                   (symbol (case (first f)
                             (span (let ((sub-items))
                                     (dolist (item (cddr f))
                                       ;; cons the name of the closure to the context list
                                       (setf sub-items (process-entry item sub-items)))
                                     (push (list 'list :type :span :name (second f)
                                                       :items (cons 'list (reverse sub-items)))
                                           accumulator)))
                             (pad (destructuring-bind (with for-or-spec &optional spec-arg) (rest f)
                                    (let ((count (if (integerp for-or-spec)
                                                     for-or-spec (case for-or-spec
                                                                   (:to (- spec-arg index))
                                                                   ;; the align argument lets you align
                                                                   ;; output to a given byte granularity
                                                                   (:align (mod index spec-arg))))))
                                      (incf index count)
                                      (push (list 'list :type :pad :with with :length count
                                                        :upto (and (eq :to for-or-spec) spec-arg)
                                                        :swap-by swap-by)
                                            ;; might need it for nonzero padding
                                            accumulator))))
                             (str (destructuring-bind (actual &key encode-by length terminated-by) (rest f)
                                    (let ((count (* (length actual) (or length 1))))
                                      (push (list 'list :type :string :actual actual :swap-by swap-by
                                                        :terminated-by terminated-by :length count
                                                        :encode-by (list 'quote encode-by))
                                            accumulator)
                                      (incf index count))))
                             (masque (destructuring-bind (spec-string &rest bindings) (rest f)
                                       (let ((bindings-out))
                                         (dolist (b bindings)
                                           (push (if (keywordp (second b))
                                                     (let ((field-out (process-field (rest b) t)))
                                                       (setf (getf (rest field-out) :bind-to)
                                                             (list 'quote (first b)))
                                                       field-out)
                                                     (list 'list :binding (list 'quote b)))
                                                 bindings-out))
                                         
                                         (let* ((masque-width (nth-value 3 (quantify-mask-string
                                                                            spec-string nil)))
                                                (unit-width
                                                  (+ (ash masque-width (- (+ 3 unit-spec)))
                                                     (if (zerop (logand masque-width
                                                                        (1- (ash 1 (+ 3 unit-spec)))))
                                                         0 1))))
                                           ;; increment the index from the actual masque field width;
                                           ;; in the case of an unaligned masque field the output gets
                                           ;; padded, but this would be a mistake, in pracice masques
                                           ;; should always line up with the output unit
                                           (incf index unit-width)
                                           (push (list 'list :type :masque :length unit-width
                                                             :actual spec-string :swap-by swap-by
                                                             :bindings (cons 'list (reverse bindings-out)))
                                                 accumulator)))))
                             (manifest (push (list 'list :type :manifest :name (second f)
                                                         :actual (list 'quote (cddr f)))
                                             accumulator))))
                   (t (error "Invalid clause; may not start with ~a." (first f))))
                 accumulator))
        
        (dolist (f fields) (setf field-specs (process-entry f field-specs)))

        (when (and length (< index length))
          (push (list 'list :type :pad :with (or pad 0) :offset offset
                            :swap-by swap-by :length (- length index))
                field-specs))
        
        `(eval-when (:compile-toplevel :load-toplevel :execute)
           (setf (gethash ',name *manifests*)
                 ,(cons 'list (reverse field-specs))))))))

(defun cs-adapt (item)
  (if (and (vectorp item)
           (typep item '(unsigned-byte 8)))
      item (if (vectorp item)
               (let ((output (make-array (length item) :element-type '(unsigned-byte 8))))
                 (loop :for i :across item :for ix :from 0 :do (setf (aref output ix) i))
                 output))))

(defun find-in-manifest (spec handler)
  (let ((output))
    (loop :for item :in spec :until output
          :do (setf output (if (eq :span (getf item :type))
                               (find-in-manifest (getf item :items) handler)
                               (funcall handler item))))
    output))

(defmacro marshal (name destination &body pairs)
  (let* ((spec (gethash name *manifests*))
         (params (first spec))
         (regions) (slots) (slots-of)
         (swap-specs) (offset)
         (access) (enter) (index) (tell) (write) ; (bindings-sym)
         (values) (length) (serializers) (map) (b) (i)
         ;; (access (gensym "AC")) (enter (gensym "EN")) (index (gensym "IN"))
         ;; (tell (gensym "TL")) (write (gensym "WR")) (bindings-sym (gensym "BN"))
         ;; (values (gensym "VL")) (length (gensym "LN")) (serializers (gensym "SR"))
         ;; (map (gensym "MP")) (b (gensym "BB")) (i (gensym "II"))
         (olength) (sub-lexicon))

    ;; populate the symbol lexicon or import it from an enclosing marshal macro
    (if (getf pairs :-+sub-lexicon+-)
        (destructuring-bind (&key ac-sym en-sym in-sym tl-sym wr-sym bn-sym
                               vl-sym ln-sym sr-sym mp-sym b-sym i-sym)
            (getf pairs :-+sub-lexicon+-)
          (setf access ac-sym enter en-sym index in-sym tell tl-sym write wr-sym ; bindings-sym bn-sym
                values vl-sym length ln-sym serializers sr-sym map mp-sym b b-sym i i-sym))
        (setf access (gensym "AC")
              enter (gensym "EN")
              index (gensym "IN")
              tell (gensym "TL")
              write (gensym "WR")
              ;; bindings-sym (gensym "BN")
              values (gensym "VL")
              length (gensym "LN")
              serializers (gensym "SR")
              map (gensym "MP")
              b (gensym "BB")
              i (gensym "II")
              sub-lexicon (list :ac-sym access :en-sym enter :in-sym index :tl-sym tell
                                :wr-sym write ;; :bn-sym bindings-sym
                                :vl-sym values :ln-sym length
                                :sr-sym serializers :mp-sym map :b-sym b :i-sym i)))

    (setf olength length)

    (setf swap-specs (remove-duplicates swap-specs))
    
    (labels ((preprocess-spec (spec-items)
               (dolist (item spec-items)
                 (destructuring-bind (&key length name swap-by type slot items &allow-other-keys) item
                   ;; build list of swap specs for endian conversion
                   (when swap-by (push swap-by swap-specs))
                   (setf offset (getf item :offset))
                   ;; strings always use the 0-mode swap because they use no endian conversion;
                   ;; endian handling is done within the string codec if necessary
                   ;; since there are many string formats with different endian properties
                   (push name regions)
                   (when slot
                     (push item slots)
                     (push (list name (first slot) length swap-by)
                           (getf slots-of (second slot))))
                   (case type
                     (:span (preprocess-spec items)))
                   (when (eq type :string) (push 0 swap-specs)))))
             (resolve-region (name slot-field slot-type slot-width swap-by)
               `(funcall ,write (aref ,serializers ,swap-by)
                        (cons ,slot-width
                              ;; :length-of → (- end start) ; :offset-of → start
                              ,(case slot-type
                                 (:length-of `(- (third (assoc ,name ,map))
                                                 (second (assoc ,name ,map))))
                                 (:offset-of `(second (assoc ,name ,map)))))
                        (second (assoc ',slot-field ,map)))) ; slot's reserved position
             (assign-serializer (s)
               `(setf (aref ,serializers ,s)
                      (serializer-for ,(getf params :unit-spec) ,s)))
             (masque-binder (b)
               (if (eq :binding (first b))
                   (second b)
                   (destructuring-bind (&key bind-to &allow-other-keys) b
                     (list (getf b :bind-to) (or (getf pairs (getf b :name)) 0)))))
             (enumerate (table-name value)
               (if (not table-name)
                   value (let ((table (gethash table-name *enums*)))
                           (typecase value
                             (keyword (getf (rest table) value))))))
             (generate (item &optional context)
               (destructuring-bind (&key name type kind signed (swap-by 0) upto default offset
                                      count actual bindings with length encode-by end-by
                                      type-indicator type-conditions
                                      slot enumerate-by subtypes items)
                   item
                 
                 ;; (when context (setf name (intern (format nil "~a/~a" (first context) name)
                 ;;                                  "KEYWORD")))
                 
                 ;; (print (list :nn name context))
                 ;; types: ; :scalar | :pad | :masque | :bytes | :sized | :leb | :name | :slot
                 (case type
                   (:pad `(:-pad
                           (loop :repeat ,(if (not upto) length `(max 0 (- ,upto ,index)))
                                 :do (funcall (aref ,serializers ,swap-by) ,with ,enter))
                           (assert (<= ,index ,olength) ()
                                   "Out of bounds.")))
                   (:string `(,name
                              (funcall (aref ,serializers 0)
                                       ;; strings always use 0-swap for reasons given above
                                       (funcall ,(or encode-by '#'identity) ,actual)
                                       ,enter)
                              ,@(when end-by `((funcall (aref ,serializers 0)
                                                        (funcall ,(or encode-by '#'identity) ,actual)
                                                        ,end-by)))))
                   (:masque `(:-masque
                              (funcall (aref ,serializers ,swap-by)
                                       (masque ,actual ,@(mapcar #'masque-binder bindings))
                                       ,enter)))
                   (:manifest
                    (append
                     `((push (list ,name ,index ,index) ,map))
                     (loop :for closure :in context
                           :collect `(unless (assoc ,closure ,map)
                                       (push (list ,closure ,index ,index) ,map)))
                     (list name (macroexpand (append (list 'marshal (first actual) destination)
                                                     (list :-+sub-lexicon+- sub-lexicon)
                                                     (rest actual)
                                                     (getf pairs name))))
                     `((setf (third (assoc ,name ,map)) ,index))
                     (loop :for closure :in context
                           :collect `(setf (third (assoc ,closure ,map)) ,index))
                     (loop :for slot-of :in (getf slots-of name)
                           :when (not (eq :checksum-of (second slot-of)))
                             :collect (apply #'resolve-region name slot-of))))
                   (:span
                    (append (loop :for i :in items :append (generate i (cons name context)))
                            (loop :for slot-of :in (getf slots-of name)
                                  :when (not (eq :checksum-of (second slot-of)))
                                    :collect (apply #'resolve-region name slot-of))))
                   (t (let ((length-form (if type-indicator `(caddr (assoc (getf ,values ,type-indicator)
                                                                           ',type-conditions))
                                             length)))
                        ;; (print (list :oo name type-indicator type-conditions))
                        `(,name
                          (push (list ,name ,index ,index) ,map)
                          ,@(loop :for closure :in context
                                  :collect `(unless (assoc ,closure ,map)
                                              (push (list ,closure ,index ,index) ,map)))

                          ,(cond
                             ((member :leb subtypes)
                              `(funcall (if signed #'encode-leb128-signed #'encode-leb128-unsigned)
                                        (cons ,length-form
                                              ,(or (enumerate enumerate-by (getf pairs name))
                                                   default (error "Field ~a not specified." name)))
                                        ,enter))
                             ((member :vec subtypes)
                              (if (getf pairs name)
                                  `(loop :for e :across ,(getf pairs name)
                                         :do (funcall (aref ,serializers ,swap-by)
                                                      (cons ,length-form e)
                                                      ,enter))
                                  (if length-form
                                      `(loop :for e :below ,length-form
                                             :do (funcall (aref ,serializers ,swap-by)
                                                          (cons ,length-form ,default)
                                                          ,enter))
                                      (error "No content or default specified for vector data."))))
                             ((and slot (eq :checksum-of (first slot)))
                              `(funcall (aref ,serializers ,swap-by)
                                        (cons ,length-form
                                              (destructuring-bind (start end)
                                                  (rest (assoc ,(second slot) ,map))
                                                (funcall ,(getf slot :by) ,access start end)))
                                        ,enter))
                             (t `(funcall (aref ,serializers ,swap-by)
                                          (cons ,length-form
                                                ,(or (enumerate enumerate-by (getf pairs name))
                                                     default
                                                     (and slot 0)
                                                     ;; slot values do not need a default since they 
                                                     ;; will be populated according to other slots' values
                                                     (error "Field ~a not specified." name)))
                                          ,enter)))

                          (setf (third (assoc ,name ,map)) ,index)
                          ,@(and (getf slots-of name)
                                 (loop :for slot-of :in (getf slots-of name)
                                       :when (not (eq :checksum-of (second slot-of)))
                                         :collect (apply #'resolve-region name slot-of)))
                          ,@(loop :for closure :in context
                                  :collect `(setf (third (assoc ,closure ,map)) ,index))
                          (push ,(getf pairs name) ,values)
                          (push ,name ,values)
                          ;; (print (list :bi ,name ,index ,length))

                          )))))))

      (preprocess-spec (rest spec))

      (loop :for (key _) :on slots-of :by #'cddr
            :unless (member key regions)
              :do (error "Slot reference found with no matching region."))
      
      (append (if (getf pairs :-+sub-lexicon+-) `(progn)
                  `(let* ((,access ,destination)
                          (,index ,(or offset 0))
                          (,values) ;; (,bindings-sym)
                          ,@(if (getf params :length) `((,length ,(getf params :length))))
                          (,enter) (,tell) (,write) (,map)
                          (,serializers (make-array ,(1+ (reduce #'max swap-specs)) :initial-element nil)))

                     (typecase ,access
                       (function (setf ,enter (lambda (,b) (funcall ,access ,b))
                                       ,tell  (lambda () ,index)))
                       (vector   (setf ,enter (lambda (,b) (setf (aref ,access ,index) ,b) (incf ,index))
                                       ,tell  (lambda () ,index)
                                       ,write (lambda (serializer datum position)
                                                (let ((opos ,index))
                                                  (setf ,index position)
                                                  (funcall serializer datum ,enter)
                                                  (setf ,index opos)))))
                       (stream   (setf ,enter (lambda (,b) (incf ,index) (write-byte ,b ,access))
                                       ,tell  (lambda () (file-position ,access))
                                       ,write (lambda (serializer datum position)
                                                (let ((opos (file-position ,access)))
                                                  (file-position ,access position)
                                                  (setf ,index position)
                                                  (funcall serializer datum ,enter)
                                                  (file-position ,access opos)
                                                  (setf ,index opos))))))))

              (unless (getf pairs :-+sub-lexicon+-)
                (mapcar #'assign-serializer swap-specs))
              
              (if spec (apply #'append (mapcar #'generate (rest spec)))
                    (error "Manifest not found."))

              ;; `((print (list :mmm ,map)))
              
              (list index)
              ;; (print (list :v ,values))
              ))))

#|

;; --- enums (work today) ---
(defenum png-color-type nil :grayscale 0 :rgb 2 :palette 3 :grayscale-alpha 4 :rgba 6)
(defenum png-interlace  nil :none 0 :adam7 1)

;; --- IHDR body, 13 bytes (works today as a standalone marshal) ---
(defmanifest png-ihdr (:unit 8 :endian :big)
  (:width       (:u 4) :default 0)
  (:height      (:u 4) :default 0)
  (:bit-depth   :u8    :default 8)
  (:color-type  :u8    :default 0 :enumerate-by png-color-type)
  (:compression :u8    :default 0)
  (:filter      :u8    :default 0)
  (:interlace   :u8    :default 0 :enumerate-by png-interlace))

;; --- generic chunk: opaque data (span + checksum work; :length-of waits on the patch fix) ---
(defmanifest png-chunk (:unit 8 :endian :big)
  (:length (:u 4) :slot (:length-of :data))                 ; ⚠ patch-before, not yet functional
  (span :type+data
    (:type (:u 1 :vec))                                     ; 4-byte tag, caller-supplied
    (:data (:u 1 :vec)))                                    ; opaque bytes
  (:crc (:u 4) :default 0 :slot (:checksum-of :type+data :by #'crc))) ; ✓ works

;; --- IHDR chunk: body via sub-manifest (waits on sub-manifest fixes #1/#2) ---
(defmanifest png-ihdr-chunk (:unit 8 :endian :big)
  (:length (:u 4) :slot (:length-of :ihdr))                 ; = 13
  (span :type+data
    (:type (:u 1 :vec))                                     ; #(73 72 68 82) "IHDR"
    (manifest :png-ihdr png-ihdr))                          ; ⚠ needs :name + no-let* inlining
  (:crc (:u 4) :default 0 :slot (:checksum-of :type+data :by #'crc)))

;; --- whole file (waits on sub-manifest fixes) ---
(defmanifest png-file (:unit 8 :endian :big)
  (:signature (:u 1 :vec))                                  ; caller passes the 8 magic bytes
  (manifest :ihdr png-ihdr-chunk)
  (manifest :idat png-idat-chunk)                           ; IDAT = opaque zlib blob
  ;; IEND, empty data
  (manifest :iend png-iend-chunk))

Intended call once the fixes land:
(marshal png-ihdr-chunk buf
  :type #(73 72 68 82)
  :png-ihdr (:width 1 :height 1 :bit-depth 8 :color-type :rgb))

|#


;; #(55 12 0 0 0 0 5 0 2 0 0 0 0 0 0 0 0 0 0 5 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
;;   0 0 0 0 0 0 0 0 0 0 20 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
;;   0 0 0 0)

;; #(55 12 0 0 0 0 5 0 2 0 0 0 0 0 0 0 0 0 0 5 4 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
;;   0 0 0 0 0 0 0 0 0 0 20 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
;;   0 0 0 0)

;; #(55 12 0 0 0 0 5 0 2 0 0 0 0 0 0 0 0 0 0 5 0 0 0 4 0 0 0 0 0 0 0 0 0 0 0 0 0 0
;;   0 0 0 0 0 0 0 0 0 0 20 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
;;   0 0 0 0)

(defmacro unmarshal (name destination &rest keys)
  (let ((fn-sym (find-symbol (format nil "⍁UNMARSHAL-COMPOSE-~a" name)
                             (package-name (symbol-package name)))))
    (if (fboundp fn-sym)
        (apply (symbol-function fn-sym) destination keys)
        (error "Manifest not found."))))
#|

(macroexpand `(defmanifest png-chunk (:unit 8 :endian :big)
                (:length (:u 4) :slot (:length-of :data))
                (span :type+data
                      (:type (:u 1 :vec))
                      (:data (:u 1 :vec)))
                (:crc (:u 4) :slot (:checksum-of :type+data :by #'crc))))

|#


;; (defmacro make-masquer (string &rest assignments)
;;   (let* ((params (if (listp (caar assignments)) (first assignments) nil))
;;          (assignments (if (not params) assignments (rest assignments)))
;;          (width (rest (assoc :width params)))
;;          (mask  (1- (ash 1 width))))
;;     `(lambda (array offset)
;;        (let ((bound (array-total-size array)))
;;          ,(multiple-value-bind (base segments symbols) (quantify-mask-string string nil)
;;             (print (list :ss base segments symbols))
;;             ;; (format t "~v,'0B~%" 16 (ash (1- (ash 1 length)) (nth index segments)))

;;             (let* ((base-sym 'a)
;;                    (rev-sym (reverse symbols))
;;                    (rev-seg (reverse segments))
;;                    (assign -1)
;;                    (steps (loop :for assignment :in assignments :for sym :in rev-sym
;;                                 :for seg :in rev-seg :for n :from 0
;;                                 :append (destructuring-bind (key value) assignment
;;                                           (let ((index (position (intern (string-upcase key) "KEYWORD")
;;                                                                  symbols))
;;                                                 (seg-length (if (= n (1- (length assignments)))
;;                                                                 (nth n rev-seg)
;;                                                                 (- (nth n rev-seg) (nth (1+ n) rev-seg)))))
;;                                             (when (not index)
;;                                               (error "Symbol ~a not found in mask ~a." key string))
;;                                             (let ((length (- (nth (1+ index) segments)
;;                                                              (nth index segments)))
;;                                                   (divs (/ seg-length width)))
;;                                               (print (list :ee divs length))
;;                                               (if (= 1 divs)
;;                                                   `((setf (aref array offset) ,value)
;;                                                     (bounded-iterate offset bound))
;;                                                   (loop :for d :below divs
;;                                                         :append `((setf (aref array offset)
;;                                                                         (logand (ash ,value ,(- (* 8 d)))
;;                                                                                 ,mask))
;;                                                                   (bounded-iterate offset bound))))))))))
;;               (cond ((not steps) base)
;;                     (t (cons 'progn steps))
;;                     (t `(let ((,base-sym ,base)) ,@steps ,base-sym)))))))))


;; (defmacro mqbase (name opcsym mnesym args string &body body)
;;   ;; this is a currying macro for masque, allowing the creation
;;   ;; of a specialized masque with static contents for some fields
;;   (destructuring-bind (static-specs clauses &optional disassembler) body
;;     (let ((dsarg (gensym)) (flargs (gensym))
;;           (static-form (loop :for ss :in static-specs
;;                              :collect (if (not (eq :static (first ss)))
;;                                           (list 'quote ss)
;;                                           `(list :static ,@(loop :for spec :in (rest ss)
;;                                                                  :collect `(list ',(first spec)
;;                                                                                  ,(second spec))))))))
;;       `(defmacro ,name (,opcsym ,mnesym)
;;          (list (list 'lambda ',(cons 'program-api args) ;; TODO - gensym for args
;;                      (list 'flet '((of-program (&rest ,flargs) (apply program-api ,flargs)))
;;                            '(declare (ignorable (function of-program)))
;;                            (append (list 'masque ,string)
;;                                    ;; generate a template incorporating the :static values
;;                                    ;; into a (masque) expansion within the defined macro
;;                                    (list (list ,@static-form))
;;                                    ',clauses)))
;;                ,@(if (not disassembler)
;;                      nil `((list 'lambda (list ',dsarg 'program-api)
;;                                  ;; '(print (list :ee program-api ,dsarg))
;;                                  (list 'flet '((of-program (&rest ,flargs) (apply program-api ,flargs)))
;;                                        '(declare (ignorable (function of-program)))
;;                                        (list 'unmasque ,string ',dsarg (list ,@static-form)
;;                                              ',(mapcar #'first clauses) ;; ',disassembler
;;                                              (list ,@(loop :for item :in disassembler
;;                                                            :collect (if (and (symbolp item)
;;                                                                              (eql item mnesym))
;;                                                                         `(intern (string ,item) "KEYWORD")
;;                                                                         `(quote ,item))))))))))))))

(defmacro mqbase (name opcsym mnesym args string &body body)
  "This is a currying macro for dual functions based on masque and unmasque, allowing the creation of specialized functions to compose and decompose binary values in a consistent way across many permutations. It was originally created to compose IBM System Z functions, with its design facilitated by System Z's consistent instruction formats."
  (destructuring-bind (params clauses &optional disassembler) body
    (let ((dsarg (gensym)) (flargs (gensym))
          (static-form (loop :for ss :in params
                             :collect (if (not (eq :static (first ss)))
                                          (list 'quote ss)
                                          `(list :static ,@(loop :for spec :in (rest ss)
                                                                 :collect `(list ',(first spec)
                                                                                 ,(second spec)))))))
          (determine-format (if (not (assoc :determine-by params))
                                #'list (destructuring-bind (macro-sym schemes bindings)
                                           (rest (assoc :determine-by params))
                                         #'(lambda (body)
                                           `((list ',macro-sym ,mnesym ',schemes ',bindings ,body)))))))
      `(defmacro ,name (,opcsym ,mnesym)
         (list (list 'lambda ',(cons 'program-api args) ;; TODO - gensym for args
                     ,(append `(list 'flet '((of-program (&rest ,flargs) (apply program-api ,flargs)))
                                     '(declare (ignorable (function of-program))))
                              (funcall determine-format
                                       `(append (list 'masque ,string)
                                                ;; generate a template incorporating the :static values
                                                ;; into a (masque) expansion within the defined macro
                                                (list (list ,@static-form))
                                                ',clauses))))
               ,@(if (not disassembler)
                     nil `((list 'lambda (list ',dsarg 'program-api)
                                 ;; '(print (list :ee program-api ,dsarg))
                                 (list 'flet '((of-program (&rest ,flargs) (apply program-api ,flargs)))
                                       '(declare (ignorable (function of-program)))
                                       (list 'unmasque ,string ',dsarg (list ,@static-form)
                                             ',(mapcar #'first clauses) ;; ',disassembler
                                             (list ,@(loop :for item :in disassembler
                                                           :collect (if (and (symbolp item)
                                                                             (eql item mnesym))
                                                                        `(intern (string ,item) "KEYWORD")
                                                                        `(quote ,item))))))))))))))

(defclass width-spec ()
  ((%name :accessor wspec-name
          :initform nil
          :initarg  :name
          :documentation "The spec's name.")
   (%type :accessor wspec-bits
          :initform nil
          :initarg  :type
          :documentation "The spec's width in bits."))
  (:documentation "Generic class for width specs."))

(defmethod wspec-name ((name t))
  "For width specs modeled using symbols, (reg-name) simply returns the symbol."
  nil)

(defmethod wspec-name ((name symbol))
  "For width specs modeled using symbols, (reg-name) simply returns the symbol."
  name)

(defclass register ()
  ((%name  :accessor reg-name
           :initform nil
           :initarg  :name
           :documentation "The register's name.")
   (%type  :accessor reg-type
           :initform nil
           :initarg  :type
           :documentation "The register's type.")
   (%index :accessor reg-index
           :initform nil
           :initarg  :index
           :documentation "The register's index."))
  (:documentation "Generic class for registers."))

(defmethod reg-name ((register t))
  "Objects not recognized as registers return nil for a register name check."
  (declare (ignore register))
  nil)

(defmethod reg-name ((register symbol))
  "For registers modeled using symbols, (reg-name) simply returns the symbol."
  register)
  
(defgeneric of-register-type-index (register &optional type)
  (:documentation "Get a register's index."))

(defmethod of-register-type-index ((register register) &optional type)
  (if (or (not type) (eq type (reg-type register)))
      (values (reg-index register) (or type (reg-type register)))))

(defclass immediate ()
  ((%value :accessor imm-value
           :initform nil
           :initarg  :name
           :documentation "The actual value.")
   (%type  :accessor imm-type
           :initform nil
           :initarg  :type
           :documentation "The value's type.")
   (%width :accessor imm-width
           :initform nil
           :initarg  :width
           :documentation "The value's width in bits."))
  (:documentation "Base class for immediate-values."))

(defmethod imm-value ((value t))
  "Immediates not recognised as a number return nil when their value is checked."
  (declare (ignore value))
  nil)

(defmethod imm-value ((value number))
  "For immediates modeled as plain numbers, (imm-value) simply returns the number."
  value)

(defgeneric make-immediate (value &optional type width)
  (:documentation "Fucntion to create an immediate value."))

;; (defgeneric imm (value &rest type)
;;   (:documentation "Get a register's index."))

(defmethod make-immediate (value &optional type width)
  (when width
    (typecase value
      (integer (assert (and (not (minusp value))
                            (zerop (ash value (- width))))
                       (value width) "Immediate integer value ~a overflows its width of ~a."))))
  (make-instance 'immediate :width width :value (if (integerp value) value)
                            :type (or type (typecase value
                                             (integer 'integer)))))

;; (defmethod imm ((value integer) &rest type)
;;   (if (not type) value))

;; (defmethod imm ((value immediate) &rest type)
;;   (declare (ignore type))
;;   (imm-value value))

(defclass memory-access-scheme ()
  () (:documentation "Base class for memory access schemes."))

(defclass mas-based (memory-access-scheme)
  ((%base :accessor mas-base
          :initform nil
          :initarg  :base))
  (:documentation "A class encompassing memory access schemes with a base register."))

(defclass mas-indexed (memory-access-scheme)
  ((%index :accessor mas-index
           :initform nil
           :initarg  :index))
  (:documentation "A class encompassing memory access schemes with an index register."))

(defclass mas-displaced (memory-access-scheme)
  ((%displ :accessor mas-displ
           :initform nil
           :initarg  :displ))
  (:documentation "A class encompassing memory access schemes with a displacement value."))

(defclass mas-scaling-displaced (mas-displaced)
  ((%scale :accessor mas-sdisp-scale
           :initform nil
           :initarg  :scale))
  (:documentation "A class encompassing memory access schemes with a displacement value that may be scaled; i.e. multiplied by a power of two."))

(defclass mas-absolute (memory-access-scheme)
  ((%addr :accessor mas-addr
          :initform nil
          :initarg  :addr))
  (:documentation "A class encompassing memory access schemes referencing an absolute address."))

(defclass register-file ()
  ((%name   :accessor rfile-name
            :initform nil
            :initarg  :name)
   (%base   :accessor rfile-base
            :initform nil
            :initarg  :source)
   (%tags   :accessor rfile-tags
            :initform nil
            :initarg  :tags)
   (%width  :accessor rfile-width
            :initform nil
            :initarg  :width)
   (%widths :accessor rfile-widths
            :initform nil
            :initarg  :widths)))

(defclass rfile-reduced (register-file)
  ((%map  :accessor rf-reduced-map
          :initform nil
          :initarg  :map)))

(defclass rfile-displaced (register-file)
  ((%bit-width  :accessor rf-displaced-bit-width
                :initform nil
                :initarg  :bit-width)
   (%bit-offset :accessor rf-displaced-bit-offset
                :initform nil
                :initarg  :bit-offset)))

(defclass rfile-blocked (register-file)
  ((%map  :accessor rf-reduced-map
          :initform nil
          :initarg  :map)))

(defmacro as-register-file (name props &body forms)
  (destructuring-bind (tags &rest derivations) forms
    (let ((base (gensym)) (syms))
      `(let ((,base (make-instance 'register-file ,@(append props (list :tags tags))))
             ,@(loop :for d :in derivations
                     :collect (let ((assign (gensym)))
                                (push assign syms)
                                (setf (fourth d) (append (list :base base) (fourth d)))
                                (list assign (macroexpand (append d))))))
         (list ,base ,@(reverse syms))))))

(defmacro deriving-file (type name props &body forms)
  `(make-instance ',(case type (:reduced 'rfile-reduced) (:displaced 'rfile-displaced))
                  :name ,name ,@props))

(defclass instruction ()
  ((%mnem :accessor inst-mnem
          :initform nil
          :initarg  :mnem
          :documentation "Mnemonic for the instruction.")
   (%oper :accessor inst-oper
          :initform nil
          :initarg  :oper
          :documentation "Operands taken by the instruction.")
   (%span :accessor inst-span
          :initform nil
          :initarg  :span
          :documentation "Width of data addressed by the instruction.")
   (%extn :accessor inst-extn
          :initform nil  
          :initarg  :extn
          :documentation "ISA extension(s) within which the instruction is available."))
  (:documentation "A generic instruction class."))

(defclass assembler ()
  ((%name    :accessor   asm-name
             :initform   nil
             :initarg    :name)
   (%extns   :accessor   asm-extns
             :allocation :class
             :initform   nil
             :initarg    :extns)
   (%storage :accessor   asm-storage
             :initform   nil
             :initarg    :storage)
   (%lexicon :accessor   asm-lexicon
             :initform   nil
             :initarg    :lexicon)
   (%pmodel  :accessor   asm-pmodel
             :initform   nil
             :initarg    :pmodel
             :documentation "The program model: collected register files.")
   (%pool    :accessor   asm-pool
             :documentation "Pool of available registers, to be solved for after initialization.")
   (%reserve :accessor   asm-reserve
             :initform   nil
             :initarg    :reserve)
   (%breadth :accessor   asm-breadth
             :allocation :class
             :initform   16
             :initarg    :breadth)
   ;; (%joiner  :accessor   asm-joiner
   ;;           :allocation :class
   ;;           :initform   #'joinb
   ;;           :initarg    :joiner)
   (%exmodes :accessor   asm-exmodes
             :initform   nil
             :initarg    :exmodes)
   (%pro-api :accessor   asm-program-api
             :allocation :class
             :initform   'program-api
             :initarg    :program-api)
   (%pro-aac :accessor   asm-program-api-access
             :allocation :class
             :initform   'of-program
             :initarg    :program-api-access))
  (:documentation "A generic assembler class."))

(defmethod initialize-instance :after ((asm assembler) &key)
  "After-init method for assemblers; builds register pool exclusing blocked registers."
  (let ((pool (make-hash-table :test #'eq))
        (blocked))
    (dolist (file (asm-pmodel asm))
      (typecase file
        (rfile-blocked (loop :for tag :in (rfile-tags file) :unless (member tag blocked)
                             :do (push tag blocked)))))
    (dolist (file (asm-pmodel asm))
      (unless (typep file 'rfile-blocked)
        (loop :for tag :in (rfile-tags file)
              :for index :from 0
              :unless (member tag blocked)
                :do (setf (gethash tag pool)
                          (cons file index)))))
    (setf (asm-pool asm) pool)))

(defclass assembler-encoding (assembler)
  ((%decoder :accessor asm-enc-decoder
             :initform nil
             :initarg  :decoder))
  (:documentation "An assembler with relatively simple correspondences between instruction and encoding (such as z80 or 6502) such that many or most instructions can be disassembled through simple lookups."))

(defclass assembler-masking (assembler)
  ((%segment :accessor asm-msk-segment
             :initform nil
             :initarg  :segment)
   (%battery :accessor asm-msk-battery
             :initform nil
             :initarg  :battery))
  (:documentation "An assembler whose instructions can be disassembled on the basis of comparing and decomposing bitmasks."))

(defgeneric extns-of (assembler)
  (:documentation "Fetch an assembler's types."))

(defgeneric extn-p (assembler extension)
  (:documentation "Check for an assembler's support of an ISA extension."))

(defgeneric %derive-pmodel (assembler &rest params) ;; TODO: Remove this method
  (:documentation "Determine programming model for a given assembler."))

(defgeneric of-lexicon (assembler key &optional value)
  (:documentation "Fetch lexical get/setter for given assembler."))

(defgeneric of-storage (assembler key)
  (:documentation "Fetch storage object for given assembler."))

(defgeneric storage-type-p (assembler type &rest keys)
  (:documentation "Confirm membership of symbol(s) in a given storage type of an assembler."))

(defgeneric of-pmodel (assembler key)
  (:documentation "Fetch register file from program model by name or key for given assembler."))

(defgeneric specify-ops (assembler asm-symbol op-symbol operands params)
  (:documentation "Specify assembly functions for members of a given assembler's class."))

(defgeneric interpret-ops (assembler asm-symbol op-symbol operands params)
  (:documentation "Specify disassembly functions for members of a given assembler's class.")
  (:method-combination or))

(defgeneric qualify-ops (assembler operands form order)
  (:documentation "Qualify operations for members of a given assembler's class."))

(defgeneric extend-clauses (assembler mnemonic operands body params)
  (:documentation "Extend opcode specification clauses for an assembler."))

(defgeneric clause-processor (assembler action mnemonic operands body params)
  (:documentation "Process opcode specification clauses for an assembler."))

(defgeneric locate (assembler items)
  (:documentation "Locate available storage for use by a program."))

(defgeneric reserve (assembler &rest params)
  (:documentation "Reserve a storage location for use by a program."))

(defgeneric locate-relative  (assembler location-spec program-props)
  (:documentation "Find the position of a branch point relative to a location."))

(defgeneric compose (assembler params expression)
  (:documentation "A function composing an instruction for an assembler, translating the symbols in an instruction into specific values and class instances."))

(defgeneric interpret (assembler params array)
  (:documentation "A function converting operation codes for a given ISA into a human-readable assembly language."))

(defgeneric interpret-element (assembler ipattern reader)
  (:documentation "A function to interpret an individual opcode or other element.")
  (:method-combination or))

(defgeneric of-decoder (assembler-encoding key &optional value)
  (:documentation "Encoding getter/setter for an encoding assembler."))

(defgeneric of-battery (assembler-encoding key &optional value)
  (:documentation "Battery getter/setter for an encoding assembler."))

(defgeneric %assemble (assembler assembler-sym params expressions)
  (:documentation "A function implementing generation of opcodes from assembly code."))

(defmethod of-lexicon ((assembler assembler) key &optional value)
  (if value (setf (gethash key (asm-lexicon assembler)) value)
      (gethash key (asm-lexicon assembler))))

(defmethod of-storage ((assembler assembler) key)
  (let ((return-type) (return-index))
    (loop :for (type-key names) :on (asm-storage assembler) :by #'cddr :while (not return-type)
          :do (let ((pos (position key names)))
                (when pos (setf return-type type-key return-index pos))))
    (values return-index return-type)))

(defmethod of-pmodel ((assembler assembler) key)
  (let ((return-type) (return-index))
    (typecase key
      (integer (nth key (asm-pmodel assembler)))
      (symbol (loop :for rfile :on (asm-pmodel assembler)
                    :when (eq key (rfile-name rfile)) :return rfile)))))

(defmethod storage-type-p ((assembler assembler) type &rest keys)
  (let ((type-domain (getf (asm-storage assembler) type)))
    (loop :for key :in keys :always (position key type-domain))))

(defmethod of-decoder ((assembler assembler-encoding) key &optional value)
  (if value (setf (gethash key (asm-enc-decoder assembler)) value)
      (gethash key (asm-enc-decoder assembler))))

(defmethod of-battery ((assembler assembler-masking) key &optional value)
  (if value (setf (gethash key (asm-msk-battery assembler)) value)
      (gethash key (asm-msk-battery assembler))))

(defmethod extns-of ((item t))
  (declare (ignore item))
  nil)

(defmethod extns-of ((assembler assembler))
  (append (call-next-method)
          (asm-extns assembler)))

(defmethod extn-p ((assembler assembler) extension)
  (member extension (asm-extns assembler)))

(defmethod %derive-pmodel ((assembler assembler) &rest params) ;; TODO: remove
  (let ((derived-ranges))
    (loop :for p :in params
          :do (destructuring-bind (qualifier &rest bindings) p
                (when (or (eq t qualifier)
                          (member qualifier (asm-extns assembler)))
                  (loop :for b :in bindings
                        :do (setf (getf derived-ranges (first b))
                                  (max (second b) (or (getf derived-ranges (first b))
                                                      0)))))))
    ;; (print (list :der derived-ranges))
    (setf (asm-pmodel assembler)
          (loop :for (key length) :on derived-ranges :by #'cddr
                :collect (cons key (loop :for i :below length
                                         :unless (member i (rest (assoc key (asm-reserve assembler))))
                                           :collect i))))))

(defmacro derive-pmodel (assembler &rest params) ;; TODO: remove
  `(%derive-pmodel ,assembler ,@(loop :for p :in params :collect `(quote ,p))))

(defmacro specops (symbol operands assembler &body items)
  (let* ((params (if (not (and (listp (first items)) (listp (caar items))
                               (keywordp (caaar items))))
                     nil (first items)))
         (operations (if (not params) items (rest items))))
    (specify-ops (symbol-value assembler)
                 symbol operands (cons (cons :assembler-sym assembler) params)
                 operations)))

(defmethod clause-processor :around ((assembler assembler) action mnemonic operands params body)
  (declare (ignore assembler))
  ;; (print (list :pa params))
  (let ((args (gensym)) (types-to-match (gensym))
        (wrap-body (or (rest (assoc :wrap-body params)) #'identity))
        (api-access-sym (asm-program-api assembler))
        (api-access (asm-program-api-access assembler))
        (operands (case action (of-lexicon operands) (t)))
        (asm-sym (rest (assoc :assembler-sym params))))
    ;; (print (list :abc action mnemonic operands body))
    (multiple-value-bind (content key is-constant) (call-next-method)
      ;; (print (list :gg content key action params operands))
      (list action asm-sym key
            (if is-constant content
                `#'(lambda ,(cons api-access-sym operands)
                   (flet ((,api-access (&rest ,args) (apply ,api-access-sym ,args))
                          ,@(if (assoc :type-matcher params)
                                ;; generate type-matching function for use within instruction assembler
                                `((,(rest (assoc :type-matcher params))
                                   (&rest ,types-to-match)
                                   (intersection ,types-to-match (funcall ,api-access-sym
                                                                          :assembler-type))))))
                     ;; make API and type-checking function (if present) ignorable
                     (declare (ignorable (function ,api-access)
                                         ,@(if (assoc :type-matcher params)
                                               `((function ,(rest (assoc :type-matcher params)))))))
                     ,@(if (assoc :for-types params)
                           ;; generate top-level type-checking code
                           `((assert (intersection (,api-access :assembler-type)
                                                   ',(rest (assoc :for-types params)))
                                     () ,(format nil "Instruction ~a is not compatible ~a."
                                                 key "with this architecture"))))
                     ,@(funcall wrap-body content))))))))

(defmethod extend-clauses ((assembler assembler) mnemonic operands params body)
  (declare (ignore assembler mnemonic operands params))
  body)

(defmethod clause-processor ((assembler assembler) action mnemonic operands params body)
  (declare (ignore assembler action operands params))
  (values body (intern (string mnemonic) "KEYWORD")))

(defun process-clause-matrix (assembler op-symbol operands params operations)
  (let ((clauses) (prefixes) (opcode-base (if (numberp op-symbol) op-symbol 0)))
    (flet ((encoder-and-maybe-decoder-for (clause)
             (let* ((xtclause (if (not (listp (second clause)))
                                  nil (extend-clauses assembler (first clause)
                                                      operands params (rest clause))))
                    (encoder (clause-processor assembler 'of-lexicon (first clause)
                                               operands params (or xtclause (rest clause)))))
               ;; (print (list :xx xtclause))
               (if (not (assoc :duplex params))
                   encoder `(progn ,encoder ,@(loop :for c :in (or xtclause (rest clause))
                                                    :collect (clause-processor
                                                              assembler (rest (assoc :duplex params))
                                                              (first clause)
                                                              operands params c)))))))

      (loop :for row :in (rest operations) :for row-index :from 0
            :do (loop :for cell :in (rest row) :for cellx :in (cdar operations) :for col-index :from 0
                      :do (let ((opcode (+ opcode-base (first cellx) (caar row))))
                            (if (symbolp cell)
                                (case cell
                                  ;; prefix clauses desigate a particular code as a prefix, which
                                  ;; should be shifted and added to the next word to designate
                                  ;; a prefixed opcode; this is used for opcodes as in the
                                  ;; CB__ and ED__ tables of Z80 instructions
                                  (:prefix (push `(of-decoder ,(rest (assoc :assembler-sym params))
                                                              ,opcode :prefix)
                                                 prefixes)))
                                (destructuring-bind (&optional ins &rest opr) cell
                                  (when ins (if (assoc ins clauses)
                                                (when (not (atom (second (assoc ins clauses))))
                                                  ;; don't push an item if it's another possible
                                                  ;; opcode for an instruction without operands,
                                                  ;; as it's redundant and it interferes with the
                                                  ;; clause organization
                                                  (push (list opr opcode) (rest (assoc ins clauses))))
                                                (push (if opr (list ins (list opr opcode))
                                                          (list ins opcode))
                                                      clauses))))))))
      ;; (print (list :clau clauses params))
      ;; (loop :for c :in clauses :collect (decode (clause-processor assembler (first c) operands params c)))
      ;; (loop :for c :in clauses :collect (encoder-and-maybe-decoder-for c))
      (append (mapcar #'encoder-and-maybe-decoder-for clauses)
              prefixes))))

(defun complete-dforms (processor symbol options form)
  "A function that facilitates processing of forms within an operation spec; it is now used mainly for the composition of (determine-in-context) macros according to the parameters of (specops) forms they appear in."
  (if (not processor)
      form (typecase form
             (atom form)
             (list (or (funcall processor symbol options form)
                        (loop :for item :in form
                             :collect (complete-dforms processor symbol options item)))))))

(defmethod specify-ops ((assembler assembler) op-symbol operands params operations)
  "A simple scheme for implementing operations - the (specop) content is directly placed within functions in the lexicon hash table."
  ;; retrieve the form processor specified in the params, if any - this is often added in the
  ;; course of macro currying performed in specific architecture modules
  (let ((form-processor (and (rest (assoc :process-forms params))
                             (symbol-function (rest (assoc :process-forms params))))))
    (cond ((assoc :combine params)
           ;; combinatoric parameters are used to specify mnemonics and opcodes that can be derived
           ;; by combining lists of base components, such as the Xcc conditional instructions seen
           ;; in many ISAs, where many different conditions share a common numeric and mnemonic base
           (destructuring-bind (co-symbol join-by indexer &rest combinators)
               (rest (assoc :combine params))
             (cons 'progn (loop :for co :in combinators :for i :from 0
                                :collect (let ((comp-sym (case join-by
                                                           (:appending (intern (format nil "~a~a" op-symbol co)
                                                                               "KEYWORD"))))
                                               (index (funcall (case indexer (:by-index #'identity))
                                                               i)))
                                           (clause-processor
                                            assembler 'of-lexicon comp-sym operands
                                            (append (list (cons :wrap-body ;; TODO: IL PROBLEM HERE
                                                                (lambda (body)
                                                                  `((let ((,co-symbol ,index)) ,@body)))))
                                                  params)
                                          (complete-dforms form-processor comp-sym params operations)))))))
        ((assoc :tabular params)
         ;; tabular parameters are used to specify many opcodes for ISAs like Z80 and 6502 along
         ;; with more recent one-byte instruction sets like WebAssembly
         (destructuring-bind (mode &rest properties) (rest (assoc :tabular params))
           (declare (ignore properties))
           (case mode (:cross-adding
                       (cons 'progn (process-clause-matrix
                                     assembler op-symbol
                                     operands params (complete-dforms form-processor op-symbol
                                                                      params operations)))))))
        (t (clause-processor assembler 'of-lexicon op-symbol operands params
                             (complete-dforms form-processor op-symbol params operations))))))

(defmacro readops (symbol operands assembler &body items)
  (let* ((params (if (not (and (listp (first items)) (listp (caar items))
                               (keywordp (caaar items))))
                     nil (first items)))
         (operations (if (not params) items (rest items))))
    (interpret-ops (symbol-value assembler)
                   symbol operands (cons (cons :assembler-sym assembler) params)
                   operations)))

(defmethod interpret-ops or ((assembler assembler-encoding) designator operands params operations)
  "A simple scheme for implementing operations - the (specop) content is directly placed within functions in the lexicon hash table."
  (let ((designator (macroexpand designator)))
    (if (not (numberp designator))
        nil `(of-decoder ,(rest (assoc :assembler-sym params)) ,designator
                         (lambda ,operands (declare (ignorable ,@operands)) ,@operations)))))

(defmethod interpret-ops or ((assembler assembler-masking) designator operands params operations)
  "A simple scheme for implementing operations - the (specop) content is directly placed within functions in the lexicon hash table."
  (if (not (symbolp designator))
      nil `(of-battery ,(rest (assoc :assembler-sym params)) ,(intern (string designator) "KEYWORD")
                       (lambda ,operands (declare (ignorable ,@operands)) ,@operations))))

;; (defmacro readop (symbol args assembler-sym &body body)
;;   (let ((symbol (macroexpand symbol))
;;         (function `(lambda ,args (declare (ignorable ,@args)) ,@body)))
;;     (if (numberp symbol)
;;         `(of-decoder *assembler-prototype-m68k* ,symbol ,function)
;;         `(of-battery *assembler-prototype-m68k* ,(intern (string symbol) "KEYWORD") ,function))))

(defmacro determine-in-context (utils mnemonic specs &optional bindings &rest body)
  (destructuring-bind (&key qualify derive verbalize &allow-other-keys) utils
    (labels ((process-level (body bindings specs)
               (if (not bindings)
                   body (if (and (first bindings) (listp (first bindings)))
                            `((multiple-value-bind ,(first bindings)
                                  ;; ,(funcall derive (caar specs) (cadar specs))
                                  ,(funcall derive (first specs))
                                ,@(process-level body (rest bindings) (rest specs))))
                            (process-level body (rest bindings) (rest specs))))))
      (let* ((mnem-length (length (string mnemonic)))
             (op-strings (loop :for spec :in specs :collect (string (first spec))))
             (op-max-length (reduce #'max (loop :for string :in op-strings :collect (length string)))))
        `(if ,(cons 'and (loop :for spec :in specs
                               :collect (destructuring-bind (operand &rest types) spec
                                          (cons 'or (loop :for type :in types
                                                          :collect (funcall qualify operand type))))))
             ,(if body `(let ,(if bindings (loop :for b :in bindings :for s :in specs
                                                 :when (and b (symbolp b))
                                                   :collect (list b (funcall derive s))))
                         ,@(process-level body bindings specs)))
             (error ,(apply #'concatenate 'string
                            (format nil "Invalid operand(s) for instruction ~a. Format: ~%" mnemonic)
                            (format nil "~a ~v,a - ~a" mnemonic op-max-length (caar specs)
                                    (funcall verbalize (cadar specs)))
                            (append (loop :for sub-spec :in (cddar specs)
                                          :collect (or (format nil "~v,a ~a" (+ mnem-length 3 op-max-length)
                                                               #\  (funcall verbalize sub-spec))
                                                       ""))
                                    (loop :for spec :in (rest specs)
                                          :append (append (list (format nil "~v,a ~a - ~a"
                                                                        mnem-length #\ (first spec)
                                                                        (funcall verbalize (cadr spec))))
                                                          (loop :for sub-spec :in (cddr spec)
                                                                :collect (format
                                                                          nil "~v,a ~a"
                                                                          (+ mnem-length 3 op-max-length)
                                                                          #\ (funcall verbalize sub-spec)))
                                                        ))))))))))

(defparameter *default-segment* 1000)

(defmethod locate-relative ((assembler assembler) location-spec program-props)
  ;; (print (list :aea location-spec program-props))
  (destructuring-bind (label bit-offset field-length index) location-spec
    (declare (ignore bit-offset))
    (let ((offset (- (rest (assoc label (getf program-props :marked-points))) index)))
      (if (not (minusp offset)) ;; two's complement conversion
          offset (+ (ash 1 (1- field-length))
                    (abs offset))))))

(defmethod locate ((assembler assembler) items)
  (let ((reserved) (pmodel (asm-pmodel assembler)))
    (loop :for item :in items
          :collect (destructuring-bind (symbol type &key bind &allow-other-keys) item
                     (let* ((type-spec (getf pmodel type))
                            (rset (assoc type reserved))
                            (out-item (if bind (typecase type-spec
                                                 (list     (and (position bind type-spec :test #'eq)
                                                                bind))
                                                 (function (funcall type-spec bind)))
                                          (let ((options (set-difference type-spec (rest rset))))
                                            ;; (print (list :aa pmodel type-spec (rest rset)))
                                            (nth (random (length options)) options)))))
                       (unless rset
                         (push (setf rset (list type)) reserved))
                       (when (and bind out-item)
                         (when (not (member out-item type-spec))
                           (error "The member ~a of type ~a is not available in this assembler's domain."
                                  bind type))
                         (when (member out-item (rest (assoc type reserved)))
                           (error "The member ~a of type ~a has already been reserved." bind type)))
                       (if out-item (push out-item (rest (assoc type reserved)))
                           (error "Unable to reserve member ~a of type ~a." bind type))
                       (list symbol out-item))))))

(defmethod compose ((assembler assembler) params expression)
  "The top-level method for assembly. Generates a byte vector from a list of instructions formatted as small lists."
  (let* ((unit (asm-breadth assembler))
         (codes (make-array *default-segment* :element-type (list 'unsigned-byte unit)
                                              :initial-element 0 :adjustable t :fill-pointer 0))
         (codes-length) (props (list :marked-points nil :tag-points nil))
         (two-power (floor (log unit 2)))
         (api-access (lambda (mode &rest args)
                       (case mode
                         (:assembler-type (asm-extns assembler))
                         (:exmode (or (rest (assoc :exmode params))
                                      (first (asm-exmodes assembler))))
                         (:label (destructuring-bind (field-length offset-bits symbol) args
                                   (typecase symbol
                                     (integer symbol)
                                     (symbol  (let ((spec (list symbol offset-bits field-length
                                                                (fill-pointer codes))))
                                                (or (and (assoc symbol (getf props :marked-points))
                                                         (locate-relative assembler spec props))
                                                    (and (push spec (getf props :tag-points))
                                                         0)))))))))))
    (flet ((add-value (value)
             (unless (< (fill-pointer codes) (1- (length codes)))
               (adjust-array codes (+ *default-segment* (length codes)) :initial-element 0))
             (vector-push value codes)))
      (loop :for item :in expression
            :do (typecase item
                  (symbol (push (cons item (fill-pointer codes)) (getf props :marked-points)))
                  (list   (destructuring-bind (instruction &rest operands) item
                            (let ((build-instruction (of-lexicon assembler instruction)))
                              (multiple-value-bind (code properties)
                                  (if (not (functionp build-instruction))
                                      build-instruction (apply build-instruction api-access operands))
                                (if (functionp code) ;; TODO: remove this clause
                                    (let ((breadth (or (getf properties :breadth) 0)))
                                      (push (append (list (fill-pointer codes) code)
                                                    properties)
                                            (getf props :tag-points))
                                      (dotimes (i breadth) (add-value 0)))
                                    (if (listp code)
                                        (loop :for item :in code :when item
                                              :do (serialize item unit #'add-value))
                                        (serialize code unit #'add-value)))))))))
      (setf codes-length (fill-pointer codes))
      
      (loop :for tag-spec :in (getf props :tag-points)
            :do (destructuring-bind (symbol bit-offset field-length index) tag-spec
                  (let ((value (locate-relative assembler tag-spec props))
                        (unaligned-bits (logand bit-offset (1- (ash 1 two-power)))))
                    (setf (fill-pointer codes) (+ index (ash bit-offset (- two-power))))
                    (unless (zerop unaligned-bits)
                      (let ((first-original (aref codes (fill-pointer codes))))
                        (serialize (+ first-original (ash value (- (- field-length
                                                                      (- unit unaligned-bits)))))
                                   unit #'add-value)))
                    (serialize (rest (assoc symbol (getf props :marked-points)))
                               unit #'add-value))))
      (let ((output (make-array codes-length :element-type (list 'unsigned-byte unit))))
        (loop :for i :below codes-length :do (setf (aref output i) (aref codes i)))
        output))))

(defmethod %assemble ((assembler assembler) assembler-sym params expressions)
  `(let ,(if (not (rest (assoc :store params)))
             nil (locate assembler (rest (assoc :store params))))
     (compose ,assembler-sym ',params
              (list ,@(loop :for e :in expressions
                            :collect (if (not (and (listp e) (keywordp (first e))))
                                         e (cons 'list e)))))))

(defmacro assemble (assembler &rest expressions)
  (let* ((params (if (not (and (listp (first expressions))
                               (caar expressions) (listp (caar expressions))))
                     nil (first expressions)))
         (expressions (if (not params) expressions (rest expressions))))
    (%assemble (symbol-value assembler) assembler params expressions)))

;; (defmethod interpret ((assembler assembler) params array)
;;   "The top-level method for disassembly. Composes a list of instructions from a byte vector according to the properties of a given ISA."
;;   (let* ((etype (let ((element-type (array-element-type array)))
;;                   (unless (and (listp element-type)
;;                                (eq 'unsigned-byte (first element-type)))
;;                     (error "Invalid array."))
;;                   (second element-type)))
;;          (to-read (/ etype (asm-breadth assembler)))
;;          ;; (intervals (loop :for segment :in (asm-msk-segment assembler)
;;          ;;                  :collect (ash etype (- (+ 2 segment)))))
;;          (intervals (coerce (asm-msk-segment assembler) 'vector))
;;          (deltas (cons 0 (loop :for i :from (1- (length intervals)) :downto 1
;;                                :collect (* etype (- (aref intervals i) (aref intervals (1- i)))))))
;;          (point 0) (disassembled))
;;     (labels ((read-words (from count)
;;                ;; (print (list :et etype from count))
;;                (let ((value 0))
;;                  (loop :for c :below (* count to-read)
;;                        :do (setf value (ash  value etype))
;;                            (incf value (aref array (+ c from))))
;;                  value)))
;;       (loop :while (< point (1- (length array)))
;;             :do (let* ((match) (this-interval) ;; (ipatterns)
;;                        (sub-count 0)
;;                        (reader (lambda (in)
;;                                  (lambda (count)
;;                                    (let ((this-count sub-count))
;;                                      (incf sub-count count)
;;                                      (read-words (+ point this-count in) count)))))
;;                        (access (lambda (in)
;;                                  (lambda (mode &rest args)
;;                                    (case mode
;;                                      (:read (let ((count (first args))
;;                                                   (this-count sub-count))
;;                                               (incf sub-count count)))))))
;;                        ;;                      (read-words (+ point this-count in) count))))))
;;                        (ivindex (min (1- (length intervals)) (- (length array) point 1)))
;;                        (pattern (read-words point (aref intervals ivindex))))
;;                   ;; (print (list :dd deltas))
                  
;;                   (loop :for ix :below ivindex :for delta :in deltas
;;                         :do (setf pattern (ash pattern (- delta)))
;;                             (let ((match? (interpret-element assembler pattern
;;                                                              (funcall reader (aref intervals ix)))))
;;                               (when match? (setf match         match?
;;                                                  this-interval (aref intervals ix)))))
                 
;;                   (if match (progn (push match disassembled)
;;                                    (setf point (+ point sub-count this-interval)))
;;                       ;; (progn (push nil disassembled)
;;                       ;;        (setf point (+ point sub-count this-interval)))
;;                       (error "Undecipherable instruction!")
;;                       )))
;;       (reverse disassembled))))

(defmethod interpret ((assembler assembler) params array)
  "The top-level method for disassembly. Composes a list of instructions from a byte vector according to the properties of a given ISA."
  (let* ((etype (let ((element-type (array-element-type array)))
                  (unless (and (listp element-type)
                               (eq 'unsigned-byte (first element-type)))
                    (error "Invalid array."))
                  (second element-type)))
         (to-read (/ etype (asm-breadth assembler)))
         ;; (intervals (loop :for segment :in (asm-msk-segment assembler)
         ;;                  :collect (ash etype (- (+ 2 segment)))))
         (intervals (coerce (asm-msk-segment assembler) 'vector))
         (deltas (cons 0 (loop :for i :from (1- (length intervals)) :downto 1
                               :collect (* etype (- (aref intervals i) (aref intervals (1- i)))))))
         (point 0) (disassembled))
    (labels ((read-words (from count)
               ;; (print (list :et etype from count))
               (let ((value 0))
                 (loop :for c :below (* count to-read)
                       :do (setf value (ash  value etype))
                           (incf value (aref array (+ c from))))
                 value)))
      (loop :while (< point (1- (length array)))
            :do (let* ((match) (this-interval)
                       (base 0) (sub-count 0)
                       ;; (reader (lambda (in)
                       ;;           (lambda (count)
                       ;;             (let ((this-count sub-count))
                       ;;               (incf sub-count count)
                       ;;               (read-words (+ point this-count in) count)))))
                       (access (lambda (in)
                                 (declare (ignore in))
                                 (lambda (mode &rest args)
                                   (case mode
                                     (:read (let ((count (first args))
                                                  (this-count sub-count))
                                              (incf sub-count count)
                                              (read-words (+ point this-count base) count)))))))
                       (ivindex (min (1- (length intervals)) (- (length array) point 1)))
                       (pattern (read-words point (aref intervals ivindex))))
                  ;; (print (list :dd deltas))
                  
                  (loop :for ix :below ivindex :for delta :in deltas
                        :do (setf pattern (ash pattern (- delta))
                                  base    (aref intervals ix))
                            (let ((match? (interpret-element assembler pattern access)))
                              (when match? (setf match         match?
                                                 this-interval (aref intervals ix)))))
                 
                  (if match (progn (push match disassembled)
                                   (setf point (+ point sub-count this-interval)))
                      ;; (progn (push nil disassembled)
                      ;;        (setf point (+ point sub-count this-interval)))
                      (error "Undecipherable instruction!")
                      )))
      (reverse disassembled))))

(defmethod interpret-element or ((assembler assembler-encoding) ipattern reader)
  (let ((match (gethash ipattern (asm-enc-decoder assembler))))
    ;; (print (list :ma match))
    (if (not (functionp match)) match (identity (funcall match reader)))))

(defmethod interpret-element or ((assembler assembler-masking) ipattern reader)
  (let ((match))
    (maphash (lambda (key unmasker)
               (declare (ignore key))
               (unless match (let ((attempt (funcall unmasker ipattern reader)))
                               (when attempt (setf match attempt)))))
             (asm-msk-battery assembler))
    ;; (loop :until match :for unmasker :being :the :hash-values :of (asm-msk-battery assembler)
    ;;       :do (let ((attempt (funcall unmasker ipattern reader)))
    ;;             (when attempt (setf match attempt))))
   match))

(defmacro match-types (&rest pairs)
  (let ((items) (types) (count (length pairs)))
    (loop :for sym :in pairs :for i :from 0 :do (if (< i (ash count -1))
                                                    (push sym items) (push sym types)))
    (cons 'and (loop :for it :in items :for ty :in types :collect `(typep ,it ',ty)))))

(defmacro to-tag (function &rest properties)
  (list 'values function (cons 'list properties)))

(defmacro define-extension (symbol assembler output-sym input-bindings &body body)
  (let ((input (gensym)) (params (gensym)))
    `(defun ,symbol (,input &optional ,params)
       (funcall #'(lambda ,(cons output-sym input-bindings) ,@body)
                (cons (compose ,assembler ,params ,input) ,input)))))

;; (defmethod clause-processor ((assembler assembler) mnemonic operands body params)
;;   (declare (ignore assembler))
;;   (let ((args (gensym))
;;         (wrap-body (or (rest (assoc :wrap-body params)) #'identity))
;;         (api-access-sym (asm-program-api assembler))
;;         (api-access (asm-program-api-access assembler)))
;;     `(of-lexicon ,(rest (assoc :assembler-sym params))
;;                  ;; ,assembler-symbol
;;                  ,(intern (string mnemonic) "KEYWORD")
;;                  ;; ,(add-alias `
;;                  (lambda ,(cons api-access-sym operands)
;;                    (flet ((,api-access (&rest ,args) (apply ,api-access-sym ,args)))
;;                      ,@(funcall wrap-body body))))))

;; (defun process-clause-matrix (assembler asm-sym operands matrix params)
;;   (declare (ignore params))
;;   (let ((clauses) ;; (varops (remove '&optional operands))
;;         (clause-processor (clause-processor assembler asm-sym)))
;;     (print (list :mm matrix))
;;     (symbol-macrolet ((opcode (+ (first cellx) (caar row))))
;;       (loop :for row :in (rest matrix) :for row-index :from 0
;;             :do (loop :for cell :in (rest row) :for col-index :from 0
;;                       :for cellx :in (cdar matrix)
;;                       :do (destructuring-bind (&optional ins &rest opr) cell
;;                             (when ins (if (assoc ins clauses)
;;                                           (when (not (atom (second (assoc ins clauses))))
;;                                             ;; don't push an item if it's another possible
;;                                             ;; opcode for an instruction without operands,
;;                                             ;; as it's redundant and it interferes with the
;;                                             ;; clause organization
;;                                             (push (list opr opcode) (rest (assoc ins clauses))))
;;                                           (push (if opr (list ins (list opr opcode))
;;                                                     (list ins opcode))
;;                                                 clauses))))))
;;       (print (list :clau clauses))
;;       (loop :for c :in clauses :append (funcall clause-processor c operands)))))

;; (defun process-clause-matrix (assembler asm-sym operands matrix params)

;; (defmethod compose :around ((assembler assembler) params expression)
;;   (destructuring-bind (op &rest props) expression
;;     (typecase expression
;;       (atom expression)
;;       (list (if (atom (first expression)) (call-next-method)
;;                 (loop :for item :in expression :collect (compose assembler params item)))))))


;; (defmethod compose ((assembler assembler) params expression)
;;   (print (list :par params))
;;   (destructuring-bind (instruction &rest operands) expression
;;     (let ((build-instruction (gethash instruction (asm-lexicon assembler))))
;;       (if (not (functionp build-instruction))
;;           build-instruction (apply build-instruction operands)))))

;; (defmethod %assemble ((assembler assembler) assembler-sym params expressions)
;;   (let ((compose-params))
;;     `(let ,(locate assembler assembler-sym (rest (assoc :store (rest params))))
;;        (apply (asm-joiner ,assembler-sym)
;;               (compose ,assembler-sym ,compose-params
;;                               (list ,@(loop :for e :in expressions
;;                                             :collect (if (not (and (listp e) (keywordp (first e))))
;;                                                          e (cons 'list e)))))))))

;; (defun label-delta (label-index inst-params)
;;   (- label-index (nth 2 inst-params)))

;; (loop :for ix :from (min (1- (length intervals)) (- (length array) point 1)) :downto 0
;;       :do (let* ((interval (aref intervals ix))
;;                  (pattern (read-words point interval))
;;                  (match? (interpret-element assembler pattern (funcall reader interval))))
;;             (when match? (setf match         match?
;;                                this-interval interval))))

;; (loop :for ix :from (length intervals) :downto (max 0 (- (length array) point))
;;       :for iv :across intervals
;;       :do (let ((pattern (read-words point iv)))))
;; (loop :for in :in intervals :when (> (length array) (+ 1 point in))
;;       :do (push (read-words point in) ipatterns))
;; (print (list :ip ipatterns))
;; (loop :for in :in (reverse intervals) :for ip :in ipatterns ;; :until match
;;       :do (let ((match? (interpret-element assembler ip (funcall reader in))))
;;             (when match?
;;               (setf match match?
;;                     this-interval in))))
;; (print (list match point (length array)))

;; (defmethod locate ((assembler assembler) location-spec program-props)
;;   ;; (print (list :aea location-spec program-props))
;;   (destructuring-bind (label bit-offset field-length index) location-spec
;;     (asm-pmodel)
;;     (let ((offset (- (rest (assoc label (getf program-props :marked-points))) index)))
;;       (if (not (minusp offset)) ;; two's complement conversion
;;           offset (+ (ash 1 (1- field-length))
;;                     (abs offset))))))

;; (define-extension lock-inst #'joinb bytes (instruction &rest operands)
;;   (declare (ignore operands))
;;   (if (not (position instruction #(:add)))
;;       (error "Ineligible instruction for use with lock prefix.")
;;       )
;;   )
  
;; (defmethod interpret or ((assembler assembler-encoding) params array)
;;   (let* ((etype (let ((element-type (array-element-type array)))
;;                   (unless (eq 'unsigned-byte (first element-type))
;;                     (error "Invalid array."))
;;                   (second element-type)))
;;          (to-read (/ etype (asm-breadth assembler)))
;;          (disassembled) (point 0))
;;     (labels ((read-words (count)
;;                (let ((value 0))
;;                  (loop :for i :below count
;;                        :do (loop :for i :below to-read
;;                                  :do (setf value (ash value etype))
;;                                      (incf value (aref array index))
;;                                      (incf index)))
;;                  value)))
;;       (loop :while (< point (1- (length array)))
;;             :do (let ((match (of-decoder assembler (read-words 1) nil)))
;;                   (push (if (not (functionp match))
;;                             match (funcall match #'read-words))
;;                         disassembled))))))

;; (defmethod interpret or ((assembler assembler-masking) params array)
;;   (let* ((etype (let ((element-type (array-element-type array)))
;;                   (unless (and (listp element-type)
;;                                (eq 'unsigned-byte (first element-type)))
;;                     (error "Invalid array."))
;;                   (second element-type)))
;;          (to-read (/ etype (asm-breadth assembler)))
;;          (intervals (loop :for segment :in (asm-msk-segment assembler)
;;                           :collect (ash etype (- (+ 2 segment)))))
;;          (ipatterns) (disassembled) (point 0))
;;     (labels ((read-words (from count)
;;                (let ((value 0))
;;                  (loop :for c :below (* count to-read)
;;                        :do (setf value (ash  value etype))
;;                            (incf value (aref array (+ c from))))
;;                  value)))
      
;;       (loop :while (< point (1- (length array)))
;;             :do (let ((total 0) (match) (this-interval) (sub-count 0))
;;                   (loop :for i :in intervals :do (push (read-words point i) ipatterns))
;;                   (loop :for in :in intervals :for ip :in ipatterns :while (not match)
;;                         :do (loop :for unmasker :being :the :hash-values
;;                                     :of (asm-msk-battery assembler) :while (not match)
;;                                   :do (let ((attempt (funcall unmasker ip
;;                                                               (lambda (count)
;;                                                                 (let ((this-count sub-count))
;;                                                                   (incf sub-count count)
;;                                                                   (read-words (+ point this-count in)
;;                                                                               count))))))
;;                                         ;; (format t "~v,'0B~%" 16 ip)
;;                                         (when attempt (setf match         attempt
;;                                                              this-interval in)))))
;;                   (if match (progn (push match disassembled)
;;                                    (setf point (+ point sub-count this-interval)))
;;                       (error "Undecipherable instruction!"))))
;;       (reverse disassembled))))

;; (defmethod compose ((assembler assembler) params expression)
;;   (destructuring-bind (ins &rest ops) expression
;;     (let ((width (if (not (keywordp (first ops)))
;;                      nil (position (first ops) #(:b :w :l) :test #'eq)))
;;           (bindings (rest (assoc :store params))))
;;       (print (list :bi bindings width params ins ops))
;;       (apply (gethash ins (asm-lexicon assembler))
;;              (process-operands bindings ops)))))

;; (defun process-operands (bindings items)
;;   (loop :for i :in items :collect (typecase o
;;                                     (keyword o)
;;                                     (symbol (second (assoc o bindings)))
;;                                     (list (destructuring-bind (constructor &rest members) o
;;                                             (funcall (symbol-function constructor)
;;                                                      (process-operands bindings members))))
;;                                     (t o))))

;; (let ((compose-params))
;;   `(let ,(locate (symbol-value assembler)
;;                  (rest (assoc :store (rest params))))
;;      (join (compose ,assembler ,compose-params
;;                     (list ,@(loop :for e :in expressions
;;                                   :collect (if (not (and (listp e) (keywordp (first e))))
;;                                                e (cons 'list e)))))))))

;; (defmacro assemble (assembler params &rest expressions)
;;   `(%assemble ,assembler ',(rest params)
;;               ,(cons 'list (loop :for e :in expressions
;;                                  :collect (if (not (and (listp e) (keywordp (first e))))
;;                                               e (list 'quote e))))))

;; (defun field (&rest values)
;;   (let ((collected) (count 0))
;;     (loop :for value :in values
;;           :do  (if (zerop (ash value -8))
;;                    (progn (incf count) (push 0 collected))
;;                    (loop :until (zerop value) ;; larger values are decomposed and appended
;;                          :do (push (logand value #xFF) collected)
;;                              (setf value (ash value -8)) (incf count))))
;;     (make-array count :element-type '(unsigned-byte 8) :initial-contents collected)))

;; (defun vectorize (bytes value)
;;   (let ((collected))
;;     (loop :until (zerop bytes)
;;           :do (push (logand value #xFF) collected)
;;               (setf value (ash value -8)) (decf bytes))
;;     (make-array (length collected) :element-type '(unsigned-byte 8) :initial-contents collected)))

;; (defun inc-field (field delta)
;;   (let ((index 0))
;;     (loop :until (zerop delta)
;;           :do (incf (aref field index) (logand delta #xFF))
;;               (setf delta (ash delta -8))
;;               ;; (print delta)
;;               (incf index))))

;; (let ((base (gensym)) (bits (gensym)) (mapper (gensym)) (input (gensym)))
;;   `(multiple-value-bind (,base ,bits ,mapper) (bitmapper ,string)
;;      ;; ,@(loop for c :in clauses :collect `(format t "~v,'0B~%" 24 (funcall ,mapper ,@c)))
;;      (funcall (if (>= 8 ,bits) #'identity (lambda (,input) (vectorize (ceiling ,bits 8) ,input)))
;;               (reduce #'+ (list ,base ,@(loop for c :in clauses :collect `(funcall ,mapper ,@c))))))))


;; (defun prefix-vex (&key type length m-value third-op)
;;   ;; length is either 2 or 3 bytes with indicating initial byte
;;   (+ (case type (2 #xc500) (3 #xc40000))
;;      ;; the m-value extends the opcode, and is indicated in specs by the
;;      ;; byte sequences 
;;      (case m-value (1 #x0100) (2 #x0200) (3 #x0300))
;;      ;; the third bit in the last byte is 1 if the vector length is 256 bits, off if not
;;      (if (= 256 length) #b00000100 0) ;; the L bit is turned on if the vector is of length 256
;;      (if third-op (ash (flipbits third-op 4) 3))
;;      ;; if a third register address is to be passed in the last byte, reverse its bits and shift it left 3
;;      ))

;; (defun bitmapper (string)
;;   (let ((segments) (symbols) (base 0) (bits 0))
;;     (loop :for c :across string :for ix :from 0 :when (not (char= c #\.)) ;; period is used as a spacer
;;           :do (if (member c '(#\1 #\0) :test #'char=)
;;                   (when (char= c #\1) (incf base)) ;; set constant 1 bits
;;                   (when (or (not symbols) (not (eq (intern (string-upcase c) "KEYWORD") (first symbols))))
;;                     (push (intern (string-upcase c) "KEYWORD") symbols)
;;                     (push bits segments)))
;;               ;; shift the number to the left unless this is the last digit
;;               (unless (= ix (1- (length string))) (setf base (ash base 1)))
;;               (incf bits))
;;     ;; (print (list :in bits))
;;     (setf segments (cons 0 (loop :for s :in segments :collect (abs (- s bits)))))
;;     ;; (list segments symbols)
;;     (flet ((ones-field (n)
;;              (let ((output 0))
;;                (loop :for i :below n
;;                      :do (incf output 1) (when (< i (1- n)) (setf output (ash output 1))))
;;                output)))
;;       ;; (format t "~v,'0B~%" 24 base)
;;       (values base bits
;;               (lambda (seg-sym value)
;;                 ;; (print (list :ss seg-sym value))
;;                 (let* ((insym (intern (string-upcase seg-sym) "KEYWORD"))
;;                        (index (loop :for s :in symbols :for ix :from 0 :when (eq s insym) :return ix))
;;                        (length (- (nth (1+ index) segments) (nth index segments))))
;;                   (ash (logand value (ones-field length))
;;                        (nth index segments))))))))



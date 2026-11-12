;;;; test/test-units.lisp — The code builder for binary values, src/units.lisp: orders of units,
;;;; and the forms that store a value at a target and load it from a source, compiled and run
;;;; here on their own, apart from manifests. Pure: no files.

(in-package #:specops-tests)

(in-suite units)

;;; ============================================================
;;; Helpers
;;; ============================================================

(defun run-form (form)
  "The values of FORM, compiled and called."
  (funcall (compile nil `(lambda () ,form))))

(defun entered-units (encoding value-form)
  "The units a store of VALUE-FORM, as ENCODING gives, enters at an entering target, in order."
  (run-form `(let* ((units nil)
                    (enter (lambda (u) (push u units))))
               ,(store-form (make-instance 'entry-target :enter 'enter) encoding value-form)
               (reverse units))))

(defun stored-vector (size index encoding value-form)
  "A vector of SIZE octets after a store of VALUE-FORM at INDEX through a vector target, and the
index after the store."
  (run-form `(let ((v (make-array ,size :element-type '(unsigned-byte 8) :initial-element 0))
                   (i ,index))
               ,(store-form (make-instance 'vector-target :vector 'v :index 'i) encoding value-form)
               (values v i))))

;;; ============================================================
;;; Orders of units
;;; ============================================================

(def-test an-order-gives-the-significance-of-each-unit-as-stored ()
  "An order lists the significance of each stored unit, 0 the least significant: :big from the
most significant, :little from the least, and a grouped order its groups in the outer order
and each group's units in the inner one."
  (is (equal '(3 2 1 0) (unit-significances 4 :big)))
  (is (equal '(0 1 2 3) (unit-significances 4 :little)))
  (is (equal '(2 3 0 1) (unit-significances 4 '(:big :little 2))))     ; BADC
  (is (equal '(1 0 3 2) (unit-significances 4 '(:little :big 2))))     ; CDAB
  (is (equal '(0 1) (unit-significances 2 '(:big :little 2))))
  (is (equal '(0) (unit-significances 1 '(:little :big 2))))
  (is (equal '(4 5 6 7 0 1 2 3) (unit-significances 8 '(:big :little 4)))))

(def-test a-value-that-does-not-fill-its-groups-is-an-error ()
  "A value of more units than a group, but not a whole number of groups, cannot be ordered."
  (signals error (unit-significances 3 '(:big :little 2))))

(def-test byte-orders-are-named-or-written-explicitly ()
  "NIL and :big name the big-endian order, :little the little-endian one, and :pdp the
PDP-11's, (:big :little 2); an explicit order is kept; :middle, retired, and an unknown name
are errors, and the error for :middle names the explicit forms."
  (is (eq :big (unit-order nil)))
  (is (eq :big (unit-order :big)))
  (is (eq :little (unit-order :little)))
  (is (equal '(:big :little 2) (unit-order :pdp)))
  (is (equal '(:little :big 2) (unit-order '(:little :big 2))))
  (let ((message (handler-case (progn (unit-order :middle) "")
                   (error (e) (princ-to-string e)))))
    (is-true (search "(:big :little 2)" message))
    (is-true (search "(:little :big 2)" message)))
  (dolist (bad '(:sideways (:big :little) (:big :up 2) (:big :little 1)))
    (signals error (unit-order bad))))

;;; ============================================================
;;; Stores
;;; ============================================================

(def-test a-store-enters-a-value-s-units-in-its-order ()
  "A store at an entering target enters a value's units in its order, whatever the width of
its units."
  (is (equal '(1 2 3 4) (entered-units (make-encoding 4 8) '(identity #x01020304))))
  (is (equal '(4 3 2 1) (entered-units (make-encoding 4 8 :order :little) '(identity #x01020304))))
  (is (equal '(2 1 4 3) (entered-units (make-encoding 4 8 :order :pdp) '(identity #x01020304))))
  (is (equal '(3 4 1 2) (entered-units (make-encoding 4 8 :order '(:little :big 2))
                                       '(identity #x01020304))))
  (is (equal '(#x0102 #x0304) (entered-units (make-encoding 2 16) '(identity #x01020304))))
  (is (equal '(#x0304 #x0102) (entered-units (make-encoding 2 16 :order :little)
                                             '(identity #x01020304)))))

(def-test a-store-at-a-vector-target-stores-at-its-index-and-advances-it ()
  "A store through a vector target writes a value's units from the index in their order,
leaving the rest of the vector as it was, and advances the index past them."
  (multiple-value-bind (v i) (stored-vector 6 1 (make-encoding 2 8 :order :little) '(identity #x1234))
    (is (equalp #(0 #x34 #x12 0 0 0) v))
    (is (eql 3 i))))

(def-test a-negative-value-is-stored-in-two-s-complement ()
  "A negative value is stored as its two's complement in the value's units."
  (is (equal '(#xFF #xFE) (entered-units (make-encoding 2 8) '(identity -2)))))

(def-test an-integer-is-split-into-units-at-expansion ()
  "A store of an integer is made of the units themselves, with nothing split when the code
runs."
  (let ((form (store-form (make-instance 'entry-target :enter 'enter) (make-encoding 2 8) #x0102)))
    (is-false (tree-mentions-p 'ldb form))
    (is (equal '(1 2) (entered-units (make-encoding 2 8) #x0102)))))

(def-test a-value-too-wide-is-an-error-and-stores-nothing ()
  "A value that fits neither as unsigned nor as two's complement in its units signals an
error naming the units, before any unit is stored, whether it is known at expansion or not."
  (dolist (value '((identity 65536) (identity -32769) 65536 (identity :a)))
    (let* ((units nil)
           (message (handler-case
                        (progn (run-form `(let ((enter (lambda (u) (declare (ignore u))
                                                         (error "A unit was stored."))))
                                            ,(store-form (make-instance 'entry-target :enter 'enter)
                                                         (make-encoding 2 8) value)))
                               "")
                      (error (e) (princ-to-string e)))))
      (is (null units))
      (is-true (search "does not fit within 2 unit(s) of 8 bits" message)))))

(def-test a-code-is-stored-in-as-many-units-as-it-needs ()
  "A code stored without a width takes one unit when it fits, and otherwise as many as it
needs, the most significant first."
  (flet ((codes (code)
           (run-form `(let* ((units nil) (enter (lambda (u) (push u units))))
                        ,(store-code-form (make-instance 'entry-target :enter 'enter) 8 code)
                        (reverse units)))))
    (is (equal '(65) (codes 65)))
    (is (equal '(#x20 #xAC) (codes #x20AC)))))

(def-test an-extent-check-reports-the-vector-s-size ()
  "A vector target's extent check passes when the units fit after the index, and otherwise
makes the report its caller gives, with a form of the vector's size."
  (flet ((check (size index count)
           (run-form `(let ((v (make-array ,size :element-type '(unsigned-byte 8))) (i ,index))
                        ,(extent-check-form (make-instance 'vector-target :vector 'v :index 'i) count
                                            (lambda (n) `(list :overrun ,n)))))))
    (is (null (check 8 4 4)))
    (is (equal '(:overrun 8) (check 8 5 4)))))

;;; ============================================================
;;; Loads
;;; ============================================================

(def-test a-load-reads-a-value-in-its-order-from-either-source ()
  "A load assembles a value from its units in its order, from a vector directly or through
the general source, which reads a vector or an integer, and gives a signed value its sign."
  (let ((v (make-array 4 :element-type '(unsigned-byte 8) :initial-contents '(1 2 3 4))))
    (dolist (case '((:big #x01020304) (:little #x04030201) (:pdp #x02010403)
                    ((:little :big 2) #x03040102)))
      (destructuring-bind (order expected) case
        (let ((encoding (make-encoding 4 8 :order order)))
          (is (eql expected
                   (run-form `(let ((v ,v))
                                ,(load-form (make-instance 'vector-source :vector 'v) encoding 0)))))
          (is (eql expected
                   (run-form `(let ((s ,v))
                                ,(load-form (make-instance 'general-source :source 's :unit-power 0)
                                            encoding 0)))))
          (is (eql expected
                   (run-form `(let ((s #x01020304))
                                ,(load-form (make-instance 'general-source :source 's :unit-power 0)
                                            encoding 0))))))))
    (is (eql -2 (run-form `(let ((v ,(make-array 2 :element-type '(unsigned-byte 8)
                                                  :initial-contents '(#xFE #xFF))))
                             ,(load-form (make-instance 'vector-source :vector 'v)
                                         (make-encoding 2 8 :order :little :signed t) 0)))))))

(def-test a-store-and-a-load-in-one-order-round-trip ()
  "A value stored in an order and loaded in the same order is the value, for units of 16
bits as of 8."
  (dolist (order '(:big :little :pdp (:little :big 2)))
    (let* ((encoding (make-encoding 4 16 :order order))
           (v (run-form `(let ((v (make-array 4 :element-type '(unsigned-byte 16))) (i 0))
                           ,(store-form (make-instance 'vector-target :vector 'v :index 'i)
                                        encoding '(identity #x0123456789ABCDEF))
                           v))))
      (is (eql #x0123456789ABCDEF
               (run-form `(let ((v ,v))
                            ,(load-form (make-instance 'vector-source :vector 'v) encoding 0))))))))

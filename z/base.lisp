;;;; base.lisp

(in-package #:specops.z)

(defclass z-register (register)
  ())

(defclass z-gpregister (z-register)
  ((%width :accessor   reg-width
           :allocation :class
           :initform   64
           :initarg    :width
           :documentation "The register's width.")))

(defclass z-fpregister (z-register)
  ((%width :accessor   reg-width
           :allocation :class
           :initform   64
           :initarg    :width
           :documentation "The register's width.")))

(defclass z-ctregister (z-register)
  ((%width :accessor   reg-width
           :allocation :class
           :initform   64
           :initarg    :width
           :documentation "The register's width.")))

(defclass z-acregister (z-register)
  ((%width :accessor   reg-width
           :allocation :class
           :initform   32
           :initarg    :width
           :documentation "The register's width.")))

(defclass z-vcregister (z-register)
  ())

(defclass z-mas (mas-based mas-indexed mas-displaced)
  ())

(defclass z-mras (z-mas)
  ((%length :accessor z-mras-length
            :initform nil
            :initarg  :length))
  (:documentation "A memory range access scheme, defining access to a range of memory."))

(defgeneric z-masd-rs12 (z-mas))

(defgeneric z-masd-lo12 (z-mas))

(defgeneric encoded-mras-length (z-mras))

(defgeneric z-vcr-loix (z-vcregister))

(defmethod encoded-mras-length ((z-mras z-mras))
  (if (zerop (z-mras-length z-mras))
      0 (1- (z-mras-length z-mras))))

(defmethod z-masd-rs12 ((z-mas z-mas))
  (ash (mas-displ z-mas) -12))

(defmethod z-masd-lo12 ((z-mas z-mas))
  (logand #xFFF (mas-displ z-mas)))

(defgeneric z-vcr-loix ((z-vcregister z-vcregister))
  (logand #xF (reg-index z-vcregister)))

;; (defvar *z-storage*)
(defvar *assembler-prototype-z*)

;; (setf *z-storage*
;;       (list :gpr (vector (make-instance 'z-gpregister :name :r0  :index  0)
;;                          (make-instance 'z-gpregister :name :r1  :index  1)
;;                          (make-instance 'z-gpregister :name :r2  :index  2)
;;                          (make-instance 'z-gpregister :name :r3  :index  3)
;;                          (make-instance 'z-gpregister :name :r4  :index  4)
;;                          (make-instance 'z-gpregister :name :r5  :index  5)
;;                          (make-instance 'z-gpregister :name :r6  :index  6)
;;                          (make-instance 'z-gpregister :name :r7  :index  7)
;;                          (make-instance 'z-gpregister :name :r8  :index  8)
;;                          (make-instance 'z-gpregister :name :r9  :index  9)
;;                          (make-instance 'z-gpregister :name :r10 :index 10)
;;                          (make-instance 'z-gpregister :name :r11 :index 11)
;;                          (make-instance 'z-gpregister :name :r12 :index 12)
;;                          (make-instance 'z-gpregister :name :r13 :index 13)
;;                          (make-instance 'z-gpregister :name :r14 :index 14)
;;                          (make-instance 'z-gpregister :name :r15 :index 15))
;;             :fpr (vector (make-instance 'z-fpregister :name :f0  :index  0)
;;                          (make-instance 'z-fpregister :name :f1  :index  1)
;;                          (make-instance 'z-fpregister :name :f2  :index  2)
;;                          (make-instance 'z-fpregister :name :f3  :index  3)
;;                          (make-instance 'z-fpregister :name :f4  :index  4)
;;                          (make-instance 'z-fpregister :name :f5  :index  5)
;;                          (make-instance 'z-fpregister :name :f6  :index  6)
;;                          (make-instance 'z-fpregister :name :f7  :index  7)
;;                          (make-instance 'z-fpregister :name :f8  :index  8)
;;                          (make-instance 'z-fpregister :name :f9  :index  9)
;;                          (make-instance 'z-fpregister :name :f10 :index 10)
;;                          (make-instance 'z-fpregister :name :f11 :index 11)
;;                          (make-instance 'z-fpregister :name :f12 :index 12)
;;                          (make-instance 'z-fpregister :name :f13 :index 13)
;;                          (make-instance 'z-fpregister :name :f14 :index 14)
;;                          (make-instance 'z-fpregister :name :f15 :index 15))
;;             :ctr (vector (make-instance 'z-ctregister :name :d0 :index  0)
;;                          (make-instance 'z-ctregister :name :d1 :index  1)
;;                          (make-instance 'z-ctregister :name :d2 :index  2)
;;                          (make-instance 'z-ctregister :name :d3 :index  3)
;;                          (make-instance 'z-ctregister :name :d4 :index  4)
;;                          (make-instance 'z-ctregister :name :d5 :index  5)
;;                          (make-instance 'z-ctregister :name :d6 :index  6)
;;                          (make-instance 'z-ctregister :name :d7 :index  7)
;;                          (make-instance 'z-ctregister :name :d7 :index  8)
;;                          (make-instance 'z-ctregister :name :d7 :index  9)
;;                          (make-instance 'z-ctregister :name :d7 :index 10)
;;                          (make-instance 'z-ctregister :name :d7 :index 11)
;;                          (make-instance 'z-ctregister :name :d7 :index 12)
;;                          (make-instance 'z-ctregister :name :d7 :index 13)
;;                          (make-instance 'z-ctregister :name :d7 :index 14)
;;                          (make-instance 'z-ctregister :name :d7 :index 15))
;;             :acr (vector (make-instance 'z-acregister :name :a0 :index 0)
;;                          (make-instance 'z-acregister :name :a1 :index 1)
;;                          (make-instance 'z-acregister :name :a2 :index 2)
;;                          (make-instance 'z-acregister :name :a3 :index 3)
;;                          (make-instance 'z-acregister :name :a4 :index 4)
;;                          (make-instance 'z-acregister :name :a5 :index 5)
;;                          (make-instance 'z-acregister :name :a6 :index 6)
;;                          (make-instance 'z-acregister :name :a7 :index 7)
;;                          (make-instance 'z-acregister :name :a7 :index 8)
;;                          (make-instance 'z-acregister :name :a7 :index 9)
;;                          (make-instance 'z-acregister :name :a7 :index 10)
;;                          (make-instance 'z-acregister :name :a7 :index 11)
;;                          (make-instance 'z-acregister :name :a7 :index 12)
;;                          (make-instance 'z-acregister :name :a7 :index 13)
;;                          (make-instance 'z-acregister :name :a7 :index 14)
;;                          (make-instance 'z-acregister :name :a7 :index 15))
;;             :vcr (vector (make-instance 'z-vcregister :name :a0 :index 0)
;;                          (make-instance 'z-vcregister :name :a1 :index 1)
;;                          (make-instance 'z-vcregister :name :a2 :index 2)
;;                          (make-instance 'z-vcregister :name :a3 :index 3)
;;                          (make-instance 'z-vcregister :name :a4 :index 4)
;;                          (make-instance 'z-vcregister :name :a5 :index 5)
;;                          (make-instance 'z-vcregister :name :a6 :index 6)
;;                          (make-instance 'z-vcregister :name :a7 :index 7)
;;                          (make-instance 'z-vcregister :name :a7 :index 8)
;;                          (make-instance 'z-vcregister :name :a7 :index 9)
;;                          (make-instance 'z-vcregister :name :a7 :index 10)
;;                          (make-instance 'z-vcregister :name :a7 :index 11)
;;                          (make-instance 'z-vcregister :name :a7 :index 12)
;;                          (make-instance 'z-vcregister :name :a7 :index 13)
;;                          (make-instance 'z-vcregister :name :a7 :index 14)
;;                          (make-instance 'z-vcregister :name :a7 :index 15))))

(defclass assembler-z (assembler)
  ((%storage :accessor   asm-storage
             :allocation :class
             :initform   (make-hash-table :test #'eq)
             :initarg    :storage)
   (%lexicon :accessor   asm-lexicon
             :allocation :class
             :initform   (make-hash-table :test #'eq)
             :initarg    :lexicon)
   (%domains :accessor   asm-domains
             :initform   nil
             :initarg    :domains)
   (%joiner  :accessor   asm-joiner
             :allocation :class
             :initform   #'joinw
             :initarg    :joiner)))


;; IBM's docs count the operands from 1 and these functions do the same

(defmacro rs8 (number)
  (list 'ash number -8))

(defmacro rs4 (number)
  (list 'ash number -4))

(defmacro lo4 (number)
  (list 'logand number #x0F))

(defmacro lo8 (number)
  (list 'logand number #xFF))

(defun vrmsbits (v1 &optional v2 v3 v4)
  "Get most significant bits from indices of vector registers passed as operands to a vector instruction. Used to populate the RXB fields of vector instruction codes."
  (+ (if v1 (ash (logand #b10000 (reg-index v1)) -1) 0)
     (if v2 (ash (logand #b10000 (reg-index v2)) -2) 0)
     (if v3 (ash (logand #b10000 (reg-index v3)) -3) 0)
     (if v4 (ash (logand #b10000 (reg-index v4)) -4) 0)))

(defmacro specop-z (mnemonic format opcode)
  `(specops ,mnemonic nil *assembler-prototype-z* (,format ,opcode)))

(defmacro zformat-e (opc)
  
  opc)

(mqbase zformat-i opc (i1)
    "AAAAAAAA.IIIIIIII"
  ((:static (a opc)))
  (i i1))

(mqbase zformat-ie opc (i1 i2)
    "AAAAAAAA.AAAAAAAA.00000000.IIIIJJJJ"
  ((:static (a opc)))
  (i i1) (j i2))

(mqbase zformat-mii opc (m1 ri1 ri2)
    "AAAAAAAA.MMMMRRRR.RRRRRRRR.IIIIIIII.IIIIIIII.IIIIIIII"
  ((:static (a opc)))
  (m m1) (r ri1) (i ri2))

(mqbase zformat-ri-a opc (r1 i2)
    "AAAAAAAA.RRRRZZZZ.IIIIIIII.IIIIIIII"
  ((:static (a (rs4 opc)) (z (lo4 opc))))
  (r (reg-index r1)) (i i2))

(mqbase zformat-ri-b opc (r1 i2)
    "AAAAAAAA.RRRRZZZZ.IIIIIIII.IIIIIIII"
  ((:static (a (rs4 opc)) (z (lo4 opc))))
  (r (reg-index r1)) (i i2))

(mqbase zformat-ri-c opc (m1 ri2)
    "AAAAAAAA.MMMMZZZZ.IIIIIIII.IIIIIIII"
  ((:static (a (rs4 opc)) (z (lo4 opc))))
  (m m1) (i ri2))

(mqbase zformat-rie-a opc (r1 i2 m3)
    "AAAAAAAA.RRRR0000.IIIIIIII.IIIIIIII.MMMM0000.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc))))
  (r (reg-index r1)) (i i2) (m m3))

(mqbase zformat-rie-b opc (r1 r2 m3 ri4)
    "AAAAAAAA.RRRRSSSS.IIIIIIII.IIIIIIII.MMMM0000.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc))))
  (r (reg-index r1)) (s (reg-index r2)) (i ri4) (m m3))

(mqbase zformat-rie-c opc (r1 i2 m3 ri4)
    "AAAAAAAA.RRRRMMMM.IIIIIIII.IIIIIIII.JJJJJJJJ.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc))))
  (r (reg-index r1)) (m m3) (i ri4) (j i2))

(mqbase zformat-rie-d opc (r1 i2 r3)
    "AAAAAAAA.RRRRSSSS.IIIIIIII.IIIIIIII.00000000.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc))))
  (r (reg-index r1)) (s (reg-index r3)) (i i2))

(mqbase zformat-rie-e opc (r1 ri2 r3)
    "AAAAAAAA.RRRRSSSS.IIIIIIII.IIIIIIII.00000000.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc))))
  (r (reg-index r1)) (s (reg-index r3)) (i ri2))

(mqbase zformat-rie-f opc (r1 r2 i3 i4 i5)
    "AAAAAAAA.RRRRSSSS.IIIIIIII.JJJJJJJJ.KKKKKKKK.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc))))
  (r (reg-index r1)) (s (reg-index r2)) (i i3) (j i4) (k i5))

(mqbase zformat-rie-g opc (r1 i2 m3)
    "AAAAAAAA.RRRRMMMM.IIIIIIII.IIIIIIII.00000000.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc))))
  (r (reg-index r1)) (m m3) (i i2))

(mqbase zformat-ril-a opc (r1 i2)
    "AAAAAAAA.RRRRZZZZ.IIIIIIII.IIIIIIII.IIIIIIII.IIIIIIII"
  ((:static (a (rs4 opc)) (z (lo4 opc))))
  (r (reg-index r1)) (i i2))

(mqbase zformat-ril-b opc (r1 ri2)
    "AAAAAAAA.RRRRZZZZ.IIIIIIII.IIIIIIII.IIIIIIII.IIIIIIII"
  ((:static (a (rs4 opc)) (z (lo4 opc))))
  (r (reg-index r1)) (i ri2))

(mqbase zformat-ril-c opc (m1 ri2)
    "AAAAAAAA.RRRRZZZZ.IIIIIIII.IIIIIIII.IIIIIIII.IIIIIIII"
  ((:static (a (rs4 opc)) (z (lo4 opc))))
  (r m1) (i ri2))

(mqbase zformat-ris opc (r1 i2 m3 bd4)
    "AAAAAAAA.RRRRMMMM.BBBBDDDD.DDDDDDDD.IIIIIIII.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc))))
  (r (reg-index r1)) (m m3)
  (b (mas-base bd4)) (d (mas-displ bd4)) (i i2))

(mqbase zformat-rr opc (r1 r2)
    "AAAAAAAA.RRRRSSSS"
  ((:static (a opc)))
  (r (reg-index r1)) (s (reg-index r2)))

(mqbase zformat-rrd opc (r1 r2 r3)
    "AAAAAAAA.AAAAAAAA.RRRR0000.SSSSTTTT"
  ((:static (a opc)))
  (r (reg-index r1)) (s (reg-index r2)) (t (reg-index r3)))

(mqbase zformat-rre opc (r1 r2)
    "AAAAAAAA.AAAAAAAA.00000000.RRRRSSSS"
  ((:static (a opc)))
  (r (reg-index r1)) (s (reg-index r2)))

(mqbase zformat-rrf-a opc (r1 r2 r3 m4)
    "AAAAAAAA.AAAAAAAA.RRRRMMMM.SSSSTTTT"
  ((:static (a opc)))
  (r (reg-index r3)) (m m4) (s (reg-index r1)) (t (reg-index r2)))

(mqbase zformat-rrf-b opc (r1 r2 r3 m4)
    "AAAAAAAA.AAAAAAAA.RRRRMMMM.SSSSTTTT"
  ((:static (a opc)))
  (r (reg-index r3)) (m m4) (s (reg-index r1)) (t (reg-index r2)))

(mqbase zformat-rrf-c opc (r1 r2 m3 m4)
    "AAAAAAAA.AAAAAAAA.MMMMNNNN.RRRRSSSS"
  ((:static (a opc)))
  (m m3) (n m4) (r (reg-index r1)) (s (reg-index r2)))

(mqbase zformat-rrf-d opc (r1 r2 m3 m4)
    "AAAAAAAA.AAAAAAAA.MMMMNNNN.RRRRSSSS"
  ((:static (a opc)))
  (m m3) (n m4) (r (reg-index r1)) (s (reg-index r2)))

(mqbase zformat-rrf-e opc (r1 r2 m3 m4)
    "AAAAAAAA.AAAAAAAA.MMMMNNNN.RRRRSSSS"
  ((:static (a opc)))
  (m m3) (n m4) (r (reg-index r1)) (s (reg-index r2)))

(mqbase zformat-rrs opc (r1 r2 m3 bd4)
    "AAAAAAAA.RRRRSSSS.BBBBDDDD.DDDDDDDD.MMMM0000.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc))))
  (r (reg-index r1)) (s (reg-index r2))
  (b (mas-base bd4)) (d (mas-displ bd4)) (m m3))

(mqbase zformat-rs-a opc (r1 bd2 r3)
    "AAAAAAAA.RRRRSSSS.BBBBDDDD.DDDDDDDD"
  ((:static (a opc)))
  (r (reg-index r1)) (s (reg-index r3))
  (b (mas-base bd2)) (d (mas-displ bd2)))

(mqbase zformat-rs-b opc (r1 bd2 m3)
    "AAAAAAAA.RRRRMMMM.BBBBDDDD.DDDDDDDD"
  ((:static (a opc)))
  (r (reg-index r1)) (m m3)
  (b (mas-base bd2)) (d (mas-displ bd2)))

(mqbase zformat-rsi opc (r1 ri2 r3)
    "AAAAAAAA.RRRRSSSS.IIIIIIII.IIIIIIII"
  ((:static (a opc)))
  (r (reg-index r1)) (s (reg-index r3)) (i ri2))

(mqbase zformat-rsl-a opc (bld1)
    "AAAAAAAA.LLLL0000.BBBBDDDD.DDDDDDDD.00000000.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc))))
  (l (encoded-mras-length bld1)) (b (mas-base bld1)) (d (mas-displ bld1)))

(mqbase zformat-rsl-b opc (bld1)
    "AAAAAAAA.LLLLLLLL.BBBBDDDD.DDDDDDDD.00000000.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc))))
  (l (encoded-mras-length bld1)) (b (mas-base bld1)) (d (mas-displ bld1)))

(mqbase zformat-rsy-a opc (r1 bdd2 r3)
    "AAAAAAAA.RRRRSSSS.BBBBDDDD.DDDDDDDD.HHHHHHHH.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc))))
  (r (reg-index r1)) (s (reg-index r3))
  (b (mas-base bdd2)) (d (z-masd-lo12 bdd2)) (h (z-masd-rs12 bdd2)))

(mqbase zformat-rsy-b opc (r1 bdd2 m3)
    "AAAAAAAA.RRRRMMMM.BBBBDDDD.DDDDDDDD.HHHHHHHH.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc))))
  (r (reg-index r1)) (m m3)
  (b (mas-base bdd2)) (d (z-masd-lo12 bdd2)) (h (z-masd-rs12 bdd2)))

(mqbase zformat-rx-a opc (r1 bdx2)
    "AAAAAAAA.RRRRXXXX.BBBBDDDD.DDDDDDDD"
  ((:static (a opc)))
  (r (reg-index r1))
  (x (mas-index bdx2)) (b (mas-base bdx2)) (d (mas-displ bdx2)))

(mqbase zformat-rx-b opc (m1 bdx2)
    "AAAAAAAA.MMMMXXXX.BBBBDDDD.DDDDDDDD"
  ((:static (a opc)))
  (m m1) (x (mas-index bdx2)) (b (mas-base bdx2)) (d (mas-displ bdx2)))

(mqbase zformat-rxe opc (r1 bdx2 m3)
    "AAAAAAAA.RRRRXXXX.BBBBDDDD.DDDDDDDD.MMMM0000.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc))))
  (r (reg-index r1))
  (x (mas-index bdx2)) (b (mas-base bdx2)) (d (mas-displ bdx2)) (m m3))

(mqbase zformat-rxf opc (r1 bdx2 r3)
    "AAAAAAAA.RRRRXXXX.BBBBDDDD.DDDDDDDD.SSSS0000.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc))))
  (r (reg-index r3))
  (x (mas-index bdx2)) (b (mas-base bdx2)) (d (mas-displ bdx2))
  (s (reg-index r1)))

(mqbase zformat-rxy-a opc (r1 bddx2)
    "AAAAAAAA.RRRRXXXX.BBBBDDDD.DDDDDDDD.HHHHHHHH.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc))))
  (r (reg-index r1)) (x (mas-index bddx2))
  (b (mas-base bddx2)) (d (z-masd-lo12 bddx2)) (h (z-masd-rs12 bddx2)))

(mqbase zformat-rxy-b opc (m1 bddx2)
    "AAAAAAAA.MMMMXXXX.BBBBDDDD.DDDDDDDD.HHHHHHHH.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc))))
  (m m1) (x (mas-index bddx2))
  (b (mas-base bddx2)) (d (z-masd-lo12 bddx2)) (h (z-masd-rs12 bddx2)))

(mqbase zformat-s opc (bd1)
    "AAAAAAAA.AAAAAAAA.BBBBDDDD.DDDDDDDD"
  ((:static (a opc)))
  (b (mas-base bd1)) (d (mas-displ bd1)))

(mqbase zformat-si opc (bd1 i2)
    "AAAAAAAA.IIIIIIII.BBBBDDDD.DDDDDDDD"
  ((:static (a opc)))
  (i i2) (b (mas-base bd1)) (d (mas-displ bd1)))

(mqbase zformat-sil opc (bd1 i2)
    "AAAAAAAA.AAAAAAAA.BBBBDDDD.DDDDDDDD.IIIIIIII.IIIIIIII"
  ((:static (a opc)))
  (b (mas-base bd1)) (d (mas-displ bd1)) (i i2))

(mqbase zformat-siy opc (bdd1 i2)
    "AAAAAAAA.IIIIIIII.BBBBDDDD.DDDDDDDD.HHHHHHHH.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc))))
  (i i2) (b (mas-base bdd1)) (d (z-masd-lo12 bdd1)) (h (z-masd-rs12 bdd1)))

(mqbase zformat-smi opc (m1 ri2 bd3)
    "AAAAAAAA.MMMM0000.BBBBDDDD.DDDDDDDD.RRRRRRRR.RRRRRRRR"
  ((:static (a opc)))
  (m m1) (b (mas-base bd3)) (d (mas-displ bd3)) (r ri2))

(mqbase zformat-ss-a opc (bld1 bd2)
    "AAAAAAAA.LLLLLLLL.BBBBDDDD.DDDDDDDD.EEEEFFFF.FFFFFFFF"
  ((:static (a opc)))
  (l (encoded-mras-length bld1))
  (b (mas-base bdl1)) (d (mas-displ bld1))
  (e (mas-base  bd2)) (f (mas-displ  bd2)))

(mqbase zformat-ss-b opc (bld1 bld2)
    "AAAAAAAA.LLLLMMMM.BBBBDDDD.DDDDDDDD.EEEEFFFF.FFFFFFFF"
  ((:static (a opc)))
  (l (encoded-mras-length bld1)) (m (encoded-mras-length bld2))
  (b (mas-base bdl1)) (d (mas-displ bld1))
  (e (mas-base bld2)) (f (mas-displ bld2)))

(mqbase zformat-ss-c opc (bld1 bd2 i3)
    "AAAAAAAA.LLLLIIII.BBBBDDDD.DDDDDDDD.EEEEFFFF.FFFFFFFF"
  ((:static (a opc)))
  (l (encoded-mras-length bld1)) (i i3)
  (b (mas-base bld1)) (d (mas-displ bld1))
  (e (mas-base  bd2)) (f (mas-displ  bd2)))

(mqbase zformat-ss-d opc (r1 bd1 b2 d2 r3)
    "AAAAAAAA.RRRRSSSS.BBBBDDDD.DDDDDDDD.EEEEFFFF.FFFFFFFF"
  ((:static (a opc))) ;; ?? how does this work ??
  (r (reg-index r1)) (s r3)
  (b (mas-base bd1)) (d (mas-displ bd1))
  (e (mas-base bd2)) (f (mas-displ bd2)))

(mqbase zformat-ss-e opc (r1 bd2 r3 bd4)
    "AAAAAAAA.RRRRSSSS.BBBBDDDD.DDDDDDDD.EEEEFFFF.FFFFFFFF"
  ((:static (a opc)))
  (r (reg-index r1)) (s (reg-index r3))
  (b (mas-base bd2)) (d (mas-displ bd2))
  (e (mas-base bd4)) (f (mas-displ bd4)))

(mqbase zformat-ss-f opc (bd1 bld2)
    "AAAAAAAA.LLLLLLLL.BBBBDDDD.DDDDDDDD.EEEEFFFF.FFFFFFFF"
  ((:static (a opc)))
  (l (encoded-mras-length bld2))
  (b (mas-base  bd1)) (d (mas-displ  bd1))
  (e (mas-base bld2)) (f (mas-displ bld2)))

(mqbase zformat-sse opc (bd1 bd2)
    "AAAAAAAA.AAAAAAAA.BBBBDDDD.DDDDDDDD.EEEEFFFF.FFFFFFFF"
  ((:static (a opc)))
  (b (mas-base bd1)) (d (mas-displ bd1))
  (e (mas-base bd2)) (f (mas-displ bd2)))

(mqbase zformat-ssf opc (op bd1 bd2 r3)
    "AAAAAAAA.RRRRZZZZ.BBBBDDDD.DDDDDDDD.EEEEFFFF.FFFFFFFF"
  ((:static (a (rs4 opc)) (z (lo4 opc))))
  (r (reg-index r3))
  (b (mas-base bd1)) (d (mas-displ bd1))
  (e (mas-base bd2)) (f (mas-displ bd2)))

(mqbase zformat-vri-a opc (v1 i2 m3)
    "AAAAAAAA.VVVV0000.IIIIIIII.IIIIIIII.MMMMYYYY.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc))))
  (v (z-vcr-loix v1)) (i i2) (m m3) (y (vrmsbits v1)))

(mqbase zformat-vri-b opc (v1 i2 i3 m4)
    "AAAAAAAA.VVVV0000.IIIIIIII.JJJJJJJJ.MMMMYYYY.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc))))
  (v (z-vcr-loix v1)) (i i2) (j i3) (m m4) (y (vrmsbits v1)))

(mqbase zformat-vri-c opc (v1 i2 v3 i3 m4)
    "AAAAAAAA.VVVVWWWW.IIIIIIII.IIIIIIII.MMMMYYYY.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc))))
  (v (z-vcr-loix v1)) (w (z-vcr-loix v3))
  (i i2) (m m4) (y (vrmsbits v1 nil v3)))

(mqbase zformat-vri-d opc (v1 v2 v3 i4 m5)
  "AAAAAAAA.UUUUVVVV.WWWW0000.IIIIIIII.MMMMYYYY.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc))))
  (u (z-vcr-loix v1)) (v (z-vcr-loix v2)) (w (z-vcr-loix v3))
  (i i4) (m m5) (y (vrmsbits v1 v2 v3)))

(mqbase zformat-vri-e opc (v1 v2 i3 m4 m5)
    "AAAAAAAA.VVVVWWWW.IIIIIIII.IIIIMMMM.NNNNYYYY.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc))))
  (v (z-vcr-loix v1)) (w (z-vcr-loix v2))
  (i i3) (m m5) (n m4) (y (vrmsbits v1 v2)))

(mqbase zformat-vri-f opc (v1 v2 v3 i4 m5)
    "AAAAAAAA.UUUUVVVV.WWWW0000.MMMMIIII.IIIIYYYY.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc))))
  (u (z-vcr-loix v1)) (v (z-vcr-loix v2)) (w (z-vcr-loix v3))
  (m m5) (i i4) (y (vrmsbits v1 v2 v3)))

(mqbase zformat-vri-g opc (v1 v2 i3 i4 m5)
    "AAAAAAAA.VVVVWWWW.IIIIIIII.MMMMJJJJ.JJJJYYYY.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc))))
  (v (z-vcr-loix v1)) (w (z-vcr-loix v2))
  (i i4) (m m5) (j i3) (y (vrmsbits v1 v2)))

(mqbase zformat-vri-h opc (v1 v2 i3 i4 m5)
    "AAAAAAAA.VVVV0000.IIIIIIII.IIIIIIII.JJJJYYYY.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc))))
  (v (z-vcr-loix v1)) (i i2) (j i3) (y (vrmsbits v1)))

(mqbase zformat-vri-i opc (v1 r2 i3 m4)
    "AAAAAAAA.VVVVRRRR.00000000.MMMMIIII.IIIIYYYY.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc))))
  (v (z-vcr-loix v1)) (r (reg-index r2))
  (m m4) (i i3) (y (vrmsbits v1)))

(mqbase zformat-vrr-a opc (v1 v2 m3 m4 m5)
    "AAAAAAAA.VVVVWWWW.00000000.MMMMNNNN.PPPPYYYY.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc))))
  (v (z-vcr-loix v1)) (w (z-vcr-loix v2))
  (m m5) (n m4) (p m3) (y (vrmsbits v1 v2)))

(mqbase zformat-vrr-b opc (v1 v2 v3 m4 m5)
    "AAAAAAAA.UUUUVVVV.WWWW0000.MMMM0000.NNNNYYYY.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc))))
  (u (z-vcr-loix v1)) (v (z-vcr-loix v2)) (w (z-vcr-loix v3))
  (m m5) (n m4) (y (vrmsbits v1 v2 v3)))

(mqbase zformat-vrr-c opc (v1 v2 v3 m4 m5 m6)
    "AAAAAAAA.UUUUVVVV.WWWW0000.MMMMNNNN.PPPPYYYY.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc))))
  (u (z-vcr-loix v1)) (v (z-vcr-loix v2)) (w (z-vcr-loix v3))
  (m m6) (n m5) (p m4) (y (vrmsbits v1 v2 v3)))

(mqbase zformat-vrr-d opc (v1 v2 v3 v4 m5 m6)
    "AAAAAAAA.TTTTUUUU.VVVVMMMM.NNNN0000.WWWWYYYY.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc))))
  (t (z-vcr-loix v1)) (u (z-vcr-loix v2)) (v (z-vcr-loix v3))
  (m m5) (n m6) (w (z-vcr-loix v4)) (y (vrmsbits v1 v2 v3 v4)))

(mqbase zformat-vrr-e opc (v1 v2 v3 v4 m5 m6)
    "AAAAAAAA.TTTTUUUU.VVVVMMMM.0000NNNN.WWWWYYYY.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc))))
  (t (z-vcr-loix v1)) (u (z-vcr-loix v2)) (v (z-vcr-loix v3))
  (m m6) (n m5) (w (z-vcr-loix v4)) (y (vrmsbits v1 v2 v3 v4)))

(mqbase zformat-vrr-f opc (v1 r2 r3)
    "AAAAAAAA.VVVVRRRR.SSSS0000.00000000.0000YYYY.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc))))
  (v (z-vcr-loix v1)) (r (reg-index r2)) (s (reg-index r3)) (y (vrmsbits v1)))

(mqbase zformat-vrr-g opc (v1)
    "AAAAAAAA.0000VVVV.00000000.00000000.0000YYYY.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc))))
  (v (z-vcr-loix v1)) (y (vrmsbits v1)))

(mqbase zformat-vrr-h opc (v1 v2 m3)
    "AAAAAAAA.0000VVVV.WWWW0000.MMMM0000.0000YYYY.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc))))
  (v (z-vcr-loix v1)) (w (z-vcr-loix v2)) (m m3) (y (vrmsbits v1 v2)))

(mqbase zformat-vrr-i opc (r1 v2 m3 m4)
    "AAAAAAAA.RRRRVVVV.00000000.MMMMNNNN.0000YYYY.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc))))
  (r (reg-index r1)) (v (z-vcr-loix v2))
  (m m3) (n m4) (y (vrmsbits nil v2)))

(mqbase zformat-vrs-a opc (v1 bd2 v3 m4)
    "AAAAAAAA.VVVVWWWW.BBBBDDDD.DDDDDDDD.MMMMYYYY.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc))))
  (v (z-vcr-loix v1)) (w (z-vcr-loix v3))
  (b (mas-base bd2)) (d (mas-displ bd2))
  (m m4) (y (vrmsbits v1 nil v3)))

(mqbase zformat-vrs-b opc (v1 bd2 r3 m4)
    "AAAAAAAA.VVVVRRRR.BBBBDDDD.DDDDDDDD.MMMMYYYY.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc))))
  (v (z-vcr-loix v1)) (r (reg-index r3))
  (b (mas-base bd2)) (d (mas-displ bd2))
  (m m4) (y (vrmsbits v1)))

(mqbase zformat-vrs-c opc (r1 bd2 v3 m4)
    "AAAAAAAA.RRRRVVVV.BBBBDDDD.DDDDDDDD.MMMMYYYY.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc))))
  (r (reg-index r1)) (v (z-vcr-loix v3))
  (b (mas-base bd2)) (d (mas-displ bd2))
  (m m4) (y (vrmsbits nil nil v3)))

(mqbase zformat-vrs-d opc (v1 bd2 r3)
    "AAAAAAAA.0000RRRR.BBBBDDDD.DDDDDDDD.VVVVYYYY.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc))))
  (r (reg-index r3)) (b (mas-base bd2)) (d (mas-displ bd2))
  (v (z-vcr-loix v1)) (y (vrmsbits v1)))

(mqbase zformat-vrv opc (v1 bd2 v2 m3) ;; ?? how does this work ??
    "AAAAAAAA.VVVVWWWW.BBBBDDDD.DDDDDDDD.MMMMYYYY.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc))))
  (v (z-vcr-loix v1)) (w (z-vcr-loix v2))
  (b (mas-base bd2)) (d (mas-displ bd2))
  (m m3) (y (vrmsbits v1 v2)))

(mqbase zformat-vrx opc (v1 bdx2 m3)
    "AAAAAAAA.VVVVXXXX.BBBBDDDD.DDDDDDDD.MMMMYYYY.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc))))
  (v (z-vcr-loix v1))
  (x (mas-index bdx2)) (b (mas-base bdx2)) (d (mas-displ bdx2))
  (m m3) (y (vrmsbits v1)))

(mqbase zformat-vsi opc (v1 bd2 i3)
    "AAAAAAAA.IIIIIIII.BBBBDDDD.DDDDDDDD.VVVVYYYY.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc))))
  (i i3) (b (mas-base bd2)) (d (mas-displ bd2))
  (v (z-vcr-loix v1)) (y (vrmsbits v1)))



#|

(defun zformat-e (opc)
  opc)

(defun zformat-i (opc imm)
  (masque "CCCCCCCC.NNNNNNNN"
          (c opc) (n imm)))

(defun zformat-ie (opc imm1 imm2)
  (masque "CCCCCCCC.CCCCCCCC.00000000.MMMMNNNN"
          (c opc) (m imm1) (n imm2)))

(defun zformat-mii (opc m1 ri1 ri2)
  (masque "CCCCCCCC.MMMMRRRR.RRRRRRRR.IIIIIIII.IIIIIIII.IIIIIIII"
          (c opc) (m m1) (r ri1) (i ri2)))

(defun zformat-ri-abc (opc r op iri)
  (masque "CCCCCCCC.RRRROOOO.IIIIIIII.IIIIIIII"
          (c opc) (r r) (o op) (i iri)))

(defun zformat-rie-a (opc r1 i2 m3)
  (masque "CCCCCCCC.RRRR0000.IIIIIIII.IIIIIIII.MMMM0000.DDDDDDDD"
          (c opc) (r r1) (i i2) (m m3) (d (lo8 opc))))

(defun zformat-rie-b (opc r1 r2 m3 ri4)
  (masque "CCCCCCCC.RRRRSSSS.IIIIIIII.IIIIIIII.MMMM0000.DDDDDDDD"
          (c opc) (r r1) (s r2) (i ri4) (m m3) (d (lo8 opc))))

(defun zformat-rie-c (opc r1 i2 m3 ri4)
  (masque "CCCCCCCC.RRRRMMMM.IIIIIIII.IIIIIIII.JJJJJJJJ.DDDDDDDD"
          (c (hi8 opc)) (r r1) (m m3) (i ri4) (j i2) (d (lo8 opc))))

(defun zformat-rie-de (opc r1 i2 r3)
  (masque "CCCCCCCC.RRRRSSSS.IIIIIIII.IIIIIIII.00000000.DDDDDDDD"
          (c (hi8 opc)) (r r1) (s r3) (i i2) (d (lo8 opc))))

(defun zformat-rie-f (opc r1 r2 i3 i4 i5)
  (masque "CCCCCCCC.RRRRSSSS.IIIIIIII.JJJJJJJJ.KKKKKKKK.DDDDDDDD"
          (c (hi8 opc)) (r r1) (s r2) (i i3) (j i4) (k i5) (d (lo8 opc))))

(defun zformat-rie-g (opc r1 i2 m3)
  (masque "CCCCCCCC.RRRRMMMM.IIIIIIII.IIIIIIII.00000000.DDDDDDDD"
          (c opc) (r r1) (m m3) (i i2) (d (lo8 opc))))

(defun zformat-ril-a (opc r1 op i2)
  (masque "CCCCCCCC.RRRRPPPP.IIIIIIII.IIIIIIII.IIIIIIII.IIIIIIII"
          (c opc) (r r1) (p op) (i i2) (d (lo8 opc))))

(defun zformat-ril-b (opc r1 op ri2)
  (masque "CCCCCCCC.RRRRPPPP.IIIIIIII.IIIIIIII.IIIIIIII.IIIIIIII"
          (c opc) (r r1) (p op) (i ri2) (d (lo8 opc))))

(defun zformat-ril-c (opc m1 op ri2)
  (masque "CCCCCCCC.RRRRPPPP.IIIIIIII.IIIIIIII.IIIIIIII.IIIIIIII"
          (c opc) (r m1) (p op) (i ri2) (d (lo8 opc))))

(defun zformat-ris (opc r1 i2 m3 b4 d4)
  (masque "CCCCCCCC.RRRRMMMM.BBBBDDDD.DDDDDDDD.IIIIIIII.DDDDDDDD"
          (c opc) (r r1) (m m3) (b b4) (d d4) (i i2) (d (lo8 opc))))

(defun zformat-rr (opc r1 r2)
  (masque "CCCCCCCC.RRRRSSSS"
          (c opc) (r r1) (s r2)))

(defun zformat-rrd (opc r1 r2 r3)
  (masque "CCCCCCCC.CCCCCCCC.RRRR0000.SSSSTTTT"
          (c opc) (r r1) (s r2) (t r2)))

(defun zformat-rre (opc r1 r2)
  (masque "CCCCCCCC.CCCCCCCC.00000000.RRRRSSSS"
          (c opc) (r r1) (s r2)))

(defun zformat-rrf-ab (opc r1 r2 r3 m4)
  (masque "CCCCCCCC.CCCCCCCC.RRRRMMMM.SSSSTTTT"
          (c opc) (r r3) (m m4) (s r1) (t r2)))

(defun zformat-rrf-ce (opc r1 r2 m3 m4)
  (masque "CCCCCCCC.CCCCCCCC.MMMMNNNN.RRRRSSSS"
          (c opc) (m m3) (n m4) (r r1) (s r2)))

(defun zformat-rrs (opc r1 r2 m3 b4 d4)
  (masque "CCCCCCCC.RRRRSSSS.BBBBDDDD.DDDDDDDD.MMMM0000.XXXXXXXX"
          (c (hi8 opc)) (r r1) (s r2) (b b4) (d d4) (m m3) (x (lo8 opc))))

(defun zformat-rs-a (opc r1 b2 d2 r3)
  (masque "CCCCCCCC.RRRRSSSS.BBBBDDDD.DDDDDDDD"
          (c opc) (r r1) (s r3) (b b2) (d d2)))

(defun zformat-rs-b (opc r1 b2 d2 m3)
  (masque "CCCCCCCC.RRRRMMMM.BBBBDDDD.DDDDDDDD"
          (c opc) (r r1) (m m3) (b b2) (d d2)))

(defun zformat-rsi (opc r1 ri2 r3)
  (masque "CCCCCCCC.RRRRSSSS.IIIIIIII.IIIIIIII"
          (c opc) (r r1) (s r3) (i ri2)))

(defun zformat-rsl-a (opc l1 b2 d2)
  (masque "CCCCCCCC.LLLL0000.BBBBDDDD.DDDDDDDD.00000000.XXXXXXXX"
          (c (hi8 opc)) (l l1) (b b2) (d d2) (x (lo8 opc))))

(defun zformat-rsl-b (opc l1 b2 d2)
  (masque "CCCCCCCC.LLLLLLLL.BBBBDDDD.DDDDDDDD.00000000.XXXXXXXX"
          (c (hi8 opc)) (l l1) (b b2) (d d2) (x (lo8 opc))))

(defun zformat-rsy-a (opc r1 b2 r3 dl2 dh2)
  (masque "CCCCCCCC.RRRRSSSS.BBBBDDDD.DDDDDDDD.HHHHHHHH.XXXXXXXX"
          (c (hi8 opc)) (r r1) (s r3) (b b2) (d dl2) (h dh2) (x (lo8 opc))))

(defun zformat-rsy-b (opc r1 b2 m3 dl2 dh2)
  (masque "CCCCCCCC.RRRRMMMM.BBBBDDDD.DDDDDDDD.HHHHHHHH.XXXXXXXX"
          (c (hi8 opc)) (r r1) (m m3) (b b2) (d dl2) (h dh2) (x (lo8 opc))))

(defun zformat-rx-a (opc r1 b2 d2 x2)
  (masque "CCCCCCCC.RRRRXXXX.BBBBDDDD.DDDDDDDD"
          (c opc) (r r1) (x x2) (b b2) (d d2)))

(defun zformat-rx-b (opc m1 b2 d2 x2)
  (masque "CCCCCCCC.MMMMXXXX.BBBBDDDD.DDDDDDDD"
          (c opc) (m m1) (x x2) (b b2) (d d2)))

(defun zformat-rxe (opc r1 b2 d2 x2 m3)
  (masque "CCCCCCCC.RRRRXXXX.BBBBDDDD.DDDDDDDD.MMMM0000.XXXXXXXX"
          (c (hi8 opc)) (r r1) (x x2) (b b2) (d d2) (m m3) (x (lo8 opc))))

(defun zformat-rxf (opc r1 b2 d2 x2 r3)
  (masque "CCCCCCCC.RRRRXXXX.BBBBDDDD.DDDDDDDD.SSSS0000.XXXXXAXXX"
          (c (hi8 opc)) (r r3) (x x2) (b b2) (d d2) (s r1) (x (lo8 opc))))

(defun zformat-rxy-a (opc r1 b2 dl2 dh2 x2)
  (masque "CCCCCCCC.RRRRXXXX.BBBBDDDD.DDDDDDDD.HHHHHHHH.XXXXXXXX"
          (c (hi8 opc)) (r r1) (x x2) (b b2) (d dl2) (h dh2) (x (lo8 opc))))

(defun zformat-rxy-b (opc m1 b2 dl2 dh2 x2)
  (masque "CCCCCCCC.MMMMXXXX.BBBBDDDD.DDDDDDDD.HHHHHHHH.XXXXXXXX"
          (c (hi8 opc)) (m m1) (x x2) (b b2) (d dl2) (h dh2) (x (lo8 opc))))

(defun zformat-s (opc b2 d2)
  (masque "CCCCCCCC.CCCCCCCC.BBBBDDDD.DDDDDDDD"
          (c opc) (b b2) (d d2)))

(defun zformat-si (opc b1 d1 i2)
  (masque "CCCCCCCC.IIIIIIII.BBBBDDDD.DDDDDDDD"
          (c opc) (i i2) (b b1) (d d1)))

(defun zformat-sil (opc b1 d1 i1)
  (masque "CCCCCCCC.CCCCCCCC.BBBBDDDD.DDDDDDDD.IIIIIIII.IIIIIIII"
          (c opc) (b b1) (d d1) (i i2)))

(defun zformat-siy (opc b1 dl1 dh1 i2)
  (masque "CCCCCCCC.IIIIIIII.BBBBDDDD.DDDDDDDD.HHHHHHHH.XXXXXXXX"
          (c (hi8 opc)) (i i2) (b b1) (d dl1) (h dh1) (x (lo8 opc))))

(defun zformat-smi (opc m1 ri2 b3 d3)
  (masque "CCCCCCCC.MMMM0000.BBBBDDDD.DDDDDDDD.RRRRRRRR.RRRRRRRR"
          (c opc) (m m1) (b b3) (d d3) (r ri2)))

(defun zformat-ss-a (opc b1 d1 l1 b2 d2)
  (masque "CCCCCCCC.LLLLLLLL.BBBBDDDD.DDDDDDDD.EEEEFFFF.FFFFFFFF"
          (c opc) (l l1) (b b1) (d d1) (e b2) (f d2)))

(defun zformat-ss-b (opc b1 d1 l1 b2 d2 l2)
  (masque "CCCCCCCC.LLLLMMMM.BBBBDDDD.DDDDDDDD.EEEEFFFF.FFFFFFFF"
          (c opc) (l l1) (m l2) (b b1) (d d1) (e b2) (f d2)))

(defun zformat-ss-c (opc b1 d1 l1 b2 d2 i3)
  (masque "CCCCCCCC.LLLLIIII.BBBBDDDD.DDDDDDDD.EEEEFFFF.FFFFFFFF"
          (c opc) (l l1) (i i3) (b b1) (d d1) (e b2) (f d2)))

(defun zformat-ss-d (opc b1 d1 r1 b2 d2 r3)
  (masque "CCCCCCCC.RRRRSSSS.BBBBDDDD.DDDDDDDD.EEEEFFFF.FFFFFFFF"
          (c opc) (r r1) (s r3) (b b1) (d d1) (e b2) (f d2)))

(defun zformat-ss-e (opc r1 b2 d2 r3 b4 d4)
  (masque "CCCCCCCC.RRRRSSSS.BBBBDDDD.DDDDDDDD.EEEEFFFF.FFFFFFFF"
          (c opc) (r r1) (s r3) (b b2) (d d2) (e b4) (f d4)))

(defun zformat-ss-f (opc b1 d1 b2 d2 l2)
  (masque "CCCCCCCC.LLLLLLLL.BBBBDDDD.DDDDDDDD.EEEEFFFF.FFFFFFFF"
          (c opc) (l l2) (b b1) (d d1) (e b2) (f d2)))

(defun zformat-sse (opc b1 d1 b2 d2 l2)
  (masque "CCCCCCCC.CCCCCCCC.BBBBDDDD.DDDDDDDD.EEEEFFFF.FFFFFFFF"
          (c opc) (b b1) (d d1) (e b2) (f d2)))

(defun zformat-ssf (opc op b1 d1 b2 d2 r3)
  (masque "CCCCCCCC.RRRRPPPP.BBBBDDDD.DDDDDDDD.EEEEFFFF.FFFFFFFF"
          (c opc) (r r3) (p op) (b b1) (d d1) (e b2) (f d2)))

(defun zformat-vri-a (opc rxb v1 i2 m3)
  (masque "CCCCCCCC.VVVV0000.IIIIIIII.IIIIIIII.MMMMRRRR.DDDDDDDD"
          (c (hi8 opc)) (v v1) (i i2) (m m3) (r rxb) (d (lo8 opc))))

(defun zformat-vri-b (opc rxb v1 i2 i3 m4)
  (masque "CCCCCCCC.VVVV0000.IIIIIIII.JJJJJJJJ.MMMMRRRR.DDDDDDDD"
          (c (hi8 opc)) (v v1) (i i2) (j i3) (m m4) (r rxb) (d (lo8 opc))))

(defun zformat-vri-c (opc rxb v1 i2 v3 i3 m4)
  (masque "CCCCCCCC.VVVVWWWW.IIIIIIII.IIIIIIII.MMMMRRRR.DDDDDDDD"
          (c (hi8 opc)) (v v1) (w v3) (i i2) (m m4) (r rxb) (d (lo8 opc))))

(defun zformat-vri-d (opc rxb v1 v2 v3 i4 m5)
  (masque "CCCCCCCC.VVVVWWWW.XXXX0000.IIIIIIII.MMMMRRRR.DDDDDDDD"
          (c (hi8 opc)) (v v1) (w v2) (x v3) (i i4) (m m5) (r rxb) (d (lo8 opc))))

(defun zformat-vri-e (opc rxb v1 v2 i3 m4 m5)
  (masque "CCCCCCCC.VVVVWWWW.IIIIIIII.IIIIMMMM.NNNNRRRR.DDDDDDDD"
          (c (hi8 opc)) (v v1) (w v2) (i i3) (m m5) (n m4) (r rxb) (d (lo8 opc))))

(defun zformat-vri-f (opc rxb v1 v2 v3 i4 m5)
  (masque "CCCCCCCC.VVVVWWWW.XXXX0000.MMMMIIII.IIIIRRRR.DDDDDDDD"
          (c (hi8 opc)) (v v1) (w v2) (x v3) (m m5) (i i4) (r rxb) (d (lo8 opc))))

(defun zformat-vri-g (opc rxb v1 v2 i3 i4 m5)
  (masque "CCCCCCCC.VVVVWWWW.IIIIIIII.MMMMJJJJ.JJJJRRRR.DDDDDDDD"
          (c (hi8 opc)) (v v1) (w v2) (i i4) (m m5) (j i3) (r rxb) (d (lo8 opc))))

(defun zformat-vri-h (opc rxb v1 v2 i3 i4 m5)
  (masque "CCCCCCCC.VVVV0000.IIIIIIII.IIIIIIII.JJJJRRRR.DDDDDDDD"
          (c opc) (v v1) (i i2) (j i3) (r rxb) (d (lo8 opc))))

(defun zformat-vri-i (opc rxb v1 r2 i3 m4)
  (masque "CCCCCCCC.VVVVRRRR.00000000.MMMMIIII.IIIIRRRR.DDDDDDDD"
          (c (hi8 opc)) (v v1) (r r2) (m m4) (i i3) (r rxb) (d (lo8 opc))))

(defun zformat-vrr-a (opc rxb v1 v2 m3 m4 m5)
  (masque "CCCCCCCC.VVVVWWWW.00000000.MMMMNNNN.PPPPRRRR.DDDDDDDD"
          (c (hi8 opc)) (v v1) (w v2) (m m5) (n m4) (p m3) (r rxb) (d (lo8 opc))))

(defun zformat-vrr-b (opc rxb v1 v2 v3 m4 m5)
  (masque "CCCCCCCC.VVVVWWWW.XXXX0000.MMMM0000.NNNNRRRR.DDDDDDDD"
          (c (hi8 opc)) (v v1) (w v2) (x v3) (m m5) (n m4) (r rxb) (d (lo8 opc))))

(defun zformat-vrr-c (opc rxb v1 v2 v3 m4 m5 m6)
  (masque "CCCCCCCC.VVVVWWWW.XXXX0000.MMMMNNNN.PPPPRRRR.DDDDDDDD"
          (c (hi8 opc)) (v v1) (w v2) (x v3) (m m6) (n m5) (p m4) (r rxb) (d (lo8 opc))))

(defun zformat-vrr-d (opc rxb v1 v2 v3 m4 m5 m6)
  (masque "CCCCCCCC.VVVVWWWW.XXXXMMMM.NNNN0000.VVVVRRRR.DDDDDDDD"
          (c (hi8 opc)) (v v1) (w v2) (x v3) (m m5) (n m6) (v v4) (r rxb) (d (lo8 opc))))

(defun zformat-vrr-e (opc rxb v1 v2 v3 v4 m5 m6)
  (masque "CCCCCCCC.VVVVWWWW.XXXXMMMM.0000NNNN.VVVVRRRR.DDDDDDDD"
          (c (hi8 opc)) (v v1) (w v2) (x v3) (m m6) (n m5) (v v4) (r rxb) (d (lo8 opc))))

(defun zformat-vrr-f (opc rxb v1 r2 r3)
  (masque "CCCCCCCC.VVVVRRRR.SSSS0000.00000000.0000RRRR.DDDDDDDD"
          (c (hi8 opc)) (v v1) (r r2) (s r3) (r rxb) (d (lo8 opc))))

(defun zformat-vrr-g (opc rxb v1)
  (masque "CCCCCCCC.0000VVVV.00000000.00000000.0000RRRR.DDDDDDDD"
          (c (hi8 opc)) (v v1) (r rxb) (d (lo8 opc))))

(defun zformat-vrr-h (opc rxb v1 v2 m3)
  (masque "CCCCCCCC.0000VVVV.WWWW0000.MMMM0000.0000RRRR.DDDDDDDD"
          (c (hi8 opc)) (v v1) (w v2) (m m3) (r rxb) (d (lo8 opc))))

(defun zformat-vrr-i (opc rxb r1 v2 m3 m4)
  (masque "CCCCCCCC.RRRRVVVV.00000000.MMMMNNNN.0000RRRR.DDDDDDDD"
          (c (hi8 opc)) (r r1) (v v2) (m m3) (n m4) (r rxb) (d (lo8 opc))))

(defun zformat-vrs-a (opc rxb v1 b2 d2 v3 m4)
  (masque "CCCCCCCC.VVVVWWWW.BBBBDDDD.DDDDDDDD.MMMMRRRR.DDDDDDDD"
          (c (hi8 opc)) (v v1) (w v3) (b b2) (d d2) (m m4) (r rxb) (d (lo8 opc))))

(defun zformat-vrs-b (opc rxb v1 b2 d2 r3 m4)
  (masque "CCCCCCCC.VVVVRRRR.BBBBDDDD.DDDDDDDD.MMMMXXXX.DDDDDDDD"
          (c (hi8 opc)) (v v1) (r r3) (b b2) (d d2) (m m4) (x rxb) (d (lo8 opc))))

(defun zformat-vrs-c (opc rxb r1 b2 d2 v3 m4)
  (masque "CCCCCCCC.RRRRVVVV.BBBBDDDD.DDDDDDDD.MMMMXXXX.DDDDDDDD"
          (c (hi8 opc)) (r r1) (v v3) (b b2) (d d2) (m m4) (x rxb) (d (lo8 opc))))

(defun zformat-vrs-d (opc rxb v1 b2 d2 r3)
  (masque "CCCCCCCC.0000RRRR.BBBBDDDD.DDDDDDDD.VVVVXXXX.DDDDDDDD"
          (c (hi8 opc)) (r r3) (b b2) (d d2) (v v1) (x rxb) (d (lo8 opc))))

(defun zformat-vrv (opc rxb v1 b2 d2 v2 m3)
  (masque "CCCCCCCC.VVVVWWWW.BBBBDDDD.DDDDDDDD.MMMMXXXX.DDDDDDDD"
          (c (hi8 opc)) (v v1) (w v2) (b b2) (d d2) (m m3) (x rxb) (d (lo8 opc))))

(defun zformat-vrx (opc rxb v1 b2 d2 x2 m3)
  (masque "CCCCCCCC.VVVVWWWW.BBBBDDDD.DDDDDDDD.MMMMXXXX.DDDDDDDD"
          (c (hi8 opc)) (v v1) (w x2) (b b2) (d d2) (m m3) (x rxb) (d (lo8 opc))))

(defun zformat-vsi (opc rxb v1 b2 d2 i3)
  (masque "CCCCCCCC.IIIIIIII.BBBBDDDD.DDDDDDDD.VVVVXXXX.DDDDDDDD"
          (c (hi8 opc)) (i i3) (b b2) (d d2) (v v1) (x rxb) (d (lo8 opc))))
|#

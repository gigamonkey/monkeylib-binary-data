(defpackage :binary-io/test
  (:use :common-lisp :binary-io :binary-io.common-datatypes :1am))

(in-package :binary-io/test)

(defun prepare-file (name)
  (with-open-file (fd name :element-type '(unsigned-byte 8)
                             :direction :output
                             :if-exists :supersede
                             :if-does-not-exist :create)
    (write-value :u16 fd #xdead)
    (write-value :vector fd (vector 1078530011 1078530011) :type :u32 :size 2)
    (write-value :u64 fd 4614256656552045848)))

(test endianness
  (let ((name (format nil "/tmp/~x" (random #xffffff))))
    (prepare-file name)
    (with-open-file (fd name :element-type '(unsigned-byte 8))
      (is (= (read-value :u16 fd) #xdead))
      (file-position fd 0)
      (let ((*endianness* :big))
        (is (= (read-value :u16 fd) #xadde)))
      (let ((pi32 (float pi 1f0)))
        (is (equalp (read-value :vector fd :type :float32 :size 2)
                    (vector pi32 pi32))))
      (is (= (read-value :float64 fd) pi))
      (file-position fd 2)
      (let ((*endianness* :big))
        (is (= (read-value :u32 fd) #xdb0f4940))))
    (delete-file name)))

(defun prepare-two (name)
  (with-open-file (fd name :element-type '(unsigned-byte 8)
                           :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create)
    (write-value :u32 fd #xfffefdfc)
    (write-value :vector fd (vector 65437 65437 65437) :type :u16 :size 3)))

(test two-complement
  (let ((name (format nil "/tmp/~x" (random #xffffff))))
    (prepare-two name)
    (with-open-file (fd name :element-type '(unsigned-byte 8))
      (is (= (read-value :s8 fd) -4))
      (is (= (read-value :s8 fd) -3))
      (is (= (read-value :s8 fd) -2))
      (is (= (read-value :s8 fd) -1))
      (is (equalp (read-value :vector fd :type :s16 :size 3)
                  (vector -99 -99 -99))))
    (delete-file name)))

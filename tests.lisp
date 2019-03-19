(defpackage :binary-io/test
  (:use :common-lisp :binary-io :binary-io.common-datatypes :1am))

(in-package :binary-io/test)

(test endianness
  (let ((name (format nil "/tmp/~x" (random #xffffff))))
    (with-open-file (fd name :element-type '(unsigned-byte 8)
                             :direction :output
                             :if-exists :supersede
                             :if-does-not-exist :create)
      (write-value 'u2 fd #xdead))
    (with-open-file (fd name :element-type '(unsigned-byte 8))
      (is (= (read-value 'u2 fd) #xdead))
      (file-position fd 0)
      (let ((*endianness* :big))
        (is (= (read-value 'u2 fd) #xadde))))))

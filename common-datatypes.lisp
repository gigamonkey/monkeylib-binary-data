;;
;; Copyright (c) 2005, Gigamonkeys Consulting All rights reserved.
;;

(in-package :com.gigamonkeys.binary-data.common-datatypes)

(defun bits-per-byte-of-stream (stream)
  (car (last (stream-element-type stream))))

;;; Little-endian unsigned-integers
(define-binary-type unsigned-integer (bits)
  (:reader (fd)
	   (loop with bits-per-byte = (bits-per-byte-of-stream fd)
		 with value = 0
		 for bit from 0 below bits by bits-per-byte
		 do (setf (ldb (byte bits-per-byte bit) value) (read-byte fd))
		 finally (return value)))
  (:writer (fd value)
	   (loop with bits-per-byte = (bits-per-byte-of-stream fd)
		 for bit from 0 below bits by bits-per-byte
		 do (write-byte (ldb (byte bits-per-byte bit) value) fd)))
  (:size () (ceiling bits 8)))

(define-binary-type u1 () (unsigned-integer :bits 8))
(define-binary-type u2 () (unsigned-integer :bits 16))
(define-binary-type u4 () (unsigned-integer :bits 32))
(define-binary-type u8 () (unsigned-integer :bits 64))

;;; Signed on top of unsigned
(defmacro build-signed (signed-type unsigned-type bits)
  (alexandria:with-gensyms (from-2s-complement to-2s-complement)
    (let ((mask (ash 1 (1- bits)))
	  (maximum (ash 1 bits)))
      `(flet ((,from-2s-complement (x)
		(declare (type (unsigned-byte ,bits) x))
		(+ (- (logand x ,mask)) (logand x (lognot ,mask))))
	      (,to-2s-complement (x)
		(declare (type (signed-byte ,bits) x))
		(if (plusp x) x (+ x ,maximum))))
	 (define-binary-type ,signed-type ()
	   (:reader (fd) (,from-2s-complement (read-value ',unsigned-type fd)))
	   (:writer (fd value) (write-value ',unsigned-type fd (,to-2s-complement value)))
	   (:size () (type-size ',unsigned-type)))))))

(build-signed s1 u1 8)
(build-signed s2 u2 16)
(build-signed s4 u4 32)
(build-signed s8 u8 64)

;;; IEEE floats on top of unsigned
(define-binary-type float4 ()
  (:reader (in) (ieee-floats:decode-float32 (read-value 'u4 in)))
  (:writer (out value) (write-value 'u4 out (ieee-floats:encode-float32 value)))
  (:size () (type-size 'u4)))

(define-binary-type float8 ()
  (:reader (in) (ieee-floats:decode-float64 (read-value 'u8 in)))
  (:writer (out value) (write-value 'u8 out (ieee-floats:encode-float64 value)))
  (:size () (type-size 'u8)))

;;; Vectors
(define-binary-type vector (size type)
  (:reader (in)
           (loop with arr = (make-array size)
                 for i below size
                 do (setf (aref arr i) (read-value type in))
                 finally (return arr)))
  (:writer (out value)
           (loop for e across value
                 do (write-value type out e)))
  (:size () (* size (type-size type))))

(define-binary-type ten-u8 () (vector :size 10 :type 'u8))

;;; Strings
(define-binary-type generic-string (length character-type)
  (:reader (in)
	   (let ((string (make-string length)))
	     (dotimes (i length)
	       (setf (char string i) (read-value character-type in)))
	     string))
  (:writer (out string)
	   (dotimes (i length)
	     (write-value character-type out (char string i))))
  (:size () (* length (type-size character-type))))

(define-binary-type generic-terminated-string (terminator character-type)
  (:reader (in)
	   (with-output-to-string (s)
	     (loop for char = (read-value character-type in)
		   until (char= char terminator) do (write-char char s))))
  (:writer (out string)
	   (loop for char across string
		 do (write-value character-type out char)
		 finally (write-value character-type out terminator))))

;;; ISO-8859-1 strings

(define-binary-type iso-8859-1-char ()
  (:reader (in)
	   (let ((code (read-byte in)))
	     (or (code-char code)
		 (error "Character code ~d not supported" code))))
  (:writer (out char)
	   (let ((code (char-code char)))
	     (if (<= 0 code #xff)
		 (write-byte code out)
		 (error "Illegal character for iso-8859-1 encoding: character: ~c with code: ~d" char code))))
  (:size () 1))

(define-binary-type iso-8859-1-string (length)
  (generic-string :length length :character-type 'iso-8859-1-char))

(define-binary-type iso-8859-1-terminated-string (terminator)
  (generic-terminated-string :terminator terminator :character-type 'iso-8859-1-char))

;;; UCS-2 (Unicode) strings (i.e. UTF-16 without surrogate pairs, phew.)

;;; Define a binary type for reading a UCS-2 character relative to a
;;; particular byte ordering as indicated by the BOM value.
;; v2.3 specifies that the BOM should be present. v2.2 is silent
;; though it is arguably inherent in the definition of UCS-2) Length
;; is in bytesty. On the write side, since we don't have any way of
;; knowing what BOM was used to read the string we just pick one.
;; This does mean roundtrip transparency could be broken.

(define-binary-type ucs-2-char (swap)
  (:reader (in)
	   (let ((code (read-value 'u2 in)))
	     (when swap (setf code (swap-bytes code)))
	     (or (code-char code) (error "Character code ~d not supported" code))))
  (:writer (out char)
	   (let ((code (char-code char)))
	     (unless (<= 0 code #xffff)
	       (error "Illegal character for ucs-2 encoding: ~c with char-code: ~d" char code))
	     (when swap (setf code (swap-bytes code)))
	     (write-value 'u2 out code)))
  (:size () 2))

(defun swap-bytes (code)
  (assert (<= code #xffff))
  (rotatef (ldb (byte 8 0) code) (ldb (byte 8 8) code))
  code)

(define-binary-type ucs-2-char-big-endian () (ucs-2-char :swap nil))

(define-binary-type ucs-2-char-little-endian () (ucs-2-char :swap t))

(defun ucs-2-char-type (byte-order-mark)
  (ecase byte-order-mark
    (#xfeff 'ucs-2-char-big-endian)
    (#xfffe 'ucs-2-char-little-endian)))

(define-binary-type ucs-2-string (length)
  (:reader (in)
	   (let ((byte-order-mark (read-value 'u2 in))
		 (characters (1- (/ length 2))))
	     (read-value
	      'generic-string in
	      :length characters
	      :character-type (ucs-2-char-type byte-order-mark))))
  (:writer (out string)
	   (write-value 'u2 out #xfeff)
	   (write-value
	    'generic-string out string
	    :length (length string)
	    :character-type (ucs-2-char-type #xfeff)))
  (:size () (type-size 'generic-string
		       :length length
		       :character-type 'ucs-2-char)))

(define-binary-type ucs-2-terminated-string (terminator)
  (:reader (in)
	   (let ((byte-order-mark (read-value 'u2 in)))
	     (read-value
	      'generic-terminated-string in
	      :terminator terminator
	      :character-type (ucs-2-char-type byte-order-mark))))
  (:writer (out string)
	   (write-value 'u2 out #xfeff)
	   (write-value
	    'generic-terminated-string out string
	    :terminator terminator
	    :character-type (ucs-2-char-type #xfeff)))
  (:size () (type-size 'generic-terminated-string
		       :terminator terminator
		       :character-type 'ucs-2-char)))

;;; Fix length with terminator ASCII strings. This code should work
;;; for 8bit character be it ASCII, ISO 8859 or UTF-8 sans extensions.
(define-binary-type 8bit-string (length terminator)
  (:reader (in)
           (let ((string (make-string length)))
             (dotimes (i length)
               (setf (char string i) (code-char (read-byte in))))
             (subseq string 0 (position terminator string :test #'char=))))
  (:writer (out string)
           (let* ((outstring (make-string length :initial-element terminator)))
             (loop for char across string
                   for i from 0
                   do (setf (char outstring i) (char string i)))
             (loop for char across outstring
                   do (write-byte (char-code char) out))))
  (:size () length))

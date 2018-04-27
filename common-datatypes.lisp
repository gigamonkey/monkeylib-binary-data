;;
;; Copyright (c) 2005, Gigamonkeys Consulting All rights reserved.
;;

(in-package :com.gigamonkeys.binary-data.common-datatypes)

(defparameter *endianness* :little
  "Sets unsigned intergers read/write endianness. Should be one of
  :little or :big.")

(defun byte-indexes (bits endianness)
  (let ((byte-indexes (loop for i below bits by 8 collect i)))
    (unless (eq endianness :little)
      (setf byte-indexes (nreverse byte-indexes)))
    byte-indexes))

;;; Unsigned integers
(define-binary-type unsigned-integer (bits)
  (:reader (fd)
	   (assert (equal (stream-element-type fd) '(unsigned-byte 8)))
	   (let ((byte-indexes (byte-indexes bits *endianness*))
		 (value 0))
	     (dolist (i byte-indexes value)
	       (setf (ldb (byte 8 i) value) (read-byte fd)))))
  (:writer (fd value)
	   (assert (equal (stream-element-type fd) '(unsigned-byte 8)))
	   (let ((byte-indexes (byte-indexes bits *endianness*)))
	     (dolist (i byte-indexes)
	       (write-byte (ldb (byte 8 i) value) fd))))
  (:size () (ceiling bits 8)))

(define-binary-type u1 () (unsigned-integer :bits 8))
(define-binary-type u2 () (unsigned-integer :bits 16))
(define-binary-type u4 () (unsigned-integer :bits 32))
(define-binary-type u8 () (unsigned-integer :bits 64))

;;; Signed on top of unsigned
(defmacro build-signed (signed-type unsigned-type bits)
  (let ((marshall-name (intern (format nil "MARSHALL-~a" signed-type)))
	(unmarshall-name (intern (format nil "UNMARSHALL-~a" signed-type)))
	(mask (ash 1 (1- bits)))
	(maximum (ash 1 bits)))
    `(progn
       (defun ,unmarshall-name (x)
	 (declare (type (unsigned-byte ,bits) x))
	 (+ (- (logand x ,mask)) (logand x (lognot ,mask))))
       (defun ,marshall-name (x)
	 (declare (type (signed-byte ,bits) x))
	 (if (>= x 0) x (+ x ,maximum)))
       (define-binary-type ,signed-type ()
	 (:reader (fd) (,unmarshall-name (read-value ',unsigned-type fd)))
	 (:writer (fd value) (write-value ',unsigned-type fd (,marshall-name value)))
	 (:size () (type-size ',unsigned-type))))))

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
(defun marshaller (type)
  (case type
    (s1 #'marshall-s1)
    (s2 #'marshall-s2)
    (s4 #'marshall-s4)
    (s8 #'marshall-s8)
    (float4 #'ieee-floats:encode-float32)
    (float8 #'ieee-floats:encode-float64)))

(defun unmarshaller (type)
  (case type
    (s1 #'unmarshall-s1)
    (s2 #'unmarshall-s2)
    (s4 #'unmarshall-s4)
    (s8 #'unmarshall-s8)
    (float4 #'ieee-floats:decode-float32)
    (float8 #'ieee-floats:decode-float64)))

(defun pack (octets n)
  "Make a vector of n-octet integers out of an octets vector."
  (if (= n 1)
      octets
      (let* ((m (ceiling (length octets) n))
	     (result (make-array m))
	     (byte-indexes (byte-indexes (* n 8) *endianness*)))
	(loop for i below m
	      do (loop for bi in byte-indexes
		       for j from 0
		       do (setf (ldb (byte 8 bi) (aref result i)) (aref octets (+ (* i n) j)))))
	result)))

(defun unpack (vector n)
  "Inverse of pack."
  (if (= n 1)
      vector
      (let ((result (make-array (* n (length vector)) :element-type '(unsigned-byte 8)))
	    (byte-indexes (byte-indexes (* n 8) *endianness*)))
	(loop with i = 0
	      for e across vector
	      do (dolist (bi byte-indexes)
		   (setf (aref result i) (ldb (byte 8 bi) e))
		   (incf i)))
	result)))

(define-binary-type vector (size type)
  (:reader (in)
	   (assert (equal (stream-element-type in) '(unsigned-byte 8)))
	   (let* ((type-size (type-size type))
		  (octets (make-array (* size type-size) :element-type '(unsigned-byte 8)))
		  (unmarshaller (unmarshaller type)))
	     (read-sequence octets in)
	     (let ((arr (pack octets type-size)))
	       (when (functionp unmarshaller)
		 (dotimes (i size)
		   (setf (aref arr i) (funcall unmarshaller (aref arr i)))))
	       arr)))
  (:writer (out value)
	   (assert (equal (stream-element-type out) '(unsigned-byte 8)))
	   (let ((type-size (type-size type))
		 (marshaller (marshaller type))
		 (size (length value)))
	     (when (functionp marshaller)
	       (dotimes (i size)
		 (setf (aref value i) (funcall marshaller (aref value i)))))
	     (write-sequence (unpack value type-size) out)))
  (:size () (* size (type-size type))))

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

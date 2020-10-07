;;
;; Copyright (c) 2005, Gigamonkeys Consulting All rights reserved.
;;

(in-package :binary-io.common-datatypes)

(defparameter *endianness*
  #+little-endian :little
  #+big-endian :big
  #-(or little-endian big-endian) :little
  "Sets unsigned intergers read/write endianness. Should be one of
  :little or :big.")

(defun byte-indexes (bits endianness &optional (byte-size 8))
  (let ((byte-indexes (loop for i below bits by byte-size collect i)))
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

(define-binary-type :u8 () (unsigned-integer :bits 8))
(define-binary-type :u16 () (unsigned-integer :bits 16))
(define-binary-type :u32 () (unsigned-integer :bits 32))
(define-binary-type :u64 () (unsigned-integer :bits 64))

;;; Signed on top of unsigned
(defmacro build-signed (signed-type unsigned-type bits)
  (let ((marshall-name (intern (format nil "MARSHALL-~a" signed-type)))
        (unmarshall-name (intern (format nil "UNMARSHALL-~a" signed-type)))
        (maximum (ash 1 bits)))
    `(progn
       (defun ,unmarshall-name (x)
         (declare (type (unsigned-byte ,bits) x))
         (logior x (- (mask-field (byte 1 ,(1- bits)) x))))
       (defun ,marshall-name (x)
         (declare (type (signed-byte ,bits) x))
         (if (minusp x) (+ x ,maximum) x))
       (define-binary-type ,signed-type ()
         (:reader (fd) (,unmarshall-name (read-value ',unsigned-type fd)))
         (:writer (fd value) (write-value ',unsigned-type fd (,marshall-name value)))
         (:size () (type-size ',unsigned-type))))))

(build-signed :s8 :u8 8)
(build-signed :s16 :u16 16)
(build-signed :s32 :u32 32)
(build-signed :s64 :u64 64)

(defun marshaller (type)
  "Return the marshalling function for TYPE. Can return NIL if nothing
has to be done (this should be checked by the caller)."
  (case type
    (:s8 #'marshall-s8)
    (:s16 #'marshall-s16)
    (:s32 #'marshall-s32)
    (:s64 #'marshall-s64)
    (:float32 #'(lambda (x) (ieee-floats:encode-float32 (float x 0f0))))
    (:float64 #'(lambda (x) (ieee-floats:encode-float64 (float x 0d0))))))

(defun unmarshaller (type)
  "Return the unmarshalling function for TYPE. Can return NIL if
nothing has to be done (this should be checked by the caller)."
  (case type
    (:s8 #'unmarshall-s8)
    (:s16 #'unmarshall-s16)
    (:s32 #'unmarshall-s32)
    (:s64 #'unmarshall-s64)
    (:float32 #'ieee-floats:decode-float32)
    (:float64 #'ieee-floats:decode-float64)))

;;; IEEE floats on top of unsigned
(define-binary-type :float32 ()
  (:reader (in) (funcall (unmarshaller :float32) (read-value :u32 in)))
  (:writer (out value) (write-value :u32 out (funcall (marshaller :float32) value)))
  (:size () (type-size :u32)))

(define-binary-type :float64 ()
  (:reader (in) (funcall (unmarshaller :float64) (read-value :u64 in)))
  (:writer (out value) (write-value :u64 out (funcall (marshaller :float64) value)))
  (:size () (type-size :u64)))

;;; Vectors
(defun pack (bytes n &optional (byte-size 8))
  "Make a vector of N-byte integers out of a BYTES vector. Defaults to
octet as byte."
  (if (= n 1)
      bytes
      (let* ((m (ceiling (length bytes) n))
             (result (make-array m))
             (byte-indexes (byte-indexes (* n byte-size) *endianness* byte-size)))
        (loop for i below m
              do (loop for bi in byte-indexes
                       for j from 0
                       do (setf (ldb (byte byte-size bi) (aref result i))
                                  (aref bytes (+ (* i n) j)))))
        result)))

(defun unpack (vector n &optional (byte-size 8))
  "Opposite of PACK."
  (if (= n 1)
      vector
      (let* ((result (make-array (* n (length vector))
                                 :element-type `(unsigned-byte ,byte-size)))
             (byte-indexes (byte-indexes (* n byte-size) *endianness* byte-size)))
        (loop with i = 0
              for e across vector
              do (dolist (bi byte-indexes)
                   (setf (aref result i) (ldb (byte byte-size bi) e))
                   (incf i)))
        result)))

(define-binary-type :vector (size type)
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
                 (size (length value))
                 (arr (alexandria:copy-array value)))
             (when (functionp marshaller)
               (dotimes (i size)
                 (setf (aref arr i) (funcall marshaller (aref arr i)))))
             (write-sequence (unpack arr type-size) out)))
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
           (let ((code (read-value :u16 in)))
             (when swap (setf code (swap-bytes code)))
             (or (code-char code) (error "Character code ~d not supported" code))))
  (:writer (out char)
           (let ((code (char-code char)))
             (unless (<= 0 code #xffff)
               (error "Illegal character for ucs-2 encoding: ~c with char-code: ~d" char code))
             (when swap (setf code (swap-bytes code)))
             (write-value :u16 out code)))
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
           (let ((byte-order-mark (read-value :u16 in))
                 (characters (1- (/ length 2))))
             (read-value
              'generic-string in
              :length characters
              :character-type (ucs-2-char-type byte-order-mark))))
  (:writer (out string)
           (write-value :u16 out #xfeff)
           (write-value
            'generic-string out string
            :length (length string)
            :character-type (ucs-2-char-type #xfeff)))
  (:size () (type-size 'generic-string
                       :length length
                       :character-type 'ucs-2-char)))

(define-binary-type ucs-2-terminated-string (terminator)
  (:reader (in)
           (let ((byte-order-mark (read-value :u16 in)))
             (read-value
              'generic-terminated-string in
              :terminator terminator
              :character-type (ucs-2-char-type byte-order-mark))))
  (:writer (out string)
           (write-value :u16 out #xfeff)
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

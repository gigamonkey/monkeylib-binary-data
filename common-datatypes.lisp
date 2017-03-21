;;
;; Copyright (c) 2005, Gigamonkeys Consulting All rights reserved.
;;

(in-package :com.gigamonkeys.binary-data.common-datatypes)

;; Little-endian octet integers
(define-binary-type integer (bytes sign)
  (:reader (in)
           (loop with unsigned-value = 0
                 with bits-per-byte = 8
                 for low-bit from 0 to (* bits-per-byte (1- bytes)) by bits-per-byte
                 do (setf (ldb (byte bits-per-byte low-bit) unsigned-value) (read-byte in))
                 finally (let ((bits (* bits-per-byte bytes)))
                           (if (and sign (>= unsigned-value (ash 1 (1- bits))))
                               (return (- unsigned-value (ash 1 bits)))
                               (return unsigned-value)))))
  (:writer (out value)
           (loop with bits-per-byte = 8
                 with unsigned-value = (if (plusp value)
                                           value
                                           (- (ash 1 (* bits-per-byte bytes)) value))
                 for low-bit from 0 to (* bits-per-byte (1- bytes)) by bits-per-byte
                 do (write-byte (ldb (byte bits-per-byte low-bit) unsigned-value) out)))
  (:size () bytes))

(define-binary-type u1 () (integer :bytes 1))
(define-binary-type u2 () (integer :bytes 2))
(define-binary-type u4 () (integer :bytes 4))
(define-binary-type u8 () (integer :bytes 8))

(define-binary-type s1 () (integer :bytes 1 :sign t))
(define-binary-type s2 () (integer :bytes 2 :sign t))
(define-binary-type s4 () (integer :bytes 4 :sign t))
(define-binary-type s8 () (integer :bytes 8 :sign t))

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
  (:size () (* length (object-size character-type))))

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
  (:size () (object-size 'generic-string
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
  (:size () (object-size 'generic-terminated-string
			 :terminator terminator
			 :character-type 'ucs-2-char)))

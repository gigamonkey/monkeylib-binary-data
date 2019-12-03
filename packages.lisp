;;
;; Copyright (c) 2005, Gigamonkeys Consulting All rights reserved.
;;

(defpackage :binary-io
  (:use :common-lisp :alexandria)
  (:export :define-binary-class
           :define-tagged-binary-class
           :define-binary-type
           :define-enumeration
           :define-bitfield
           :read-value
           :write-value
           :type-size
           #+sbcl :open-mmap
           #+sbcl :mmap-stream-fd
           :*in-progress-objects*
           :parent-of-type
           :immediate-parent
           :current-binary-object
           :+null+))

(defpackage :binary-io.common-datatypes
  (:use :common-lisp :binary-io)
  (:export
   :*endianness*
   :integer
   :marshaller
   :unmarshaller
   :generic-string
   :generic-terminated-string
   :iso-8859-1-char
   :iso-8859-1-string
   :iso-8859-1-terminated-string
   :ucs-2-char
   :ucs-2-char-big-endian
   :ucs-2-char-little-endian
   :ucs-2-char-type
   :ucs-2-string
   :ucs-2-terminated-string
   :8bit-string))
